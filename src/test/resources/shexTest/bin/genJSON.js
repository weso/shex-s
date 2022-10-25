#!/usr/bin/env node

/* genJSON - tool to generate JSON-LD version of manifest.ttl
 * install and run:

git clone git@github.com:shexSpec/shexTest.git
npm install
cd validation/
../bin/genJSON.js manifest.ttl -w

*/

// Parse arguments
var args = process.argv.slice(2);
if (args > 1 || args.indexOf("-help") !== -1 || args.indexOf("--help") !== -1) {
  console.error('usage: genJSON manifest.ttl [-o outfile] [-w|-e] > manifest.jsonld');
  return process.exit(1);
}
var OUTFILE = null;
if (args[1] === "-o") {
  OUTFILE = args[2];
  args.splice(1, 2); // git rid of -o outfile
}
var errors = 0;
var WARN = args[1] === "-w" ? "warn" : args[1] === "-e" ? "err" : null;
function report (msg) {
  console.warn(msg);
  if (WARN === "err") {
    ++errors;
  }
}
function jsonLdId (t) {
  switch (t.termType) {
  case "NamedNode": return t.value;
  case "BlankNode": return "_:" + t.value;
  default: throw Error(`unknown termType in ${JSON.stringify(t)}`);
  }
}

var fs = require('fs');
var Path = require("path");
var N3 = require("n3");
var parser = new N3.Parser({blankNodePrefix: ""});
var util = N3.Util;
var store = new N3.Store();
//var json = fs.readFileSync(args[0]).toString();

var P = {
  "rdf":  "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
  "mf":   "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#",
  "sht":  "http://www.w3.org/ns/shacl/test-suite#",
  "sx":   "https://shexspec.github.io/shexTest/ns#"
};

var testDir = Path.basename(Path.dirname(Path.resolve(args[0])));
var basePath = "https://raw.githubusercontent.com/shexSpec/shexTest/master/";
var dirPath = basePath + testDir + '/';
function RelPath (p) {
  return Path.relative(dirPath, p);
}
var apparentBase = dirPath + "manifest";

parser.parse(
  "@base <" + apparentBase + "> .\n"+
  fs.readFileSync(args[0], "utf8"),
  function (error, quad, prefixes) {
    if (error) {
      error.message = "Error parsing " + args[0] + ": " + error.message;
      throw error;
    }
    if (quad)
      store.addQuad(quad)
    else
      genText();
  });

/** expandCollection - N3.js utility to return an rdf collection's elements.
*/
function expandCollection (h) {
  if (store.getQuads(h.object, P.rdf + "first", null).length) {
    var ret = [];
    while (h.object.value !== "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil") {
      ret.push(store.getQuads(h.object, P.rdf + "first", null)[0].object);
      h = store.getQuads(h.object, P.rdf + "rest", null)[0];
    }
    return ret;
  } else {
    return h.object
  }
}

function genText () {
  var g = []; // stuff everything into a JSON-LD @graph
  var ret = {
    "@context": [
      {"@base": apparentBase},
      "../context.jsonld"
    ],
    "@graph": g
  };

  var manifest = store.getQuads(null, P.rdf + "type", P.mf + "Manifest")[0].subject;
  var manifestComment = store.getQuads(manifest, P.rdfs + "comment", null)[0].object.value;
  var entries = [];
  var knownMissing = {}; // record missing files.
  var head = store.getQuads(manifest, P.mf + "entries", null)[0].object;
  while (head.value !== P.rdf + "nil") {
    entries.push(store.getQuads(head, P.rdf + "first", null)[0].object.value);
    head = store.getQuads(head, P.rdf + "rest", null)[0].object;
  }
  var unmatched = entries.reduce(function (ret, ent) {
    ret[ent] = true;
    return ret;
  }, {});
  var expectedTypes = ["ValidationTest", "ValidationFailure", "RepresentationTest", "NegativeSyntax", "NegativeStructure"].map(function (suffix) {
    return P.sht + suffix;
  });

  g.push({
    "@id": "",
    "@type": "mf:Manifest",
    "rdfs:comment": manifestComment,
    "entries": store.getQuads(null, P.rdf + "type", null).filter(function (t) {
      var ret = expectedTypes.indexOf(t.object.value) !== -1;
      if (ret === false &&
          t.object.value !== P.mf + "Manifest") {
        report("test " + t.subject.value + " has unexpected type " + t.object.value);
      }
      return ret;
    }).map(function (t) {
      return [t.subject.value, t.object.value];
    }).filter(function (t) {
      var ret = entries.indexOf(t[0]) !== -1;
      if (ret === false) {
        report("unreferenced test: " + t[0]);
      } else {
        delete unmatched[t[0]];
      }
      return ret;
    }).sort(function (l, r) {
      return l[0] === r[0] ? 0 :
        entries.indexOf(l[0]) > entries.indexOf(r[0]) ? 1 :
        -1;
    }).map(function (st) {
      var s = st[0], t = st[1];
      var testName = store.getQuads(s, P.mf + "name", null)[0].object.value;
      var testType = store.getQuads(s, P.rdf + "type", null)[0].object.value.replace(P.sht, '');
      var expectedName = s.substr(apparentBase.length+1);
      if (WARN && testName !== expectedName) {
	report("expected label \"" + expectedName + "\" ; got \"" + testName + "\"");
      }
      var actionTriples = store.getQuads(s, P.mf + "action", null);
      function exists (filename) {
        var filepath = Path.join(__dirname, "../" + testDir + '/' + filename);
        if (WARN && !fs.existsSync(filepath) && !(filepath in knownMissing)) {
          report("non-existent file: " + RelPath(filepath) + " is missing " + Path.relative(process.cwd(), filepath));
	  knownMissing[filepath] = Path.relative(process.cwd(), filepath);
        }
        return filename;
      }
      if (actionTriples.length !== 1) {
        if (["RepresentationTest", "NegativeSyntax", "NegativeStructure"].indexOf(testType) === -1) {
          throw Error("expected 1 action for " + s + " -- got " + actionTriples.length);
        }
        // Representation/Syntax/Structure tests
        return [
          //      ["rdf"  , "type"    , function (v) { return v.substr(P.sht.length); }],
          [s, "mf"   , "name"    , function (v) { return v[0].value; }],
          [s, "sht", "trait"  , function (v) {
           return v.map(function (x) {
             return x.value.substr(P.sht.length);
           }).sort();
          }],
          //[s, "rdfs" , "comment" , function (v) { return (v[0].value; }],
          [s, "mf", "status"  , function (v) { return "mf:"+v[0].value.substr(P.mf.length); }],
          [s, "sx", "shex", function (v) { return exists(RelPath(v[0].value)); }],
          [s, "sx", "json", function (v) { return exists(RelPath(v[0].value)); }],
          [s, "sx", "ttl", function (v) { return exists(RelPath(v[0].value)); }],
          [s, "mf", "startRow"   , function (v) { return parseInt(v[0].value); }],
          [s, "mf", "startColumn", function (v) { return parseInt(v[0].value); }],
          [s, "mf", "endRow"     , function (v) { return parseInt(v[0].value); }],
          [s, "mf", "endColumn"  , function (v) { return parseInt(v[0].value); }],
        ].reduce(function (ret, row) {
          var found = store.getQuads(row[0], P[row[1]]+row[2], null).map(expandCollection);
          var target = ret;
          if (found.length)
            target[row[2]] = row[3](found);
          return ret;
        }, {"@id": s.substr(apparentBase.length), "@type": "sht:"+t.substr(P.sht.length)});
      }
      var a = actionTriples[0].object;
      return [
        //      ["rdf"  , "type"    , function (v) { return v.substr(P.sht.length); }],
        [s, "mf"   , "name"    , function (v) { return v[0].value; }],
        [s, "sht", "trait"  , function (v) {
          return v.map(function (x) {
            return x.value.substr(P.sht.length);;
          }).sort();
        }],
        [s, "rdfs" , "comment" , function (v) { return v[0].value; }],
        [s, "mf", "status"  , function (v) { return "mf:"+v[0].value.substr(P.mf.length); }],
        [a, "sht", "schema"  , function (v) { return exists("../" + v[0].value.substr(basePath.length)); } ], // could be more judicious in creating a relative path from an absolute path.
        [a, "sht", "shape"   , function (v) { return v[0].value.indexOf(dirPath) === 0 ? RelPath(v[0].value) : jsonLdId(v[0]); }],
        [a, "sht", "data"    , function (v) { return exists(RelPath(v[0].value)); }],
        [a, "sht", "map"    , function (v) { return exists(RelPath(v[0].value)); }],
        [a, "sht", "focus"   , function (v) {
          // Focus can be a literal
          if (util.isLiteral(v[0])) {
            var lang = v[0].language;
            var dt = v[0].datatype.value;
            var res = {'@value': v[0].value};
            if (lang.length > 0) {res['@language'] = lang}
            if (dt.length > 0) {res['@type'] = dt}
            return res;
          } else {
            return (v[0].value.indexOf(dirPath) === 0 ? RelPath(v[0].value) : jsonLdId(v[0]));
          }
        }],
        [a, "sht", "semActs" , function (v) { return exists("../" + v[0].value.substr(basePath.length)); }], // could be more judicious in creating a relative path from an absolute path.
        [a, "sht", "shapeExterns" , function (v) { return exists("../" + v[0].value.substr(basePath.length)); }], // could be more judicious in creating a relative path from an absolute path.
        [s, "mf", "result"  , function (v) { return exists(RelPath(v[0].value)); }],
        [s, "mf", "extensionResults"  , function (v) {
          return v[0].map(function (x) {
            return {
              extension: store.getQuads(x, P.mf + "extension", null)[0].object.value,
              prints: store.getQuads(x, P.mf + "prints", null)[0].object.value
            };
          });
        }]
      ].reduce(function (ret, row) {
        var found = store.getQuads(row[0], P[row[1]]+row[2], null).map(expandCollection);
        var target = row[0] === s ? ret : row[0] === a ? ret.action : ret.extensionResults;
        if (found.length)
          target[row[2]] = row[3](found);
        return ret;
      }, {"@id": s.substr(apparentBase.length), "@type": "sht:"+t.substr(P.sht.length), action: {}, extensionResults: []});
    })
  });
  var remaining = Object.keys(unmatched);
  if (remaining.length) {
    report("no definitions for " + remaining.join(", "));
  }
  if (!errors) {
    if (OUTFILE) {
      fs.writeFileSync(OUTFILE, JSON.stringify(ret, null, "  ") + "\n");
    } else {
      console.log(JSON.stringify(ret, null, "  ") + "\n");
    }
    process.exit(0);
  } else {
    process.exit(1);
  }
}
