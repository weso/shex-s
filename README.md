# ShEx-s

Scala implementation of SHEX.

This project contains an implementation of [Shape Expressions (ShEx)](http://www.shex.io).

[![Continuous Integration](https://github.com/weso/shex-s/actions/workflows/ci.yml/badge.svg)](https://github.com/weso/shex-s/actions/workflows/ci.yml)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/d421668975834528bf562ca81bff4433)](https://www.codacy.com/gh/weso/shex-s?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=weso/shex-s&amp;utm_campaign=Badge_Grade)
[![codecov](https://codecov.io/gh/weso/shex-s/branch/master/graph/badge.svg)](https://codecov.io/gh/weso/shex-s)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/es.weso/shexs_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/es.weso/shexs_2.13)

## Introduction

This project contains a Scala implementation of [ShEx](http://shex.io/). 
The library handles RDF using a 
[simple RDF library](https://github.com/weso/srdf), which at this moment has 2 implementations,
one using [Apache Jena](https://jena.apache.org/)
and another one using [RDF4j](http://rdf4j.org/).
This means that it is possible to use this library to validate RDF models from either Jena or RDF4J models,
as well as from external SPARQL endpoints.


## Installation and compilation

The project uses [sbt](http://www.scala-sbt.org/) for compilation as well as Java 1.8.

* `sbt test` compiles and runs the tests

## Command line usage

It is possible to run `shex-s` as a command line tool. To create the executable run:

```
$ sbt universal:packageBin
```

which will create a zip file in folder: 

```
target/universal/shex-s-{version}.zip
```

If you uncompress that zip in a folder a add it to your path, you will be able to run shex-s as a command line tool. 

```
Usage:
    shex-s mapping
    shex-s validate
    shex-s shapePath
    shex-s manifest
    shex-s wikibase
    shex-s schema

ShEx-Scala command line tool

Options and flags:
    --help
        Display this help text.
    --version, -v
        Print the version number and exit.

Subcommands:
    mapping
        Convert a schema through a mapping
    validate
        Validate RDF data using a schema and a shape map
    shapePath
        Validate a shape path
    manifest
        Run manifest file containing tests
    wikibase
        Validate RDF data from wikibase
    schema
        Schema processing actions
```

### Validate option

You can run the following in the `examples` folder to validate the turtle file `user.ttl` according to the ShEx file `user.shex` and the shape map `user.sm`:

```
shexs validate --schema user.shex --data user.ttl --shapeMap user.sm
```

Other options for validation are available as:

```
Usage:
    shex-s validate --schema <path> [--schemaFormat <format>] [--baseIRI <string>] --data <path> [--dataFormat <string>] --shapeMap <path> [--shapeMapFormat <string>] [--validator version <string>] [--showResultFormat <string>] [--output <path>] [--verbose <string>]
    shex-s validate --schema <path> [--schemaFormat <format>] [--baseIRI <string>] --endpoint <string> --shapeMap <path> [--shapeMapFormat <string>] [--validator version <string>] [--showResultFormat <string>] [--output <path>] [--verbose <string>]
    shex-s validate --schemaURL <string> [--baseIRI <string>] --data <path> [--dataFormat <string>] --shapeMap <path> [--shapeMapFormat <string>] [--validator version <string>] [--showResultFormat <string>] [--output <path>] [--verbose <string>]
    shex-s validate --schemaURL <string> [--baseIRI <string>] --endpoint <string> --shapeMap <path> [--shapeMapFormat <string>] [--validator version <string>] [--showResultFormat <string>] [--output <path>] [--verbose <string>]

Validate RDF data using a schema and a shape map

Options and flags:
    --help
        Display this help text.
    --schema <path>, -s <path>
        Path to ShEx file.
    --schemaFormat <format>
        Schema format, default = (ShExC). Possible values = (ShExC,ShExJ)
    --baseIRI <string>
        base IRI
    --schemaURL <string>
        URL of schema
    --data <path>, -d <path>
        Path to data file.
    --dataFormat <string>
        Data format. Default=Turtle, available=Turtle,NTriples,RDF/XML,JSON-LD
    --endpoint <string>
        endpoint URL
    --shapeMap <path>, -s <path>, -m <path>
        Path to shapeMap file.
    --shapeMapFormat <string>
        ShapeMap format, default=Compact, available formats=List(Compact, JSON)
    --validator version <string>, -e <string>
        version of validator. Default = 2.2. Other values = 2.1,ref
    --showResultFormat <string>
        showResultFormat
    --output <path>
        Output to file (default = console)
    --verbose <string>, -v <string>
        verbose level (0-nothing,1-basic,2-info,3-details,4-debug,5-step,6-all)
```



## Implementation details

* The engine is based on Monads using the [cats library](http://typelevel.org/cats/)
* The ShEx compact syntax parser
  is implemented using the following [Antlr grammar](https://github.com/shexSpec/grammar/blob/master/ShExDoc.g4) (previous versions used Scala Parser Combinators)
  which is based on this [grammar](https://github.com/shexSpec/shex.js/blob/master/doc/bnf)
* JSON encoding and decoding uses the Json structure [defined here](https://shexspec.github.io/spec/) and is implemented using [Circe](https://github.com/travisbrown/circe)
* It supports ShEx + extends

## Compatibility tests

We also pass the [ShEx test-suite](https://github.com/shexSpec/shexTest).

<!-- In order to run the shex test-suite and generate the EARL report, you can do the following:

```
sbt
...
sbt:shexRoot> project shex
sbt:shex> test
```
-->

## Publishing to OSS-Sonatype

This project uses [the sbt ci release](https://github.com/olafurpg/sbt-ci-release) plugin for publishing to [OSS Sonatype](https://oss.sonatype.org/).

##### SNAPSHOT Releases
Open a PR and merge it to watch the CI release a -SNAPSHOT version

##### Full Library Releases
1. Push a tag and watch the CI do a regular release
2. `git tag -a v0.1.0 -m "v0.1.0"`
3. `git push origin v0.1.0`
_Note that the tag version MUST start with v._

## More information

* An online demo based on this library is available at [http://rdfshape.weso.es](http://rdfshape.weso.es).
* Another online demo based on this library customized for Wikidata is available at [http://wikishape.weso.es](http://wikishape.weso.es).
* This project was based on [ShExcala](http://labra.github.io/ShExcala/) which was focused on Shape Expressions only.

## Author & contributors

* Author: [Jose Emilio Labra Gayo](http://labra.weso.es)

Contributors:

* [Eric Prud'hommeaux](https://www.w3.org/People/Eric/)
* [Bogdan Roman](https://github.com/bogdanromanx)
* [Toni Cebrián](http://www.tonicebrian.com/)
* [Andrew Berezovskyi](https://github.com/berezovskyi)

## Adopters

* [RDFShape](http://rdfshape.weso.es): An online demo powered by this library.
* [Wikishape](http://wikishape.weso.es): An online demo powered by this library for Wikidata.
* [Eclipse lyo](http://www.eclipse.org/lyo/): An SDK and a modelling environment to design and develop linked data applications based on the [OSLC standards](http://open-services.net/). The validation library is [lyo-validation](https://github.com/eclipse/lyo-validation).

## Contribution

Contributions are greatly appreciated.
Please fork this repository and open a
pull request to add more features or [submit issues](https://github.com/labra/shaclex/issues)


<a href="https://github.com/weso/shex-s/graphs/contributors">
  <img src="https://contributors-img.web.app/image?repo=weso/shex-s" />
</a>
