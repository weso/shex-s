# ShEx-s

Scala implementation of SHEX.

This project contains an implementation of [ShEx](http://www.shex.io).

[![Build Status](https://travis-ci.org/weso/shex-s.svg?branch=master)](https://travis-ci.org/weso/shex-s)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/d421668975834528bf562ca81bff4433)](https://www.codacy.com/gh/weso/shex-s?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=weso/shex-s&amp;utm_campaign=Badge_Grade)
[![codecov](https://codecov.io/gh/weso/shex-s/branch/master/graph/badge.svg)](https://codecov.io/gh/weso/shex-s)

## Introduction

This project contains a Scala implementation of [ShEx](http://shex.io/). The library handles RDF using a 
[simple RDF library](https://github.com/weso/srdf), which has 2 implementations,
one using [Apache Jena](https://jena.apache.org/)
and another one using [RDF4j](http://rdf4j.org/),
this means that it is possible to use this library to validate RDF models from any of those RDF libraries,
as well as from external SPARQL endpoints.

## Installation and compilation

The project uses [sbt](http://www.scala-sbt.org/) for compilation as well as Java 1.8.

* `sbt test` compiles and runs the tests

## Implementation details

* The engine is based on Monads using the [cats library](http://typelevel.org/cats/)
* The ShEx compact syntax parser
  is implemented using the following [Antlr grammar](https://github.com/shexSpec/grammar/blob/master/ShExDoc.g4) (previous versions used Scala Parser Combinators)
  which is based on this [grammar](https://github.com/shexSpec/shex.js/blob/master/doc/bnf)
* JSON encoding and decoding uses the Json structure [defined here](https://shexspec.github.io/spec/) and is implemented using [Circe](https://github.com/travisbrown/circe)

## Compatibility tests

We also aim to pass the [ShEx test-suite](https://github.com/shexSpec/shexTest).

In order to run the shex test-suite and generate the EARL report, you can do the following:

```
sbt
...
sbt:shaclex> project shex
sbt:shex> compat:test
```

## More information

* An online demo based on this library is available at [http://rdfshape.weso.es](http://rdfshape.weso.es).
* Another online demo based on this library customized for Wikidata is available at [http://wikishape.weso.es](http://wikishape.weso.es).
* This project was based on [ShExcala](http://labra.github.io/ShExcala/) which was focused on Shape Expressions only.

## Author & contributors

* Author: [Jose Emilio Labra Gayo](http://labra.weso.es)

Contributors:

* [Eric Prud'hommeaux](https://www.w3.org/People/Eric/)
* [Bogdan Roman](https://github.com/bogdanromanx)
* [Toni Cebrían](http://www.tonicebrian.com/)
* [Andrew Berezovskyi](https://github.com/berezovskyi)

## Adopters

* [RDFShape](http://rdfshape.weso.es): An online demo powered by this library.
* [Wikishape](http://wikishape.weso.es): An online demo powered by this library for Wikidata.
* [Eclipse lyo](http://www.eclipse.org/lyo/): An SDK and a modelling environment to design and develop linked data applications based on the [OSLC standards](http://open-services.net/). The validation library is [lyo-validation](https://github.com/eclipse/lyo-validation).

## Contribution

Contributions are greatly appreciated.
Please fork this repository and open a
pull request to add more features or [submit issues](https://github.com/labra/shaclex/issues)
