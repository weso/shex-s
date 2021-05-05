---
id: shapemaps
title: ShapeMaps
---

# Shape Maps

The ShapeMaps module contains an implementation of Shape Maps, which are defined [here](http://shex.io/shape-map/). 

## Creation of a ShapeMap

A `QueryShapeMap` can be introduced as input.

```scala mdoc
import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
import es.weso.shapemaps._

val pm = PrefixMap.empty.addPrefix("",IRI("http://example.org/"))
val nodesPrefixMap = pm
val shapesPrefixMap = pm
val qm = ShapeMap.fromString(":x@:S", "Compact", None, nodesPrefixMap, shapesPrefixMap)
```

