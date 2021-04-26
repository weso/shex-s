---
id: depGraphs
title: Dependency graphs
---

# Dependency graphs

Dependency graphs is a module that can be used to analyze dependencies between graphs.
The graphs can have positive or negative edges.

In the following declaration `g1` is a graph with three nodes `a`, `b` and `c`, and a positive edge between `a` and `b` and a negative edge between `a` and `c`. 

```scala mdoc 
import es.weso.depgraphs._

val g1 = DepGraph.empty[String].
        addPosEdge("a", "b").
        addNegEdge("a", "c")
```

An important operation is to detect if there are negative cycles in a dependency graph. `g1` doesn't contain negative cycles.

```scala mdoc
g1.containsNegCycle
```

On the other hand, the following graph contains negative cycles.

```scala mdoc
val g = DepGraph.empty[String].
        addNegEdge("a", "b").
        addPosEdge("a", "c").
        addPosEdge("b", "d").
        addPosEdge("d", "a")

g.containsNegCycle
```

`negCycles` returns the cycles with negative dependencies.

```scala mdoc
g.negCycles
```