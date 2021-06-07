---
id: rbe
title: RBE - Regular Bag Expressions
---

# Bags

Bags are sets which allow repeated elements. 


```scala mdoc
import es.weso.collection._

val bag = Bag("a","b","a")
```


# RBE - Regular Bag Expressions

Regular Bag Expressions (RBE) are regular expressions where the order of the elements is not significant.


## Example usage

The following line declares `rbe` as a regular bag expressions that matches between 1 or 3 `a`s and 1 `b`: 

```scala mdoc
import es.weso.rbe._

val rbe = And(Symbol("a",1,3), Symbol("b",1,1))
```

There are 2 algorithms to check if a bag matches a rbe. 

The interval algorithm is in PTime but only works if the rbe doesn't have repeated symbols. 

Example:

```scala mdoc
import es.weso.rbe.interval._

val checker = IntervalChecker(rbe)

val check = checker.check(bag, false)
```



```

```