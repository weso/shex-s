# WShEx-s

WShEx-s is an implementation of [WShEx](https://www.weso.es/WShEx/), 
 a language inspired by [ShEx](http://shex.io/) that can be used to describe and validate Wikidata and Wikibase entities.

While [ShEx](http://shex.io/) has been designed as a language to describe and validate RDF, WShEx has been designed to describe and validate Wikibase entities.

# Some examples of WShEx

## Basic example

The following Shape describes a `<Human>` as an entity with property 
`:P31` (_instance of_) with the value `:Q5` (_Human_). 

Since in WShEx we use concept URIs, it is not necessary to use `wdt:` or `p:` aliases from Wikidata's RDF serialization. 

```turtle
PREFIX : <http://www.wikidata.org/entity/>

<Human> {
  :P31 [ :Q5 ]  
}
```

## Using qualifiers

`<EnglishSite>` have property `:P856` (_official website_) with any value but with the qualifier `:P407` (_language of work or name_) with the value `:Q1860` (_English_)
 
```
PREFIX : <http://www.wikidata.org/entity/>

<EnglishSite> {
  :P856 .  {| :P407 [ :Q1860 ] |}
}
```

# Entity Schemas to WShEx

## Example with a direct property

The following example in WShEx 

```turtle
PREFIX : <http://www.wikidata.org/entity/>

<Human> {
  :P31 [ :Q5 ]  
}
```

can be represented in Entity Schemas as 

```turtle
PREFIX wd:  <http://www.wikidata.org/entity/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>

<Human> {
  wdt:P31 [ wd:Q5 ]  
}
```
which is also equivalent to 

```turtle
PREFIX ps: <http://www.wikidata.org/prop/statement/>
PREFIX p: <http://www.wikidata.org/prop/>
PREFIX wdt: <http://www.wikidata.org/prop/direct/>
PREFIX wd: <http://www.wikidata.org/entity/>

<Human> {
  p:P31 { ps:P31 [ wd:Q5 ]  }
}
```

## Example with a qualifier

The following example in WShEx:

```turtle
<EnglishSite> {
  :P856 .  {| :P407 [ :Q1860 ] |}
}
```

can be represented in Entity Schemas as:

```turtle
<EnglishSite> {
  p:P856 {
    ps:P856 . 
    pq:P407 [ :Q1860 ] 
  }
}
```