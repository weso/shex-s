# WShEx

WShEx is a ShEx extension for Wikibase that takes into account qualifiers and references for validation.

While [ShEx](http://shex.io/) has been designed as a language to describe and validate RDF, WShEx has been designed to describe and validate Wikibase instances.

# Some examples of WShEx

The following Shape describes a `<Human>` as an entity with property 
`:P31` (_instance of_) with the value `wd:Q5` (_Human_)

```
<Human> {
  :P31 [ :Q5 ]  
}
```

`<EnglishSite>` have property `:P856` (_official website_) with any value but with the qualifier `:P407` (_language of work or name_) with the value `:Q1860` (_English_)
 
```
<EnglishSite> {
  :P856 .  << :P407 [ :Q1860 ] >>
}
```

# Entity Schemas to WShEx

## Example with a direct property

The following example in WShEx 
```
<Human> {
  :P31 [ :Q5 ]  
}
```

can be represented in Entity Schemas as 

```
<Human> {
  wdt:P31 [ wd:Q5 ]  
}
```
which is equivalent to 

```
<Human> {
  p:P31 { ps:P31 [ wd:Q5 ]  }
}
```

## Example with a qualifier

The following example in WShEx:

```
<EnglishSite> {
  :P856 .  << :P407 [ :Q1860 ] >>
}
```

can be represented in Entity Schemas as:

```
<EnglishSite> {
  p:P856 {
    ps:P856 . 
    pq:P407 [ :Q1860 ] 
  }
}
```



