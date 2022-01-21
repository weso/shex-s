package es.weso.shextest.manifest


case class EntryParam(
 entry: es.weso.shextest.manifest.Entry, 
 name: String, 
 parentFolder: String, 
 testSelector: TestSelector, 
 ignoreList: List[String]
)
