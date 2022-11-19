package es.weso.wbmodel

case class PropertyRecord(id: PropertyId, vertexId: VertexId) {
  override def toString = s"$id-$vertexId"
}
