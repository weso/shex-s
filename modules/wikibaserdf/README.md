# Wikibase RDF

This module implements an `RDFReader` customized for Wikibase instances.

The main difference from Endpoint `RDFReader` is that it gets the outgoing arcs by dereferencing a node which is more efficient than going through the SPARQL endpoint.

The incoming arcs are obtained by SPARQL queries to the endpoint.