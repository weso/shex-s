package es.weso.shexsjena;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import es.weso.shapemaps.*;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.commons.io.IOUtils;
import es.weso.utils.VerboseLevel;
import es.weso.rdf.nodes.IRI;
import es.weso.rdf.nodes.RDFNode;
import java.net.*;
import es.weso.shapemaps.IRILabel;

public class ShExsValidatorTest {

 @Test
 public void exampleAddition() {
    assertEquals(1 + 1, 2);
 }

@Test
 public void exampleValidatorPass() throws URISyntaxException {
    String schemaStr = 
      "prefix : <http://example.org/>\n" +
      ":S {\n" +
      " :p .\n" +
      "}";

    String rdfStr = 
      "prefix : <http://example.org/>\n" +
      ":x :p 1 ." ;
    
    ShExsJenaValidator validator = 
      ShExsJenaValidatorBuilder.fromStringSync(schemaStr,"ShExC");
    
    // Create Jena Model from RDF String
    Model model = ModelFactory.createDefaultModel()
        .read(IOUtils.toInputStream(rdfStr, "UTF-8"), null, "TURTLE");  
    
    ResultShapeMap result = 
      validator.validateNodeShapeSync(model, 
        "http://example.org/x", 
        "http://example.org/S");
    
    RDFNode x = new IRI(new URI("http://example.org/x"));
    IRILabel s = new IRILabel(new IRI(new URI("http://example.org/S")));
    
    // System.out.println("ResultShapeMap = " + result);
    // System.out.println("ConformantShapes(x) = " + result.getConformantShapes(x));
    // System.out.println("ConformantShapes(x).contains(s) = " + result.getConformantShapes(x).contains(s));
    assertEquals(true, result.getConformantShapes(x).contains(s));
 }

@Test
 public void exampleValidatorFails() throws URISyntaxException {
    String schemaStr = 
      "prefix : <http://example.org/>\n" +
      ":S {\n" +
      " :p .\n" +
      "}";

    String rdfStr = 
      "prefix : <http://example.org/>\n" +
      ":x :other 1 ." ;
    
    ShExsJenaValidator validator = 
      ShExsJenaValidatorBuilder.fromStringSync(schemaStr,"ShExC");
    
    // Create Jena Model from RDF String
    Model model = ModelFactory.createDefaultModel()
        .read(IOUtils.toInputStream(rdfStr, "UTF-8"), null, "TURTLE");  
    
    ResultShapeMap result = 
      validator.validateNodeShapeSync(model, 
        "http://example.org/x", 
        "http://example.org/S");
    
    RDFNode x = new IRI(new URI("http://example.org/x"));
    IRILabel s = new IRILabel(new IRI(new URI("http://example.org/S")));
    
    assertEquals(false, result.getConformantShapes(x).contains(s));
 }

}
