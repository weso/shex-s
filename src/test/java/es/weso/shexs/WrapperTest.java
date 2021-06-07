package es.weso.shexs;
import es.weso.shex.* ;
import org.apache.jena.rdf.model.*;
import org.junit.Test;
import java.io.InputStream ;
import java.io.ByteArrayInputStream;




public class WrapperTest {

  @Test
  public void testWrapper() {
    String schemaStr = "prefix : <http://example.org/>\n" +
                    ":S { :p . } " ;
    String dataStr = "prefix : <http://example.org/>\n" +
                  ":S { :p . } " ;                 
    ValidatorWrapper vw = ValidatorWrapper.create(schemaStr, "ShEXC");
    Model dataModel = ModelFactory.createDefaultModel();
    InputStream dataStream = new ByteArrayInputStream(dataStr.getBytes());
    dataModel.read(dataStream,"");

    ResultShapeMap result = vw.validate(dataModel);
  }

}