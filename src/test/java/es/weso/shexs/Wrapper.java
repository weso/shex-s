package es.weso.shexs;
import es.weso.shex._ ;



public class WrapperTest {

  @Test
  public void testWrapper() {
    String schemaStr = "prefix : <http://example.org/>\n" +
                    ":S { :p . } " ;
    String data = "prefix : <http://example.org/>\n" +
                  ":S { :p . } " ;                 
    ValidatorWrapper vw = ValidatorWrapper.create(schemaStr);
    Model dataModel = 
  }

}