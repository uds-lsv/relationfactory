package run;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

import org.apache.log4j.Level;
import org.junit.BeforeClass;
import org.junit.Test;

import testutil.JunitTestBase;

/*
 * Beware: Integration test, interacting with the FS. Might be slow.
 */
public class ExtractTextTest extends JunitTestBase {
  
  @BeforeClass
  public static void setUpClass() throws Exception {
    ExtractText2.logger.setLevel(Level.ALL);
  }
  
  @Test
  public void testMain() throws IOException {
    File doc1 = createFileWithContent("<DOC> \n" + 
        "<DOCID> doc1 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "content \n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> \n" +
        "<DOC> \n" + 
        "<DOCID> doc11 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "content11 \n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ");
    File doc2 = createFileWithContent("<DOC> \n" + 
        "<DOCID> doc2 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "content2 \n" +
        "moar content \n" +
        "#raute \n" +
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ");
    File dscoreFile = createFileWithContent(
        "SF_ENG_001 0 " + doc1.getCanonicalPath() + ":doc1 1 3.628674030303955 lucene\n" + 
        "SF_ENG_001 0 " + doc1.getCanonicalPath() + ":doc11 1 3.628674030303955 lucene\n" +
        "SF_ENG_003 0 " + doc1.getCanonicalPath() + ":doc11 1 3.628674030303955 lucene\n" +
    		"SF_ENG_002 0 " + doc2.getCanonicalPath() + ":doc2 2 3.628674030303955 lucene");
    
    OutputStream captureStream = redirectStdOut();
    ExtractText2.main(new String[]{dscoreFile.getCanonicalPath()});
    
    captureStream.flush();
    String out = captureStream.toString().trim();
    assertTrue(out.contains("#doc1:SF_ENG_001\n" +
    		"content"));
    assertTrue(out.contains("#doc2:SF_ENG_002\n" +
    		"content2 \n" +
    		"moar content \n" +
    		"raute"));
    assertTrue(out.contains("#doc11:SF_ENG_001\n" +
        "content11"));
    assertTrue(out.contains("#doc11:SF_ENG_003\n" +
        "content11"));
    
  }
  
}
