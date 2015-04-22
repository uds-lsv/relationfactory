package corpus;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;

import org.apache.log4j.Level;
import org.junit.BeforeClass;
import org.junit.Test;

import testutil.JunitTestBase;

/*
 * Beware: Integration test, interacting with the FS. Might be slow.
 */
public class CorpusReaderTest extends JunitTestBase {
  
  @BeforeClass
  public static void setUpClass() throws Exception {
    CorpusReader.logger.setLevel(Level.ALL);
  }
  
  @Test
  public void testSimpleSGMLDoc() throws IOException {
    File f = createFileWithContent("<DOC> \n" + 
    		"<DOCID> CNN929-15.941123.LDC98T25 </DOCID> \n" + 
    		"<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
    		"<DATETIME> 1994-11-23 </DATETIME> \n" + 
    		"<BODY> \n" + 
    		"<TEXT> \n" + 
    		"<TURN> \n" + 
    		"<SPEAKER> DEXTER KOEHL, Travel Ind. Association of America </SPEAKER> \n" + 
    		"I certainly am. \n" + 
    		"</TURN> \n" + 
    		"<TURN> \n" + 
    		"<SPEAKER> CAIN </SPEAKER> \n" + 
    		"We keep saying this is one of the biggest travel days of the \n" + 
    		"year. It seems to me I had heard, at one point, it was the \n" + 
    		"biggest, do you know? \n" + 
    		"</TURN> \n" + 
    		"<TURN> \n" + 
    		"<SPEAKER> Mr. KOEHL </SPEAKER> \n" + 
    		"Yes, it is. Based on research that our U.S. Travel Data Center \n" + 
    		"has conducted for the American Automobile Association, this is \n" + 
    		"the busiest travel day or holiday weekend of the year. More than \n" + 
    		"30 million Americans will be traveling this weekend. In fact, \n" + 
    		"that's about 3 percent more than last year, almost a million \n" + 
    		"people more than over the Thanksgiving holiday last year. \n" +
    		"</TURN> \n" + 
    		"</TEXT> \n" + 
    		"</BODY> \n" + 
    		"</DOC> ");
    Collection<CorpusDocument> docs = CorpusReader.readDocuments(
        f.getCanonicalPath());
    assertEquals(1, docs.size());
    CorpusDocument doc = docs.iterator().next();
    //assertEquals(f.getCanonicalPath(), doc.getPath());
    assertEquals("CNN929-15.941123.LDC98T25", doc.getId());
    assertTrue("Body incorrect: \n" + doc.getBody(), 
        doc.getBody().contains("Mr. KOEHL\n\nYes, it is."));
    assertTrue("Body incorrect: \n" + doc.getBody(),
        doc.getBody().contains("do you know?\n\nMr. KOEHL"));
  }
  
  @Test
  public void testSimpleSGMLDoc2() throws IOException {
    File f = createFileWithContent("<DOC> \n" + 
        "<DOCID> CNN929-15.941123.LDC98T25 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "<TURN> \n" + 
        "I certainly am. \n" +
        "<SPEAKER> DEXTER KOEHL, Travel Ind. Association of America </SPEAKER> \n" +
        "</TURN> \n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ");
    Collection<CorpusDocument> docs = CorpusReader.readDocuments(
        f.getCanonicalPath());
    assertEquals(1, docs.size());
    CorpusDocument doc = docs.iterator().next();
    assertTrue("Body incorrect -- lost text: \n" + doc.getBody(), 
        doc.getBody().contains("I certainly am."));
    assertTrue("Body incorrect: \n" + doc.getBody(),
        doc.getBody().contains("DEXTER KOEHL"));
  }
  
  @Test
  public void testSimpleSGMLDoc3() throws IOException {
    File f = createFileWithContent("<DOC id=\"CNN929-15.941123.LDC98T25\"> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "Test. \n" +
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ");
    Collection<CorpusDocument> docs = CorpusReader.readDocuments(
        f.getCanonicalPath());
    assertEquals(1, docs.size());
    CorpusDocument doc = docs.iterator().next();
    assertEquals("CNN929-15.941123.LDC98T25", doc.getId());
    assertTrue("Body incorrect: \n" + doc.getBody(),
        doc.getBody().contains("Test."));
  }
  
  @Test
  public void testSGMLDocEntityHandling() throws IOException {
    File f = createFileWithContent("<DOC> \n" + 
        "<DOCID> CNN929-15.941123.LDC98T25 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "<TURN> \n" + 
        "&amp; &quot;content &apos; &lt; &gt;content\n" + 
        "</TURN> \n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ");
    Collection<CorpusDocument> docs = CorpusReader.readDocuments(
        f.getCanonicalPath());
    assertEquals(1, docs.size());
    CorpusDocument doc = docs.iterator().next();
    assertEquals("Entitys not escaped", doc.getBody().trim(),
        "& \"content ' < >content");
  }
  
  @Test
  public void testReallySimpleSGMLDoc() throws IOException {
    File f = createFileWithContent("<DOC> \n" + 
        "<DOCID> doc1 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "content \n" +
        "moar content\n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ");
    Collection<CorpusDocument> docs = CorpusReader.readDocuments(
        f.getCanonicalPath());
    assertEquals(1, docs.size());
    CorpusDocument doc = docs.iterator().next();
    //assertEquals(f.getCanonicalPath(), doc.getPath());
    assertEquals("doc1", doc.getId());
    assertEquals("Body incorrect", 
        "content \nmoar content", doc.getBody());
  }
  
  @Test
  public void testMultipleDocs() throws IOException {
    File f = createFileWithContent("<DOC> \n" + 
        "<DOCID> doc1 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "content \n" +
        "moar content\n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> \n" +
        "<DOC> \n" + 
        "<DOCID> doc2 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "content \n" +
        "moar content\n <QUOTE quote=\"quote\">\n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ");
    Collection<CorpusDocument> docs = CorpusReader.readDocuments(
        f.getCanonicalPath());
    assertEquals(2, docs.size());
    Iterator<CorpusDocument> docIter = docs.iterator();
    CorpusDocument doc = docIter.next();
    //assertEquals(f.getCanonicalPath(), doc.getPath());
    assertEquals("doc1", doc.getId());
    assertEquals("Body incorrect", 
        "content \nmoar content", doc.getBody());
    doc = docIter.next();
    assertEquals("doc2", doc.getId());
    assertEquals("Body incorrect", 
        "content \nmoar content\n\nquote", doc.getBody());
  }
  
  @Test
  public void testDirtySGMLDoc() throws IOException {
    File f = createFileWithContent("<DOC> \n" + 
        "<DOCID> CNN929-15.941123.LDC98T25 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "<QUOTE quote=\"\n" + 
        "I certainly am. \n" + 
        "\">\n" +
        "This is an &amp; &quot; &apos; &lt; &gt;\n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ");
    Collection<CorpusDocument> docs = CorpusReader.readDocuments(
        f.getCanonicalPath());
    assertEquals(1, docs.size());
    CorpusDocument doc = docs.iterator().next();
    assertEquals("CNN929-15.941123.LDC98T25", doc.getId());
    assertTrue("Body incorrect: \n" + doc.getBody(), 
        doc.getBody().contains("I certainly am."));
    assertTrue("Body incorrect: \n" + doc.getBody(), 
        doc.getBody().contains("This is an & \" ' < >"));
  }
  
  @Test
  public void test2013DiscussionDoc() throws IOException {
    File f = createFileWithContent("<doc id=\"id\"> \n" + 
        "<headline> headline </headline> \n" + 
        "<post> \n" + 
        "<quote> quote <quote> nested </quote> </quote>\n" +
        "Oh look, a post!\n" + 
        "</post> \n" +
        "<post> \n" + 
        "another post!\n" + 
        "</post> \n" +
        "</doc> ");
    Collection<CorpusDocument> docs = CorpusReader.readDocuments(
        f.getCanonicalPath());
    assertEquals(1, docs.size());
    CorpusDocument doc = docs.iterator().next();
    assertEquals("id", doc.getId());
    assertTrue("Body incorrect: quote missing \n" + doc.getBody(), 
        doc.getBody().contains("quote\n\nnested"));
    assertTrue("Body incorrect: \n" + doc.getBody(), 
        doc.getBody().contains("Oh look, a post!"));
    assertTrue("Body incorrect: \n" + doc.getBody(), 
        doc.getBody().contains("another post!"));
  }
  
  @Test
  public void testRawDocs() throws IOException {
    String doc1Content = "<DOC> \n" + 
        "<DOCID> doc1 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "content \r\n" +  // \r\n
        "moar content\n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> \n";
    File f = createFileWithContent(doc1Content +
        "<DOC> \n" + 
        "<DOCID> Doc2 </DOCID> \n" + // uppercase id 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "content \n" +
        "moar content\n <QUOTE quote=\"quote\">\n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC>"); // no linebreak at EOF
    Iterator<RawCorpusDocument> docIter = CorpusReader.readDocumentsRaw(
        f.getCanonicalPath()).iterator();
    
    RawCorpusDocument doc = docIter.next();
    //assertEquals(f.getCanonicalPath(), doc.getPath());
    assertEquals("doc1", doc.getId());
    assertEquals("Body incorrect", 
        doc1Content, doc.getRawContent());
    assertTrue("Only one document found. Should be 2", docIter.hasNext());
    doc = docIter.next();
    assertEquals("Doc2", doc.getId());
  }
    
}
