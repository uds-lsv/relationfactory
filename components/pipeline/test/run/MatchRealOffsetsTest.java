package run;

import static org.junit.Assert.assertEquals;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.List;

import org.apache.log4j.Level;
import org.junit.BeforeClass;
import org.junit.Test;

import testutil.JunitTestBase;
import util.OffsetPair;

/*
 * Beware: Integration test, interacting with the FS. Might be slow.
 */
public class MatchRealOffsetsTest extends JunitTestBase {
  
  @BeforeClass
  public static void setUpClass() throws Exception {
    MatchRealOffsets.logger.setLevel(Level.ALL);
  }
  
  @Test
  public void testOffsets() throws IOException, InterruptedException {
    
    // 168
    /*System.err.println(new String("<DOC> \n" + 
        "<DOCID> LTW_ENG_20070425.0002.LDC2009T13.3 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n").length());*/
    
    File doc1 = createFileWithContent("<DOC> \n" + 
        "<DOCID> LTW_ENG_20070425.0002.LDC2009T13.3 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "A Washington Post obituary for Rep. Juanita Millender-McDonald incorrectly credited the congresswoman with being the first African American to chair the House Administration Committee . \n" + 
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
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ");
    File dscoreFile = createFileWithContent(
        "SF_ENG_001 0 " + doc1.getCanonicalPath() + ":LTW_ENG_20070425.0002.LDC2009T13.3 1 3.628674030303955 lucene\n" + 
        "SF_ENG_002 0 " + doc2.getCanonicalPath() + ":doc2 2 3.628674030303955 lucene");
    
    final String candidates = "SF_ENG_001\t" +
    		"per:employee_of\t" +
    		"Washington Post\t" +
    		"LTW_ENG_20070425.0002.LDC2009T13.3:SF_ENG_001:1\t" +
    		"6\t" +
    		"8\t" +
    		"1\t" +
    		"3\t" +
    		"A Washington Post obituary for Rep. Juanita Millender-McDonald incorrectly credited the congresswoman with being the first African American to chair the House Administration Committee .";
    
    OutputStream captureStream = redirectStdOut();
    
    ByteArrayInputStream inStream = new ByteArrayInputStream(
        candidates.getBytes("UTF-8"));
    
    MatchRealOffsets mro = new MatchRealOffsets(dscoreFile.getCanonicalPath());
    BufferedReader br = new BufferedReader(new InputStreamReader(inStream, 
        "UTF-8"));
    mro.printCandidatesWithOffsets(br);
    br.close();
    
    captureStream.flush();
    String out = captureStream.toString();
    
    assertEquals("SF_ENG_001\t" +
        "per:employee_of\t" +
        "Washington Post\t" +
        "LTW_ENG_20070425.0002.LDC2009T13.3:SF_ENG_001:1:170-184:204-229:168-352\t" +
        "6\t" +
        "8\t" +
        "1\t" +
        "3\t" +
        "A Washington Post obituary for Rep. Juanita Millender-McDonald incorrectly credited the congresswoman with being the first African American to chair the House Administration Committee .", 
        out.trim());
  }
  
  @Test
  public void testLinebreakMatching() {
    String content = "<DOC> \n" + 
        "<DOCID> LTW_ENG_20070425.0002.LDC2009T13.3 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "Line1\r\n" +
        "Line2\n" +
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> \n" +
        "";
    String sentence = "Line1 Line2";
    List<OffsetPair> results = 
        MatchRealOffsets.matchSentenceAndArgs(content, sentence);
    assertEquals(1, results.size());
    String matchedString = content.substring(results.get(0).start, 
        results.get(0).end + 1);
    assertEquals(168, results.get(0).start);
    assertEquals("End match wrong, matched " + matchedString, 
        179, results.get(0).end);
  }
  
  @Test
  public void testXmlEntityMatching() {
    // 168
    /*System.err.println(new String("<DOC> \n" + 
        "<DOCID> LTW_ENG_20070425.0002.LDC2009T13.3 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n").length());*/
    String content = "<DOC> \n" + 
        "<DOCID> LTW_ENG_20070425.0002.LDC2009T13.3 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "&quot;These things have become partisan,&quot; Democratic California\n" + 
        "Representative Juanita Millender-McDonald replied at a March 2005\n" + 
        "congressional field hearing when asked why she and others in Congress\n" + 
        "had come to Ohio to investigate the 2004 election. &quot;Images are so\n" + 
        "critical, especially when the stakes are high and stakes are high in\n" + 
        "presidential elections,&quot; the now-deceased congresswoman continued,\n" + 
        "referring to the lingeri g memory of thousands of African-Americans\n" + 
        "waiting for hours outside in a cold rain to vote the previous November\n" + 
        "in Ohio's inner cities. Many elected Democrats and voting rights\n" + 
        "attorneys saw the delays as intentional voter suppression resulting\n" + 
        "from partisan election administration. To some, it stirred memories of\n" + 
        "the segregated south. \n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ";
    String sentence = "`` These things have become partisan, '' Democratic California Representative Juanita Millender-McDonald " + 
    		"replied at a March 2005 congressional field hearing when asked why she and others in Congress had come " + 
    		"to Ohio to investigate the 2004 election .";
    // 250 + 2 + 2 (for quots)
    //System.err.println(sentence.length());
    //String sentence = "`` These things have become partisan, '' Democratic California Representative"; 
    List<OffsetPair> results = 
        MatchRealOffsets.matchSentenceAndArgs(content, sentence);
    assertEquals(1, results.size());
    String matchedString = content.substring(results.get(0).start, 
        results.get(0).end + 1);
    assertEquals(168, results.get(0).start);
    assertEquals("End match wrong, matched " + matchedString, 
        168 + 254, results.get(0).end);
  }
  
  @Test
  public void testWeirdWhitespaceMatching() {
    // 00A0 is unicode for non-breaking space. \c2\a0 in utf8.
    String content = "<DOC> \n" + 
        "<DOCID> LTW_ENG_20070425.0002.LDC2009T13.3 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "Larry Crim is not your typical &quot;wholesaler&quot;. \u00A0In fact, he is not a\n" + 
        "wholesaler per se, but an entrepreneur with a scholarly background who\n" + 
        "has founded and directed a number of enterprises. Crim holds a Master\n" + 
        "of Public Administration degree from University of Tennessee and a\n" + 
        "Master of Organizational Psychology degree from Middle Tennessee State\n" + 
        "University. \u00A0His Master's Thesis on Employer and Employee Perceptions\n" + 
        "was featured at the American Psychological Association's Symposium on\n" + 
        "Organizational Psychology. Crim dervied his acadmic research from\n" + 
        "&quot;real world&quot; Top Management and Employees with whom he worked as an\n" + 
        "Award Winning Executive Recruiter. \u00A0Larry Crim performed his\n" + 
        "Psychological Practicuum at Vanderbilt University. \u00A0Mr. Crim founded\n" + 
        "and served as Executive Director of a Counseling Center in Nashville,\n" + 
        "one of his first entreprenuerial enterprises.\n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ";
    
    String sentence = "\ufffdHis Master's Thesis on Employer and Employee Perceptions\n" + 
        "was featured at the American Psychological Association's Symposium on\n" + 
        "Organizational Psychology.";
    
    List<OffsetPair> results = 
        MatchRealOffsets.matchSentenceAndArgs(content, sentence);
    assertEquals(1, results.size());
    String matchedString = content.substring(results.get(0).start, 
        results.get(0).end + 1);
    assertEquals("Start match wrong, matched " + matchedString, 536, 
        results.get(0).start);
    assertEquals("End match wrong, matched " + matchedString, 
        689, results.get(0).end);
  }
  
  @Test
  public void testWeirdWhitespaceMatching2() {
    // 00A0 is unicode for non-breaking space. \c2\a0 in utf8.
    String content = "<DOC> \n" + 
        "<DOCID> LTW_ENG_20070425.0002.LDC2009T13.3 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "Alcatel \u00A0His\n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ";
    
    String sentence = "Alcatel His";
    
    List<OffsetPair> results = 
        MatchRealOffsets.matchSentenceAndArgs(content, sentence);
    assertEquals(1, results.size());
    String matchedString = content.substring(results.get(0).start, 
        results.get(0).end + 1);
    assertEquals("Start match wrong, matched " + matchedString, 168, 
        results.get(0).start);
    assertEquals("End match wrong, matched " + matchedString, 
        179, results.get(0).end);
  }
  
  @Test
  public void testQuotationMarkMatching() {
    String content = "<DOC> \n" + 
        "<DOCID> LTW_ENG_20070425.0002.LDC2009T13.3 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "\"We will also carry out attacks against the Pakistan People's\n" + 
        "Party (led by Zardari) and Awami National Party leadership,\" he\n" + 
        "said, referring to two of the parties that comprise Pakistan's\n" + 
        "fragile coalition government.\n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ";
    
    String sentence = "`` We will also carry out attacks against the Pakistan People 's Party ( led by Zardari ) and Awami National Party leadership, '' he said , referring to two of the parties that comprise Pakistan 's fragile coalition government .";
    
    List<OffsetPair> results = 
        MatchRealOffsets.matchSentenceAndArgs(content, sentence);
    assertEquals(1, results.size());
    String matchedString = content.substring(results.get(0).start, 
        results.get(0).end + 1);
    assertEquals("Start match wrong, matched " + matchedString, 168, 
        results.get(0).start);
    assertEquals("End match wrong, matched " + matchedString, 
        385, results.get(0).end);
  }
  
  @Test
  public void testEmailSplitMatching() {
    String content = "<DOC> \n" + 
        "<DOCID> LTW_ENG_20070425.0002.LDC2009T13.3 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "On 14 Aug, 13:21, Steve Taylor &lt;st ... @thetaylorfamily.org.uk&gt; wrote:\n" + 
        "\n" + 
        "<QUOTE PREVIOUSPOST=\"\n" + 
        "&gt; Rich B wrote:\n" + 
        "&gt; &gt; On 14 Aug, 09:06, Steve Taylor &lt;st ... @thetaylorfamily.org.uk&gt; wrote:\n" + 
        "&gt; &gt;&gt; Richard wrote:\n" + 
        "&gt; &gt;&gt;&gt; Does anyone know where I can get 1/4&quot; UNC Nyloc nuts?\n" + 
        "&gt; &gt;&gt; Depends where you are. I have some in my collection.\n" + 
        "\n" + 
        "&gt; &gt;&gt; Steve\n" + 
        "\n" + 
        "&gt; &gt; Forgive me for butting in, but I thought the idea behind threadlock\n" + 
        "&gt; &gt; was to make it difficult, but not *impossible*,\n" + 
        "\n" + 
        "&gt; &lt;3mm screws are VERY likely to shear off before you break the bond of\n" + 
        "&gt; even threadlock. The torch trick we use to release 1mm screws !\n" + 
        "\">\n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ";
    
    String sentence = "> Rich B wrote : > > On 14 Aug , 09:06 , Steve Taylor < st...@thetaylorfamily.org.uk > wrote : > > > Richard wrote : > > > > Does anyone know where I can get 1/4 '' UNC Nyloc nuts ?";
    
    List<OffsetPair> results = 
        MatchRealOffsets.matchSentenceAndArgs(content, sentence);
    assertEquals(1, results.size());
    String matchedString = content.substring(results.get(0).start, 
        results.get(0).end + 1);
    assertEquals("Start match wrong, matched " + matchedString, 268, 
        results.get(0).start);
    assertEquals("End match wrong, matched " + matchedString, 
        478, results.get(0).end);
  }
  
  @Test
  public void testStuff() {
    String content = "<DOC> \n" + 
        "<DOCID> LTW_ENG_20070425.0002.LDC2009T13.3 </DOCID> \n" + 
        "<DOCTYPE SOURCE=\"broadcast news\"> NEWS STORY </DOCTYPE> \n" + 
        "<DATETIME> 1994-11-23 </DATETIME> \n" + 
        "<BODY> \n" + 
        "<TEXT> \n" + 
        "&gt;- pissy\n" + 
        "&gt; ? \u00a0adjective [vulgar slang]\n" + 
        "&gt; \u00a0 \u00a0relating to urine.\n" + 
        "&gt; \u00a0 \u00a0contemptible or inferior\n" + 
        "&gt; .\n" + 
        "&gt; \u00a9 Oxford University Press, 2004\n" + 
        "\n" + 
        "&gt; Yeah, thanks for the flowers, sistas.\n" + 
        "</TEXT> \n" + 
        "</BODY> \n" + 
        "</DOC> ";
    
    //String sentence = "> \ufffd \ufffd or inferior > . > \u00a9 Oxford University Press , 2004 > Yeah , thanks for the flowers , sistas .";
    String sentence = "> \ufffd \ufffdcontemptible or inferior > . > \u00a9 Oxford University Press , 2004 > Yeah , thanks for the flowers , sistas .";
    
    List<OffsetPair> results = 
        MatchRealOffsets.matchSentenceAndArgs(content, sentence);
    assertEquals(1, results.size());
    String matchedString = content.substring(results.get(0).start, 
        results.get(0).end + 1);
    assertEquals("Start match wrong, matched " + matchedString, 240, 
        results.get(0).start);
    assertEquals("End match wrong, matched " + matchedString, 
        359, results.get(0).end);
  }
  
  @Test
  public void testMoreStuff() {
    String content = "&gt;Joe wrote:\n" + 
    		"&gt;&gt; A very long page of evidence\n" + 
    		"\n" + 
    		"&gt;Is Joe short for Jobst?!? \u00a0LOL\n" + 
    		"\">\n" + 
    		"\n" + 
    		"Well, considering all the crime/detective shows -- Monk, Psych, L&amp;O:\n" + 
    		"CI (new &amp; reruns) L&amp;O: SVU (reruns), Burn Notice, etc. -- \u00a0USA\n" + 
    		"*network* (channel) is certainly become a police state.\n" + 
    		"\n" + 
    		"-- Rob\n" + 
    		"--";
    
    //String sentence = "> \ufffd \ufffd or inferior > . > \u00a9 Oxford University Press , 2004 > Yeah , thanks for the flowers , sistas .";
    String sentence = "LOL `` > Well , considering all the crime/detective shows -- Monk , Psych , L&O : CI ( new & reruns ) L&O : SVU ( reruns ) , Burn Notice , etc. -- USA *network* ( channel ) is certainly become a police state .";
    
    List<OffsetPair> results = 
        MatchRealOffsets.matchSentenceAndArgs(content, sentence);
    assertEquals(1, results.size());
    String matchedString = content.substring(results.get(0).start, 
        results.get(0).end + 1);
    assertEquals("Start match wrong, matched " + matchedString, 85, 
        results.get(0).start);
    assertEquals("End match wrong, matched " + matchedString, 
        291, results.get(0).end);
  }
  
}
