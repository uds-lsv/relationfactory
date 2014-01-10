package corpus;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.SequenceInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.apache.log4j.Logger;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class XMLCorpusReader extends DefaultHandler {
  
  static Logger logger = Logger.getLogger(XMLCorpusReader.class.getName());
  
  private static Collection<CorpusDocument> readDocumentsFallback(String fn) 
      throws IOException {
    BufferedReader br = new BufferedReader(new InputStreamReader(
        new FileInputStream(fn), "UTF-8"));
    List<CorpusDocument> ret = new ArrayList<CorpusDocument>();
    StringBuilder text = new StringBuilder();
    try {
      String sep = "";
      CorpusDocument currentDoc = null;
      for (String line; (line = br.readLine()) != null;) {
        line = line.trim();
        if (line.startsWith("<")) {
          if (currentDoc != null)           logger.debug("ID = " + currentDoc.id);
          if (line.startsWith("<DOCID>")) {
            // line.substring("<DOCID> ".length(), line.length() - " </DOCID>".length());
            currentDoc.id = line.substring(8, line.length() - 9);
          } else if (line.startsWith("<DOC ") || line.startsWith("<DOC>")) {
            currentDoc = new CorpusDocument();
            currentDoc.path = fn;
            text = new StringBuilder();
            if (line.startsWith("<DOC id=")) {
              // <DOC id="WPB_ENG_20100301.0003" type="story">
              currentDoc.id = line.split("\"",3)[1];
            }
          } else if (line.startsWith("</DOC>")) {
            currentDoc.text = text.toString().replace("&amp;", "&").
                replace("&lt;", "<").replace("&gt;", ">").
                replace("&apos;", "'").replace("&quot;", "\"").
		replace("\u00a0", " "); // no-break space
            ret.add(currentDoc);
            text = null;
            sep = "";
            currentDoc = null;
          } else {
            sep = "\n\n";
          }
        } else {
          text.append(sep); sep = " ";
          text.append(line);
        }
      }
      return ret;
    } finally {
      br.close();
    }
  }
  
  public static Collection<CorpusDocument> readDocuments(String fn) 
      throws IOException {
    CorpusHandler handler = new CorpusHandler();
    FileInputStream fIn = null;
    try {
      fIn = new FileInputStream(fn);
    } catch (FileNotFoundException e) {
      logger.error("Document not found: " + e.toString());
      return handler.documents;
    }
    
    try {
      Enumeration<InputStream> streams = Collections.enumeration(
          Arrays.asList(new InputStream[] {
              new ByteArrayInputStream("<root>".getBytes()),
              fIn,
              new ByteArrayInputStream("</root>".getBytes())
          }));
      SequenceInputStream fullDocStream = new SequenceInputStream(streams);
      SAXParserFactory factory = SAXParserFactory.newInstance();
      factory.setNamespaceAware(true);
      //factory.setValidating(true);
      // Comment in if we can expect to actually find a DTD defintion in XML 
      // file.
      SAXParser parser = factory.newSAXParser();
      parser.parse(fullDocStream, handler);
    } catch(SAXException e) {
      logger.error("Problem parsing document " + fn + ": " + e.toString() + 
          "\n Falling back to old parsing.");
      return readDocumentsFallback(fn);
    } catch(IOException e) {
      logger.error("Problem reading document: " + e.toString());
    } catch (ParserConfigurationException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return handler.documents;
  }
  
}
