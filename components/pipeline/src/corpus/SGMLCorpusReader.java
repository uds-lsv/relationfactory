package corpus;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.SequenceInputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.zip.GZIPInputStream;

import org.apache.log4j.Logger;
import org.ccil.cowan.tagsoup.Parser;
import org.ccil.cowan.tagsoup.Schema;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

public class SGMLCorpusReader {

  static Logger logger = Logger.getLogger(XMLCorpusReader.class.getName());
  
  static class DocSchema extends Schema {
    public DocSchema() {
    }
  }

  static Collection<CorpusDocument> readDocuments(InputStream ins) 
      throws IOException {
    CorpusHandler handler = new CorpusHandler();
    try {
      Enumeration<InputStream> streams = Collections.enumeration(
          Arrays.asList(new InputStream[] {
              new ByteArrayInputStream("<root>".getBytes()),
              ins,
              new ByteArrayInputStream("</root>".getBytes())
          }));
      SequenceInputStream fullDocStream = new SequenceInputStream(streams);

      XMLReader r;
      r = new Parser();
      
      r.setFeature(Parser.restartElementsFeature, false);
      r.setFeature(Parser.rootBogonsFeature, true);
      r.setContentHandler(handler);
      //r.setProperty(Parser.schemaProperty, new DocSchema());
      InputSource s = new InputSource(fullDocStream);
      s.setEncoding("UTF-8");
      r.parse(s);
    } catch (SAXException e) {
      logger.fatal(e);
    }
    return handler.documents;
  }
  
  static Collection<CorpusDocument> readDocuments(String fn) 
      throws IOException {
    CorpusHandler handler = new CorpusHandler();

    InputStream fIn = null;
    try {
      fIn = fn.endsWith(".gz") ?
              new GZIPInputStream(new FileInputStream(fn)) :  new FileInputStream(fn);
      return readDocuments(fIn);
    } catch (FileNotFoundException e) {
      logger.error("Document not found: " + e.toString());
    } finally {
      if (fIn != null) {
        fIn.close();
      }
    }
    return handler.documents;
  }
}
