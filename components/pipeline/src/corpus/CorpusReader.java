package corpus;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.zip.GZIPInputStream;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.Logger;

public class CorpusReader {

  static Logger logger = Logger.getLogger(XMLCorpusReader.class.getName());

  private static final String[] entities = new String[]{
    "&amp;",
    "&lt;",
    "&gt;",
    "&apos;",
    "&quot;",
  };
  private static final String[] entityExpansions = new String[]{
    "&",
    "<",
    ">",
    "'",
    "\"",
  };

  public static String replaceEntities(String in) {
    return StringUtils.replaceEach(in, entities, entityExpansions);
  }

  public static Collection<CorpusDocument> readDocuments(String fn) 
      throws IOException {
    return SGMLCorpusReader.readDocuments(fn);
  }

  public static Iterable<RawCorpusDocument> readDocumentsRaw(String fn) {
    BufferedReader br = null;
    List<RawCorpusDocument> docs = new LinkedList<RawCorpusDocument>(); 
    try {
      InputStream is = fn.endsWith(".gz") ?
              new GZIPInputStream(new FileInputStream(fn)) :  new FileInputStream(fn);
      br = new BufferedReader(new InputStreamReader(is, "UTF-8"));
    } catch (IOException e) {
      logger.error("Could not open document file: " + e);
      return docs;
    }
    StringBuilder text = new StringBuilder();
    int linestartOffset = 0;
    String line = "";
    String id = "";

    int ch = -1;
    try {
      while(true) {
        ch = br.read(); // preserving linebreaks
        if (ch > -1) text.append((char)ch);
        if (ch > -1 && ch != '\n') continue;

        if (ch > -1 // TODO: review this.
            && linestartOffset > 0 
            && linestartOffset >= text.length()) {
          throw new IllegalStateException("linestart offset >= length of buffer. " +
          		"ch=" + (char)ch + ", offset=" + linestartOffset + ", " +
          				"length=" + text.length() + ", docfile="+fn+", " +
          						"text=" + text.toString());
        }
        line = text.substring(linestartOffset);
        linestartOffset = text.length();
        String lowerline = line.toLowerCase().trim();
        // Beware: line ends in linebreak (\n)!
        if (lowerline.startsWith("<docid>")) {
          // line.substring("<DOCID> ".length(), line.length() - " </DOCID>".length());
          id = line.substring(	lowerline.indexOf('>') + 1, 
              lowerline.indexOf("</docid>")).trim();
        } else if (lowerline.startsWith("<doc id=")) {
          // <DOC id="WPB_ENG_20100301.0003" type="story">
          id = line.split("\"",3)[1];
        } else if (lowerline.startsWith("</doc>")) {
          docs.add(new RawCorpusDocument(id.trim(), fn, text.toString()));
          text = new StringBuilder();
          linestartOffset = 0;
          id = "";
        }
        // EOF
        if (ch == -1) break;
      }
      br.close();
    } catch (IOException e) {
      logger.error("Problems reading document file: " + e);
    }
    return docs;
  }

  public static Collection<CorpusDocument> readDocuments(InputStream in) throws IOException {
    return SGMLCorpusReader.readDocuments(in);
  }
}
