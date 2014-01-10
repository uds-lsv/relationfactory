package run;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.Collection;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;

import util.DscoreFile;
import util.DscoreFile.DscoreEntry;
import util.TextIdentifier;
import corpus.CorpusDocument;
import corpus.CorpusReader;
import corpus.SGMLCorpusReader;

public class ExtractAllText {
  static Logger logger = Logger.getLogger(ExtractText2.class.getName());
    
  /**
   * Extracts the text for all documents in a given SGML/TAC-format file.
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {    
    Collection<CorpusDocument> docs;
    if (args.length == 1) {
      String filename = args[0];
      docs = CorpusReader.readDocuments(filename);
    } else {
      docs = CorpusReader.readDocuments(System.in);
    }
    
    BufferedWriter out = new BufferedWriter(new OutputStreamWriter(System.out, 
        "UTF-8"));
    for (CorpusDocument doc : docs) {
      TextIdentifier docQueryId = new TextIdentifier(doc.getId(), 
          null, null, null);
      out.write("#" + docQueryId.toValidString() + "\n");
      logger.info("extracting doc: " + doc.getId());
      if (docQueryId.getDocId() == null) {
        logger.error("incorrect query id:" + docQueryId.toString());
      }
      if (!doc.getTitle().isEmpty()) {
        out.write(doc.getTitle().replaceFirst("^#+", "") + "\n\n");
      }
      Pattern p = Pattern.compile("^#+", Pattern.MULTILINE);
      java.util.regex.Matcher m = p.matcher(doc.getBody());
      StringBuffer sb = new StringBuffer(); 
      while(m.find()) {
        m.appendReplacement(sb, "");
      } 
      m.appendTail(sb); 
      out.write(sb + "\n");
    }
    out.flush();
  }
}