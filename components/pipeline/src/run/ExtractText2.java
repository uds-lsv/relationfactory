package run;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;

import util.DscoreFile;
import util.DscoreFile.DscoreEntry;
import util.TextIdentifier;
import corpus.CorpusDocument;
import corpus.CorpusReader;

public class ExtractText2 {
  static Logger logger = Logger.getLogger(ExtractText2.class.getName());
    
  /**
   * Extarcts text for all documents specified in dscore.
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 1) {
      System.err.println("java ExtractText <dscore>");
      return;
    }
    BufferedWriter out = new BufferedWriter(new OutputStreamWriter(System.out, 
        "UTF-8"));
    DscoreFile dscoreFile = new DscoreFile(args[0]);    
    
    for (String filename : dscoreFile.getPaths()) {
      for (CorpusDocument doc : CorpusReader.readDocuments(filename)) {
        for (DscoreEntry entry : dscoreFile.getByDocId(doc.getId())) {
          TextIdentifier docQueryId = new TextIdentifier(entry.docid, 
              entry.qid, null, null);
          out.write("#" + docQueryId.toValidString() + "\n");
          // TODO: log written query ids.
          logger.info("extracting doc: " + entry.docid);
          if (docQueryId.getQueryId() == null || entry.docid.isEmpty()) {
            logger.error("incorrect query id:" + docQueryId.toString() + "\n for file: " + filename);
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
      }
    }
    out.flush();
  }
}