package run;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;

import util.DTagged;
import util.DTagged.TaggedSentence;
import util.DscoreFile;
import util.DscoreFile.DscoreEntry;

public class TaggedDocs {
  public static void main(String[] args) throws IOException {
    if (args.length != 1) {
      System.err.println("java TaggedDocs <dscore>");
      return;
    }
    BufferedWriter out = new BufferedWriter(new OutputStreamWriter(System.out, 
        "UTF-8"));
    DscoreFile dscoreFile = new DscoreFile(args[0]);    
    
    for (String filename : dscoreFile.getPaths()) {
      DTagged allDocsInFile = DTagged.readDtag(new File(filename));
      for (DTagged doc : allDocsInFile.getDocs()) {
        // Read every file in dtag only once.
        for (DscoreEntry entry : dscoreFile.getByDocId(doc.getDocIdFromFirstSentence())) {
          // Get all queries this document is relevant for, and write out.
          String qid = entry.qid;
          for (TaggedSentence s : doc.getSentences()) {
            s.getId().setQueryId(qid);
            out.write(s.toString());
          }
        }
      }
    }
    out.flush();
  }
}
