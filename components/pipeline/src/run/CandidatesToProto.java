package run;


import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;

import org.tartarus.snowball.SnowballStemmer;


import rerac.protos.Corpus.Document;
import util.Candidate;
import util.DocumentExtractor;

public class CandidatesToProto {
  
  /**
   * Converts candidates in tab-format into pb format.
   * Candidates are not tagged (i.e. it is relatively fast).
   * 
   * TODO: option for not tagging.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 2 && args.length != 3) {
      System.err.println(
        "CandidatesToProto <candidates> <proto_doc_out> [<weights>]");
      return;
    }
    String candsFN = args[0];
    String docFN = args[1];
    BufferedReader weightsBr = args.length == 3 ? 
        new BufferedReader(new FileReader(args[2])) : null;
    
    SnowballStemmer stemmer = null;
    try{
      Class stemClass = Class.forName("org.tartarus.snowball.ext.englishStemmer");
      stemmer = (SnowballStemmer) stemClass.newInstance();
    } catch (Exception e){
      e.printStackTrace();
    }
    
    DataOutputStream dataOut = new DataOutputStream(new BufferedOutputStream(
        new FileOutputStream(docFN)));
    
    BufferedReader br = new BufferedReader(new FileReader(candsFN));
    
    String lastTargetId = null;
    for (String line; (line = br.readLine()) != null;) {
      Candidate cand = null;
      try {
        cand = Candidate.fromDelimLine(line);
      } catch (IllegalArgumentException e) {
        System.err.println("Line in candidates file was invalid, skipping: " + 
            e);
        continue;
      }
      
      if (!cand.getQid().equals(lastTargetId)) {
        // TODO: logging
        //System.err.println(targetId);
        lastTargetId = cand.getQid();
      }
      Document sentence = DocumentExtractor.makeSentenceDoc(cand.getTokens(), 
          cand.getIdentifier());
      sentence = DocumentExtractor.stem(sentence, stemmer);
      
      double w = 1.0;
      if (null != weightsBr) {
        w = Double.parseDouble(weightsBr.readLine());    
      }
      try {
      sentence = DocumentExtractor.addRel(sentence, cand.getRel(), 
          cand.getTargetStart(), cand.getTargetEnd(), cand.getFillerStart(), 
          cand.getFillerEnd(), w);
      } catch (IndexOutOfBoundsException e) {
        throw new IndexOutOfBoundsException(e + "\n for candidate:\n" + cand.toString());
      }
      // TODO: check how format actually is:
      // Since Arguments could be expansions, preserve the actual query string as an argument annotation.
      sentence = DocumentExtractor.annotateArg(sentence, cand.getRel(), 0, 
          DocumentExtractor.ARG_ID, cand.getQid());
      sentence = DocumentExtractor.annotateArg(sentence, cand.getRel(), 1,
          DocumentExtractor.ARG_ID, cand.getCanonicalFiller());
      //sentence = DocumentExtractor.annotateArg(sentence, cand.getRel(), 1,
      //    DocumentExtractor.ARG_ID, cand.getFiller());
      sentence.writeDelimitedTo(dataOut);
    }
    br.close();
    if (null != weightsBr) {
      weightsBr.close();
    }
    dataOut.close();
  }
}
