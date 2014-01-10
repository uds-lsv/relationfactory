package run;


import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;

import org.tartarus.snowball.SnowballStemmer;

import features.DocumentTagger;
import features.OpenNlpTagger;

import rerac.protos.Corpus.Document;
import util.DocumentExtractor;

public class CandidatesToPOSTagProto {
  
  /**
   * Converts candidates in tab-format into pb format.
   * Candidates are tagged (i.e. it is slow).
   * 
   * TODO: option for not tagging.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 3 && args.length != 4) {
      System.err.println(
        "CandidatesToProto <candidates> <postagger> <proto_doc_out> [<weights>]");
      return;
    }
    String candsFN = args[0];
    String taggerFN = args[1];
    String docFN = args[2];
    BufferedReader weightsBr = args.length == 4 ? 
        new BufferedReader(new FileReader(args[3])) : null;
    
    SnowballStemmer stemmer = null;
    try{
      Class stemClass = Class.forName("org.tartarus.snowball.ext.englishStemmer");
      stemmer = (SnowballStemmer) stemClass.newInstance();
    } catch (Exception e){
      e.printStackTrace();
    }
    
    DocumentTagger posTagger = new OpenNlpTagger(taggerFN);
    
    DataOutputStream dataOut = new DataOutputStream(new BufferedOutputStream(
        new FileOutputStream(docFN)));
    
    BufferedReader br = new BufferedReader(new FileReader(candsFN));
    
    String lastTargetId = null;
    for (String line; (line = br.readLine()) != null;) {
      String[] fields = line.split("\\t", 9);
      String targetId = fields[0];
      String relation = fields[1];
      String slot = fields[2];
      String docid = fields[3];
      int targetStart = Integer.parseInt(fields[4]);
      int targetEnd = Integer.parseInt(fields[5]);
      int slotStart = Integer.parseInt(fields[6]);
      int slotEnd = Integer.parseInt(fields[7]);
      String[] tokens = fields[8].split(" ");
      
      if (!targetId.equals(lastTargetId)) {
        // TODO: logging
        //System.err.println(targetId);
        lastTargetId = targetId;
      }
      Document sentence = DocumentExtractor.makeSentenceDoc(tokens, docid);
      sentence = DocumentExtractor.stem(sentence, stemmer);
      sentence = posTagger.tag(sentence);
      
      double w = 1.0;
      if (null != weightsBr) {
        w = Double.parseDouble(weightsBr.readLine());    
      }
      sentence = DocumentExtractor.addRel(sentence, relation, targetStart, 
          targetEnd, slotStart, slotEnd, w);
      
      // TODO: check how format actually is:
      // Since Arguments could be expansions, preserve the actual query string as an argument annotation.
      sentence = DocumentExtractor.annotateArg(sentence, relation, 0, 
          DocumentExtractor.ARG_ID, targetId);
      sentence = DocumentExtractor.annotateArg(sentence, relation, 1, 
          DocumentExtractor.ARG_ID, slot);
      sentence.writeDelimitedTo(dataOut);
    }
    br.close();
    if (null != weightsBr) {
      weightsBr.close();
    }
    dataOut.close();
  }
}
