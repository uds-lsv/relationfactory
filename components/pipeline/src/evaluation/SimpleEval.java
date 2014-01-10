package evaluation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

/**
 * 
 * This is a simple evaluation script. It doesn't care about single or list 
 * slots, redundant answers or the like. It just counts how often there is an
 * exact match with one of the correct gold standard answers.
 * 
 * @author Benjamin Roth
 *
 */
public class SimpleEval {
  public static void main(String[] args) throws IOException {
    if (args.length != 3) {
      System.err.println("java SimpleEval <response> <key> <anydoc=true|false>");
      System.err.println("response:    the response to be evaluated.");
      System.err.println("key:         keyfile to be compared with.");
      System.err.println("anydoc:      whether doc id should be ignored.");
      System.err.println("Precision and Recall are written to standard out.");
      return;
    }
    String responseFn = args[0];
    String keyFn = args[1];
    boolean anyDoc = args[2].equals("true");

    Set<String> responseAnswers = new HashSet<String>();
    BufferedReader br = new BufferedReader(new FileReader(responseFn));
    for (String line; (line = br.readLine()) != null;) {
      // SF500   org:city_of_headquarters        lsv     NYT_ENG_20070119.0341.LDC2009T13        Greenwich       0       0       0       0       1.0
      String[] parts = line.split("\t");
      String qid = parts[0];
      String rel = parts[1];
      String doc = parts[3];
      String slot = parts[4];
      if (anyDoc) {
        responseAnswers.add(qid + "\t" + rel + "\t" + doc + "\t" + slot);
      } else {
        responseAnswers.add(qid + "\t" + rel + "\t" + slot);        
      }
    }
    br.close();

    Set<String> keyAnswers = new HashSet<String>();
    br = new BufferedReader(new FileReader(keyFn));
    for (String line; (line = br.readLine()) != null;) {
      // 22 SF500:org:alternate_names APW_ENG_20070813.0797.LDC2009T13 3 0 LLC
      String[] parts = line.split(" ", 6);
      String qid = parts[1].split(":", 2)[0];
      String rel = parts[1].split(":", 2)[1];
      String doc = parts[2];
      String slot = parts[5];
      if (anyDoc) {
        keyAnswers.add(qid + "\t" + rel + "\t" + doc + "\t" + slot);
      } else {
        keyAnswers.add(qid + "\t" + rel + "\t" + slot);        
      }
    }
    br.close();
    
    Set<String> relevantRetrieved = new HashSet<String>(keyAnswers);
    relevantRetrieved.retainAll(responseAnswers);
    double precision =  relevantRetrieved.size() / (double) responseAnswers.size();
    double recall = relevantRetrieved.size() / (double) keyAnswers.size();
    
    System.out.println("Relevant: " + keyAnswers.size());
    System.out.println("Retrieved: " + responseAnswers.size());
    System.out.println("Relevant retrieved: " + relevantRetrieved.size());
    System.out.println("Precision: " + precision);
    System.out.println("Recall: " + precision);
    System.out.println("FScore: " + (2.0 * precision * recall / (precision + recall)));
  }
}
