package evaluation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

public class SentenceErrorExamples {
  /**
   * This prints out, per relation, the positive sentences with the lowest 
   * scores (false negatives) and the negative sentences with the highest scores
   * (false positives).
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    String keyFn = args[0];
    String predictFn = args[1];
    String candidatesFn = args[2];
    
    Set<String> positives = new HashSet<String>();
    Set<String> negatives = new HashSet<String>();
    
    BufferedReader br = new BufferedReader(new FileReader(keyFn));
    for (String line; (line = br.readLine()) != null;) {
      // 3       SF_ENG_001:per:alternate_names  LTW_ENG_20070425.0002.LDC2009T13        -1      0       Juanita Millender - McDonald    0       270:295 -1      234:417
      // 3       SF_ENG_001:per:cause_of_death   APW_ENG_20070422.0984.LDC2009T13        1       1       cancer  0       213:219 1       37:362
      String[] parts = line.split("\t");
      String qid = parts[1].split(":", 2)[0];
      String rel = parts[1].split(":", 2)[1];
      String docid = parts[2];
      String label = parts[3];
      String response = parts[5];
      String tuple = qid + "\t" + rel + "\t" + response + "\t" + docid;
      if ("1".equals(label) || "+1".equals(label)) {
        positives.add(tuple);
      } else if ("-1".equals(label)) {
        negatives.add(tuple);
      }
    }
    br.close();
    
    // This maps relations to positive tuples with minimum scores.
    Map<String, String> minPositivesTuples = new TreeMap<String, String>();
    // This maps relations to the corresponding scores.
    Map<String, Double> minPositivesScores = new TreeMap<String, Double>();
    Map<String, String> maxNegativesTuples = new TreeMap<String, String>();
    Map<String, Double> maxNegativesScores = new TreeMap<String, Double>();
    
    br = new BufferedReader(new FileReader(predictFn));
    for (String line; (line = br.readLine()) != null;) {
      // SF_ENG_041      org:alternate_names     Washington Post LTW_ENG_20070505.0016.LDC2009T13.3      7       10      1       3       -1.1342196
      String[] parts = line.split("\t");
      String qid = parts[0];
      String rel = parts[1];
      String response = parts[2];
// old docid encoding:
//      String docid = parts[3].replaceFirst("\\.[0-9]+$", "");
// new docid encoding:
      String docid = parts[3].split(":", 2)[0];
      String tuple = qid + "\t" + rel + "\t" + response + "\t" + docid;

      // Avoid all ambiguity:
      boolean isPositive = positives.contains(tuple) && !negatives.contains(tuple);
      boolean isNegative = negatives.contains(tuple) && !positives.contains(tuple);
      
      double score = Double.parseDouble(parts[8]);
      double maxNegScore = maxNegativesScores.containsKey(rel) ? 
          maxNegativesScores.get(rel) : 0.0;
      double minPosScore = minPositivesScores.containsKey(rel) ? 
          minPositivesScores.get(rel) : 0.0;
      
      if (isNegative && score > maxNegScore) {
        maxNegativesScores.put(rel, score);
        maxNegativesTuples.put(rel, tuple);
      }
      if (isPositive && score < minPosScore) {
        minPositivesScores.put(rel, score);
        minPositivesTuples.put(rel, tuple);
      }
    }
    br.close();
    
    System.out.println("Max negatives:");
    //SF_ENG_073      org:parents     Carnival Corp.  APW_ENG_20070319.1047.LDC2009T13.14     0       1       6       8       Carnival Cruise Lines is part of Carnival Corp. , the world 's largest cruise group .
    br = new BufferedReader(new FileReader(candidatesFn));
    for (String line; (line = br.readLine()) != null;) {
      String[] parts = line.split("\t");
      String qid = parts[0];
      String rel = parts[1];
      String response = parts[2];
// old docid encoding:
//      String docid = parts[3].replaceFirst("\\.[0-9]+$", "");
// new docid encoding:
      String docid = parts[3].split(":", 2)[0];
      String tuple = qid + "\t" + rel + "\t" + response + "\t" + docid;
      if (tuple.equals(maxNegativesTuples.get(rel))) {
        System.out.println(line);
      }
    }
    br.close();
    System.out.println("Min positives:");
    br = new BufferedReader(new FileReader(candidatesFn));
    for (String line; (line = br.readLine()) != null;) {
      String[] parts = line.split("\t");
      String qid = parts[0];
      String rel = parts[1];
      String response = parts[2];
// old docid encoding:
//      String docid = parts[3].replaceFirst("\\.[0-9]+$", "");
// new docid encoding:
      String docid = parts[3].split(":", 2)[0];
      String tuple = qid + "\t" + rel + "\t" + response + "\t" + docid;
      if (tuple.equals(minPositivesTuples.get(rel))) {
        System.out.println(line);
      }
    }
    br.close();
  }
}
