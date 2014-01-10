package training;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import matcher.CandidateMatcher;
import matcher.StarContextPatternMatcher;
import matcher.Matcher;
import rerac.protos.Corpus.Document;
import util.DocumentExtractor;

public class PatternLabels {
  /**
   * Creates a file that indicates whether patterns match the sentences in
   * the protobuf or not.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 2) {
      System.err.println("PatternLabels " +
          "<candidates.pb> <patterns>");
      return;
    }
    String sentenceFn = args[0];
    String patternsFn = args[1];
    
    Matcher m1 = new CandidateMatcher(0);
    Matcher m2 = new CandidateMatcher(1);
    
    Map<String, List<String>> relToPatterns = 
        new HashMap<String, List<String>>();
        
    BufferedReader br = new BufferedReader(new FileReader(patternsFn));
    for (String line; (line = br.readLine()) != null; ) {
      line = line.trim();
      if (!line.startsWith("#") && !line.isEmpty()) {
        String[] lineParts = line.split("\\s+", 2);
        String rel = lineParts[0];
        String pat = lineParts[1];
        if (!relToPatterns.containsKey(rel)) {
          relToPatterns.put(rel, new ArrayList<String>());
        }
        relToPatterns.get(rel).add(pat);
      }
    }
    br.close();
    
    Map<String, StarContextPatternMatcher> relToContextMatcher = 
        new HashMap<String, StarContextPatternMatcher>();
    for (String rel : relToPatterns.keySet()) {
      relToContextMatcher.put(rel, 
          new StarContextPatternMatcher(m1, m2, relToPatterns.get(rel)));
    }
    
    BufferedInputStream is = new BufferedInputStream(new FileInputStream(
        sentenceFn));
    for (Document sentence; 
        (sentence = Document.parseDelimitedFrom(is)) != null;) {
      String rel = DocumentExtractor.relations(sentence).get(0);
      StarContextPatternMatcher cpm = relToContextMatcher.get(rel);
      List<String> arguments = cpm.arguments(sentence, false);
      boolean isMatch = arguments.size() > 0;
      System.out.println(isMatch ? "+1" : "0");
   }
   is.close();
  }
}
