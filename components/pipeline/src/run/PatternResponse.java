package run;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import matcher.*;
import query.QueryList;
import rerac.protos.Corpus.Document;
import util.DocumentExtractor;
import util.Responses;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;



public class PatternResponse {
  /**
   * This writes a response from a sentence file and a list of patterns.
   * Patterns can only include wildcards '*' if that fast_match is set to false.
   * No redundancy elimination whatsoever is done, so literally same slots may
   * be written out. Therefore, RedundancyEliminator needs to be called in order
   * to obtain a final response.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 4 && args.length != 5) {
      System.err.println("PatternResponse " +
          "<query_expanded_xml> <sentences> <patterns> <team_id> [<fast_match=true|false|shortened>]");
      System.err.println(
          "Fast matching allows only for patterns starting with $ARG1, having no star");
      System.err.println(
          "wildcards and ending with $ARG2.");
      return;
    }
    QueryList ql = new QueryList(args[0]);
    String sentenceFn = args[1];
    String patternsFn = args[2];
    String teamId = args[3];
    boolean fast = (args.length > 4 && (args[4].equals("true") || args[4].equals("shortened"))) ? true : false;
    boolean shortened = fast && args[4].equals("shortened");
    
    if (fast) {
      System.err.println("Fast matching.");
      if (shortened) {
        System.err.println("Using shortened patterns.");
      }
    } else {
      System.err.println("Star pattern matching.");
    }
    
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
    
    Map<String, ContextPatternMatcher> relToContextMatcher = 
        new HashMap<String, ContextPatternMatcher>();
    for (String rel : relToPatterns.keySet()) {
      if (fast) {
        if (shortened) {
          relToContextMatcher.put(rel,
              new ShortenedContextPatternMatcher(m1, m2, relToPatterns.get(rel)));
        } else {
          relToContextMatcher.put(rel,
              new FastContextPatternMatcher(m1, m2, relToPatterns.get(rel)));
        }
      } else {
        relToContextMatcher.put(rel, 
            new StarContextPatternMatcher(m1, m2, relToPatterns.get(rel)));
      }
    }

    Responses r = new Responses(ql);
    
    BufferedInputStream is = new BufferedInputStream(new FileInputStream(
        sentenceFn));
    for (Document sentence; 
        (sentence = Document.parseDelimitedFrom(is)) != null;) {
      String rel = DocumentExtractor.relations(sentence).get(0);
      ContextPatternMatcher cpm = relToContextMatcher.get(rel);
      if (null != cpm) {
        List<String> arguments = cpm.arguments(sentence, false);
        for (int argInd = 1; argInd < arguments.size(); argInd += 2) {
          String slot = arguments.get(argInd);
          //String docid = sentence.getId().replaceFirst("\\.[^.]+$", "");
          String qid = DocumentExtractor.canonicalArg(sentence, 0, rel);

          r.addResponse2012(qid, rel, teamId, sentence.getId(), slot, 
              0, 0, 0, 0, 1.0);
        }
      }
    }
    is.close();

    BufferedWriter outWriter = 
        new BufferedWriter(new OutputStreamWriter(
            System.out, Charset.forName("UTF-8").newEncoder()));
    r.writeResponse(teamId, outWriter);
    outWriter.flush();
  }

}
