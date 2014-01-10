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

import parser.DependencyTree;
import query.QueryList;
import rerac.protos.Corpus.Document;
import util.DocumentExtractor;
import util.Responses;



public class DependencyPatternResponse {

  /**
   * Writes out a response for matching sysntactic patterns, taking sentences
   * with parse annotations.
   * 
   * No redundancy elimination whatsoever is done, so literally same slots may
   * be written out. Therefore, RedundancyEliminator needs to be called in order
   * to obtain a final response.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 4) {
      System.err.println("DependencyPatternResponse " +
          "<query_expanded_xml> <sentences> <prise patterns> <team_id>");
      return;
    }
    
    // Copied over from PatternResponse (tbarth)
    QueryList ql = new QueryList(args[0]);
    String sentenceFn = args[1];
    String patternsFn = args[2];
    String teamId = args[3];

    Map<String, List<String>> relToPatterns = 
        new HashMap<String, List<String>>();

    BufferedReader br = new BufferedReader(new FileReader(patternsFn));
    for (String line; (line = br.readLine()) != null; ) {
      line = line.trim();
      if (!line.startsWith("#") && !line.isEmpty()) {
        String[] lineParts = line.split("\\s+", 2);
        String rel = lineParts[0];
        String pat = lineParts[1].trim();
        if (!relToPatterns.containsKey(rel)) {
          relToPatterns.put(rel, new ArrayList<String>());
        }
        relToPatterns.get(rel).add(pat);
      }
    }
    br.close();

    Responses r = new Responses(ql);
    
    BufferedInputStream is = new BufferedInputStream(new FileInputStream(
        sentenceFn));
    for (Document sentence; 
        (sentence = Document.parseDelimitedFrom(is)) != null;) {
      String rel = DocumentExtractor.relations(sentence).get(0);
      DependencyTree tree = new DependencyTree(sentence);
      List<String> patterns = relToPatterns.get(rel);
      if (patterns == null) {
        continue;
      }

      int idxArg1Start = DocumentExtractor.getArgStart(sentence, rel, 0);
      int idxArg1End = DocumentExtractor.getArgEnd(sentence, rel, 0);
      int idxArg1 = tree.getHead(idxArg1Start, idxArg1End);
      
      int idxArg2Start = DocumentExtractor.getArgStart(sentence, rel, 1);
      int idxArg2End = DocumentExtractor.getArgEnd(sentence, rel, 1);
      int idxArg2 = tree.getHead(idxArg2Start, idxArg2End);
      
      String shortestPath = tree.patternPathString(idxArg1, idxArg2, 
          sentence.getTokenList());
      String slot = DocumentExtractor.textFromTokens(sentence, idxArg2Start, 
          idxArg2End);
      
      //String docid = sentence.getId().replaceFirst("\\.[^.]+$", "");
      String qid = DocumentExtractor.canonicalArg(sentence, 0, rel);
      
      for (String pattern : patterns) {
        // TODO(tbarth): Use somePatternMatcher implementation here?
        if (pattern.equals(shortestPath)) {
          r.addResponse2012(qid, rel, teamId, sentence.getId(), slot, 0, 0, 0, 0, 1.0);
        }
      }
    }
    is.close();

    BufferedWriter outWriter = new BufferedWriter(
        new OutputStreamWriter(System.out, Charset.forName("UTF-8").newEncoder()));
    r.writeResponse(teamId, outWriter);
    outWriter.flush();
  }

}
