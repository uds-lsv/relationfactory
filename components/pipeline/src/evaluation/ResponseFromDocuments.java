package evaluation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import query.QueryList;
import query.QueryList.Query;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;

/**
 * This creates a response file containing all answers from the returned 
 * documents. The response file is created to measure recall. It does not 
 * contain NILs.
 * 
 * @author Benjamin Roth
 *
 */
public class ResponseFromDocuments {
  static int MAX_LENGTH = 100;
  static Collection<String> responses(List<String> lines, Query q, 
      Multimap<String, String> relToArgtags, String docid) {
    List<String> responses = new ArrayList<String>();
    for (String rel : q.getRelations()) {

      
      Set<String> startTags = new HashSet<String>();
      Set<String> inTags = new HashSet<String>();
      for (String tag : relToArgtags.get(rel)) {
        startTags.add("B-" + tag);
        inTags.add("I-" + tag);
      }

      
      // Collect all tokens and matching tags.
      List<Integer> matchingStartTags = new ArrayList<Integer>();
      List<Integer> matchingEndTags = new ArrayList<Integer>();
      String[] tokens = new String[lines.size()];
      for (int lineNr = 0; lineNr < lines.size(); lineNr++) {
        String[] lineParts = lines.get(lineNr).split(" ");
        tokens[lineNr] = lineParts[0];
        if (matchingStartTags.size() != matchingEndTags.size() &&
            !inTags.contains(lineParts[1])) {
          // A tagging ends with the first non-tagged element after tag start.
          matchingEndTags.add(lineNr);
        }
        if (startTags.contains(lineParts[1])) {
          matchingStartTags.add(lineNr);
        }
      }
      // A tagging can also end at the end of a sentence.
      if (matchingStartTags.size() != matchingEndTags.size()) {
        matchingEndTags.add(lines.size());
      }
      /*
      System.err.println("---");
      System.err.println(docid);
      System.err.println(Arrays.toString(tokens));
      System.err.println(rel);
      System.err.println(startTags);
      System.err.println(inTags);
      System.err.println("Matching tag sequences: " + matchingStartTags.size());
      */
      
      if (matchingStartTags.size() != matchingEndTags.size()) {
        throw new IllegalStateException("Unexpected tagging for: " + docid);
      }
      
      // For each slot candidate, get the closest query match and print out.
      // Print out every slot candidate only once.
      Set<String> printedSlots = new HashSet<String>(matchingStartTags.size());
      for (int j = 0; j< matchingStartTags.size(); ++j) {
        StringBuffer slotSb = new StringBuffer();
        String sep = "";
        for (int pos = matchingStartTags.get(j); pos < matchingEndTags.get(j); 
            ++pos) {
          slotSb.append(sep); sep = " ";
          slotSb.append(tokens[pos]);
        }
        String slotVal = slotSb.toString().replace('\t', ' ');
        if ((rel.contains("alternate_name") && slotVal.equals(q.getName())) ||
            (rel.contains("per:alternate_name") && !slotVal.contains(" ")) ||
            printedSlots.contains(slotVal)) {
          continue;
        }
        printedSlots.add(slotVal);
        //System.err.println(slotVal);
        // SF500   org:city_of_headquarters        lsv     NYT_ENG_20070119.0341.LDC2009T13        Greenwich       0       0       0       0       1.0
        responses.add(q.getId() + "\t" + rel + "\tlsv\t" + 
            docid + "\t" + slotVal + 
            "\t0\t0\t0\t0\t1.0");
      }
    }
    return responses;
  }

  public static void main(String[] args) throws IOException {
    if (args.length != 4) {
      System.err.println("java ResponseFromDocuments <expanded_query> <rel_config> <dtag> <dscore>");
      System.err.println("expanded_query: expanded query xml.");
      System.err.println("rel_config:     file with argtag info for each relation.");
      System.err.println("dtag:           retrieved and tagged documents for all queries.");
      System.err.println("dscore:         retrieval result (ids) for all queries.");
      System.err.println("A response file is written to stdout.");
      return;
    }
    QueryList ql = new QueryList(args[0]);
    String relConfigFn = args[1];
    String tagFn = args[2];
    String dscoreFn = args[3];

    Multimap<String, String> relToArgtags = HashMultimap.create();
    BufferedReader cfgBr = new BufferedReader(new FileReader(relConfigFn));
    for (String line; (line = cfgBr.readLine()) != null;) {
      // "<relation> argtag <TAG1> <TAG2> ... <TAGn>"
      String[] parts = line.split("\\s+",3);
      if (parts[1].equals("argtag")) {
        for (String tag : parts[2].split("\\s+")) {
          relToArgtags.put(parts[0], tag);
        }        
      }
    }
    cfgBr.close();

    BufferedReader dtagBr = new BufferedReader(new FileReader(tagFn));
    BufferedReader dscoreBr = new BufferedReader(new FileReader(dscoreFn));
    
    String sentenceId = "";
    List<String> lines = new ArrayList<String>();
    
    String currDscoreDoc = "";
    int lastSnr = 0;
    String dscoreQid = "";
    Set<String> responseLines = new HashSet<String>();
//    List<String> responseLines = new ArrayList<String>();

    // This processes the dscore doc retrieval, the queries and the sentences
    // in parallel.
    // The logic is: stay with the same query as long as the tagged sentences 
    // belong to the same document, and sentence numbers are going up.
    // If sentence numbers are not going up, or doc ids change, read the next
    // line of the dscore file and go to the corresponding query.
    for (String line; (line = dtagBr.readLine()) != null; ) {
      if (line.startsWith("<D")) {
        sentenceId = line.substring(3, line.length() - 1);
        int lastDotIdx = sentenceId.lastIndexOf('.');
        String docid = sentenceId.substring(0, lastDotIdx);
        int sNr = Integer.parseInt(sentenceId.substring(lastDotIdx+1));
        
        if (!docid.equals(currDscoreDoc) || sNr <= lastSnr) {

          
          // This needs to be done once even if doc id matches ...
          do {
            String[] fields = dscoreBr.readLine().split(" ");
            dscoreQid = fields[0];
            currDscoreDoc = fields[2].split(":")[1];
          } while (!docid.equals(currDscoreDoc));
          // ... we just go to the next doc matching.
          
          // The query corresponding to dscore entry.
/*          while (!ql.getQueries().get(currQnr).getId().equals(dscoreQid)) {
            currQnr += 1;
          }*/
        }
        lastSnr = sNr;
      } else if (line.startsWith("</D")) {
        if (lines.size() <= MAX_LENGTH) {
          int lastDotIdx = sentenceId.lastIndexOf('.');
          String docid = sentenceId.substring(0, lastDotIdx);
          for (String rl : responses(lines, ql.getQueryById(dscoreQid), relToArgtags, 
              docid)) {
            if (!responseLines.contains(rl)) {
              System.out.println(rl);
              responseLines.add(rl);
            }
          }
        }
        sentenceId = "";
        lines.clear();
      } else {
        lines.add(line);
      }
    }
    dtagBr.close();
    dscoreBr.close();
  }
}
