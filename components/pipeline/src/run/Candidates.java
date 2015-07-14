package run;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Logger;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;

import query.QueryList;
import query.QueryList.Query;
import util.TextIdentifier;

public class Candidates {
  static int MAX_LENGTH = 100;
  static Logger logger = Logger.getLogger(Candidates.class.getName());
  
 
  static void printInstances(List<String> lines, Query q, 
      Multimap<String, String> relToArgtags, String sentenceId) {
    
    List<String> alternatives = new ArrayList<String>();
    if (!q.getName().equals("")) {
      alternatives.add(q.getName());
    }
    alternatives.addAll(q.getAliases());
    
    StringBuilder sb = new StringBuilder();
    String sep = "";
    for (int lineNr = 0; lineNr < lines.size(); lineNr++) {
      String[] lineParts = lines.get(lineNr).split(" ");
      sb.append(sep).append(lineParts[0]); sep = " ";
    }
    // Check for matches of the query or aliases.
    String sentence = sb.toString();
    String[] tokens = sentence.split(" ");
    if (tokens.length != lines.size()) {
      logger.error("Unexpected tokenization, Ignoring: " + sentence);
      return;
    }
    List<Integer> matchingStartQueries = new ArrayList<Integer>();
    List<Integer> matchingEndQueries = new ArrayList<Integer>();
    int wNr = 0;
    for (int i = 0; i < sentence.length(); ++i) {
      if (i == 0 || sentence.charAt(i-1) == ' ') {
        int endArg = -1;
        for (String al : alternatives) {
          if ((sentence + " ").substring(i).startsWith(al + " ")) {
            int currEndArg = wNr + al.split(" ").length;
            if (currEndArg > endArg) {
              endArg = currEndArg;
            }
          }
        }
        int numArgs = matchingEndQueries.size();
        if (endArg > -1 && 
            (numArgs == 0 || endArg > matchingEndQueries.get(numArgs - 1))) {
          matchingStartQueries.add(wNr);
          matchingEndQueries.add(endArg);
        }
        wNr += 1;
      }
    }
    
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
            
      for (int lineNr = 0; lineNr < lines.size(); lineNr++) {
        String[] lineParts = lines.get(lineNr).split(" ");
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
      
      if (matchingStartTags.size() != matchingEndTags.size()) {
        throw new IllegalStateException("Unexpected tagging for: " + sentenceId);
      }
      
      // For each slot candidate, get the closest query match and print out.
      // Print out every slot candidate only once.
      Set<String> printedSlots = new HashSet<String>(matchingStartTags.size());
      for (int j = 0; j< matchingStartTags.size(); ++j) {
        int absDist = Integer.MAX_VALUE;
        int closestStartTag = -1;
        int closestEndTag = -1;
        int closestStartQuery = -1;
        int closestEndQuery = -1;
        for (int i = 0; i < matchingStartQueries.size(); ++i) {
          if (matchingEndQueries.get(i) <= matchingStartTags.get(j)) {
            int currAbsDist = Math.abs(matchingStartTags.get(j) - matchingEndQueries.get(i));
            if (currAbsDist < absDist) {
              absDist = currAbsDist;
              closestStartTag = matchingStartTags.get(j);
              closestEndTag = matchingEndTags.get(j);
              closestStartQuery = matchingStartQueries.get(i);
              closestEndQuery = matchingEndQueries.get(i);
            }
          } else if (matchingEndTags.get(j) <= matchingStartQueries.get(i)) {
            int currAbsDist = Math.abs(matchingStartQueries.get(i) - matchingEndTags.get(j));
            if (currAbsDist < absDist) {
              absDist = currAbsDist;
              closestStartTag = matchingStartTags.get(j);
              closestEndTag = matchingEndTags.get(j);
              closestStartQuery = matchingStartQueries.get(i);
              closestEndQuery = matchingEndQueries.get(i);
            }              
          }
        }
        if (closestStartTag > -1) {
          StringBuffer slotSb = new StringBuffer();
          sep ="";
          for (int pos = closestStartTag; pos < closestEndTag; ++pos) {
            slotSb.append(sep); sep = " ";
            slotSb.append(tokens[pos]);
          }
          String slotVal = slotSb.toString().replace('\t', ' ');
          if ((rel.contains("alternate_name") && slotVal.equals(q.getName())) ||
              (rel.contains("per:alternate_name") && !slotVal.contains(" ")) ||
              printedSlots.contains(slotVal) || 
              q.getIgnoreSlotfillers().get(rel).contains(slotVal)) {
            continue;
          }
          printedSlots.add(slotVal);
          System.out.println(q.getId() + "\t" + rel + "\t" + slotVal + "\t" + 
              sentenceId + "\t" + closestStartQuery + "\t" + closestEndQuery + 
              "\t" + closestStartTag + "\t" + closestEndTag + "\t" + 
              sentence.replace('\t', ' '));
        }
      }
    }
  }
  
  /**
   * This produces candidates for the different relations according to type
   * tags and query name/alias occurrence.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 3) {
      System.err.println("java Candidates <expanded_query> <rel_config> <dtag>");
      System.err.println("expanded_query: expanded query xml.");
      System.err.println("rel_config:     file with argtag info for each relation.");
      System.err.println("dtag:           retrieved and tagged documents for all queries.");
      System.err.println("A candidates file is written to stdout.");
      return;
    }
    QueryList ql = new QueryList(args[0]);
    
    if (ql.getQueries().size() == 0) {
      logger.fatal("No queries read in query.xml!");
      return;
    }
    
    String relConfigFn = args[1];
    String tagFn = args[2];

    Multimap<String, String> relToArgtags = HashMultimap.create();
    BufferedReader cfgBr = new BufferedReader(new FileReader(relConfigFn));
    for (String line; (line = cfgBr.readLine()) != null;) {
      if (line.isEmpty() || line.startsWith("#")) {
        continue;
      }
      // "<relation> argtag <TAG1> <TAG2> ... <TAGn>"
      String[] parts = line.split("\\s+",3);
      if (parts[1].equals("argtag")) {
        for (String tag : parts[2].split("\\s+")) {
          relToArgtags.put(parts[0], tag);
        }        
      }
    }
    cfgBr.close();

    BufferedReader dtagBr = new BufferedReader(new InputStreamReader(new FileInputStream(tagFn), "UTF-8"));
    
    TextIdentifier docQIdSnr = null;
    List<String> lines = new ArrayList<String>();

    // Stay with the same query as long as the tagged sentences 
    // belong to the same document, and sentence numbers are going up.
    // If sentence numbers are not going up, or doc ids change, read the next
    // line of the dscore file and go to the corresponding query.
    for (String line; (line = dtagBr.readLine()) != null; ) {
      if (line.startsWith("<D")) {
        docQIdSnr = 
            TextIdentifier.fromDelimited(line.substring(3, line.length() - 1));
      } else if (line.startsWith("</D")) {
        if (lines.size() <= MAX_LENGTH) {
          printInstances(lines, ql.getQueryById(docQIdSnr.getQueryId()), 
              relToArgtags, docQIdSnr.toValidString());
        }
        docQIdSnr = null;
        lines.clear();
      } else {
        lines.add(line);
      }
    }
    dtagBr.close();
  }
}
