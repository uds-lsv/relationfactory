package run;

import org.apache.log4j.Logger;
import query.QueryList;
import query.QueryList.Query;
import util.TextIdentifier;

import java.io.*;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class QueryContexts {
  static int MAX_LENGTH = 100;
  static int WINDOW_SIZE = 40;
  static String QUERY_WILDCARD = "<QUERY>";
  static Logger logger = Logger.getLogger(QueryContexts.class.getName());

  static void printInstances(List<String> lines, Query q, String sentenceId) {
    
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
      for (int qMatch = 0; qMatch < matchingStartQueries.size(); ++qMatch) {
        int qStart = matchingStartQueries.get(qMatch);
        int qEnd = matchingEndQueries.get(qMatch);
        int contextStart = 0;
        if (qEnd > QueryContexts.WINDOW_SIZE) {
          contextStart = Math.min(qEnd + QueryContexts.WINDOW_SIZE/2, tokens.length) - QueryContexts.WINDOW_SIZE;
        }
        StringBuffer contextSb = new StringBuffer();
        sep ="";
        for (int pos = contextStart; pos < tokens.length; ++pos) {
          contextSb.append(sep); sep = " ";
          if (pos == qStart) {
            contextSb.append(QueryContexts.QUERY_WILDCARD);
            pos = qEnd - 1;
          } else {
            contextSb.append(tokens[pos]);
          }
        }
        String context = contextSb.toString().replace('\t', ' ');
        System.out.println(context + "\t" + sentenceId + "\t" + rel + "\t</s>");
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
      System.err.println("java QueryContexts <expanded_query> <rel_config> <dscore>");
      System.err.println("expanded_query: expanded query xml.");
      System.err.println("rel_config:     file with argtag info for each relation.");
      System.err.println("dtag:           retrieved (untagged) documents for all queries.");
      System.err.println("A contexts file is written to stdout.");
      return;
    }
    QueryList ql = new QueryList(args[0]);
    
    if (ql.getQueries().size() == 0) {
      logger.fatal("No queries read in query.xml!");
      return;
    }
    

    String docFn = args[2];
    BufferedReader docBr = new BufferedReader(new InputStreamReader(new FileInputStream(docFn), "UTF-8"));
    
    TextIdentifier docQIdSnr = null;
    List<String> lines = new ArrayList<String>();

    // Stay with the same query as long as the sentences
    // belong to the same document, and sentence numbers are going up.
    // If sentence numbers are not going up, or doc ids change, read the next
    // line of the doc file and go to the corresponding query.
    for (String line; (line = docBr.readLine()) != null; ) {
      if (line.startsWith("<D")) {
        docQIdSnr = 
            TextIdentifier.fromDelimited(line.substring(3, line.length() - 1));
      } else if (line.startsWith("</D")) {
        if (lines.size() <= MAX_LENGTH) {
          printInstances(lines, ql.getQueryById(docQIdSnr.getQueryId()), docQIdSnr.toValidString());
        }
        docQIdSnr = null;
        lines.clear();
      } else {
        lines.add(line);
      }
    }
    docBr.close();
  }
}
