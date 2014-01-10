package training;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import query.QueryList;
import query.QueryList.Query;
import util.TextIdentifier;

public class DistsupCandidates {
  static int MAX_LENGTH = 100;
  static void printInstances(List<String> lines, Query q, String sentenceId, 
      boolean matchAll) {
    List<String> queryAlternatives = new ArrayList<String>();
    if (!q.getName().equals("")) {
      queryAlternatives.add(q.getName());
    }
    queryAlternatives.addAll(q.getAliases());
    
    for (String rel : q.getIgnoreSlotfillers().keySet()) {
      Set<String> slotfillers = 
          new HashSet<String>(q.getIgnoreSlotfillers().get(rel));
      // Collect all tokens.
      StringBuilder sb = new StringBuilder();
      String sep = "";
      for (String line : lines) {
        // For compatibility with tagged files, split and take 1st column.
        String[] lineParts = line.split(" ");
        sb.append(sep).append(lineParts[0]); sep = " ";
      }
      String sentence = sb.toString();
      
      List<Integer> matchingStartSlots = new ArrayList<Integer>();
      List<Integer> matchingEndSlots = new ArrayList<Integer>();
      for (int i = 0, wNr = 0; i < sentence.length(); ++i) {
        if (i == 0 || sentence.charAt(i-1) == ' ') {
          int endArg = -1;
          for (String al : slotfillers) {
            if ((sentence + " ").substring(i).startsWith(al + " ")) {
              int currEndArg = wNr + al.split(" ").length;
              if (currEndArg > endArg) {
                endArg = currEndArg;
              }
            }
          }
          int numArgs = matchingEndSlots.size();
          if (endArg > -1 && 
              (numArgs == 0 || endArg > matchingEndSlots.get(numArgs - 1))) {
            matchingStartSlots.add(wNr);
            matchingEndSlots.add(endArg);
          }
          wNr += 1;
        }
      }
      
      // Check for matches of the query or aliases.      
      List<Integer> matchingStartQueries = new ArrayList<Integer>();
      List<Integer> matchingEndQueries = new ArrayList<Integer>();
      for (int i = 0, wNr = 0; i < sentence.length(); ++i) {
        if (i == 0 || sentence.charAt(i-1) == ' ') {
          int endArg = -1;
          for (String al : queryAlternatives) {
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
      
      // For each slot candidate, get the closest query match and print out.
      // Print out every slot candidate only once.
      String[] tokens = sentence.split(" ");
      Set<String> printedSlots = new HashSet<String>(matchingStartSlots.size());
      for (int j = 0; j< matchingStartSlots.size(); ++j) {
        int absDist = Integer.MAX_VALUE;
        int closestStartTag = -1;
        int closestEndTag = -1;
        int closestStartQuery = -1;
        int closestEndQuery = -1;
        for (int i = 0; i < matchingStartQueries.size(); ++i) {
          
          // Return all pairs, not only closest matching ones.
          if (matchAll) {
            StringBuffer slotSb = new StringBuffer();
            sep ="";
            int startTag = matchingStartSlots.get(j);
            int endTag = matchingEndSlots.get(j);
            int startQuery = matchingStartQueries.get(i);
            int endQuery = matchingEndQueries.get(i);
            for (int pos = startTag; pos < endTag; ++pos) {
              slotSb.append(sep); sep = " ";
              slotSb.append(tokens[pos]);
            }
            String slotVal = slotSb.toString().replace('\t', ' ');
            System.out.println(q.getId() + "\t" + rel + "\t" + slotVal + "\t" + 
                sentenceId + "\t" + startQuery + "\t" + endQuery + 
                "\t" + startTag + "\t" + endTag + "\t" + 
                sentence.replace('\t', ' '));
          }
          
          if (matchingEndQueries.get(i) <= matchingStartSlots.get(j)) {
            int currAbsDist = Math.abs(matchingStartSlots.get(j) - matchingEndQueries.get(i));
            if (currAbsDist < absDist) {
              absDist = currAbsDist;
              closestStartTag = matchingStartSlots.get(j);
              closestEndTag = matchingEndSlots.get(j);
              closestStartQuery = matchingStartQueries.get(i);
              closestEndQuery = matchingEndQueries.get(i);
            }
          } else if (matchingEndSlots.get(j) <= matchingStartQueries.get(i)) {
            int currAbsDist = Math.abs(matchingStartQueries.get(i) - matchingEndSlots.get(j));
            if (currAbsDist < absDist) {
              absDist = currAbsDist;
              closestStartTag = matchingStartSlots.get(j);
              closestEndTag = matchingEndSlots.get(j);
              closestStartQuery = matchingStartQueries.get(i);
              closestEndQuery = matchingEndQueries.get(i);
            }              
          }
        }
        
        // Print closest match.
        if (!matchAll && closestStartTag > -1) {
          StringBuffer slotSb = new StringBuffer();
          sep ="";
          for (int pos = closestStartTag; pos < closestEndTag; ++pos) {
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
          System.out.println(q.getId() + "\t" + rel + "\t" + slotVal + "\t" + 
              sentenceId + "\t" + closestStartQuery + "\t" + closestEndQuery + 
              "\t" + closestStartTag + "\t" + closestEndTag + "\t" + 
              sentence.replace('\t', ' '));
        }
      }
    }
  }
  
  /**
   * This produces distant supervision candidates from retrieval results.
   * The known slot fillers are written into the 'ignore_slotfiller' field.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 4) {
      System.err.println("java Candidates <distsup_query> <drank> <dscore>  <all=true|false>");
      System.err.println("distsup_query:  distant supervision query xml with known pairs.");
      System.err.println("drank:          retrieved (not tagged) documents for all queries.");
      System.err.println("dscore:         retrieval result (ids) for all queries.");
      System.err.println("all:            whether all pairs are taken or only closest.");
      System.err.println("A candidates file is written to stdout.");
      return;
    }
    QueryList ql = new QueryList(args[0]);
    System.err.println("Query list contains " + ql.getQueries().size() + " queries.");
    String textFn = args[1];
    String dscoreFn = args[2];
    boolean matchAll = args[3].equals("true");

    BufferedReader dtextBr = new BufferedReader(new FileReader(textFn));
    BufferedReader dscoreBr = new BufferedReader(new FileReader(dscoreFn));
    
    TextIdentifier sentenceId = null;
    List<String> lines = new ArrayList<String>();
    
    String currDscoreDoc = "";
    int lastSnr = 0;

    // This processes the dscore doc retrieval, the queries and the sentences
    // in parallel.
    // The logic is: stay with the same query as long as the sentences 
    // belong to the same document, and sentence numbers are going up.
    // If sentence numbers are not going up, or doc ids change, read the next
    // line of the dscore file and go to the corresponding query.
    for (String line; (line = dtextBr.readLine()) != null; ) {

      if (line.startsWith("<D")) {
        sentenceId = 
            TextIdentifier.fromDelimited(line.substring(3, line.length() - 1));
      } else if (line.startsWith("</D")) {
        if (lines.size() <= MAX_LENGTH) {
          printInstances(lines, ql.getQueryById(sentenceId.getQueryId()), 
              sentenceId.toValidString(), matchAll);
        }
        sentenceId = null;
        lines.clear();
      } else {
        lines.add(line);
      }
    }
    dtextBr.close();
    dscoreBr.close();
  }
}
