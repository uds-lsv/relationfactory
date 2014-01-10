package evaluation;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import query.QueryList;
import query.QueryList.Query;
import util.TextIdentifier;


public class AllMatchingQueriesCandidates {
  
  static String normalize(String candidate) {
    return candidate.replaceAll("[^\\p{L}\\p{N}]", "").toLowerCase();
  }
  
  /**
   * 
   * This writes out all sentences as candidates that have matching queries or
   * query aliases.
   * Offsets can be added in a subsequent step.
   * The relation field is set to 'query'.
   * Both query and slot are the query string.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 2) {
      System.err.println("AlternateNamesCandidates <query_expanded.xml> <dtag>");
      return;
    }
    QueryList ql = new QueryList(args[0]);
    String tagFn = args[1];
    
    BufferedReader dtagBr = new BufferedReader(new FileReader(tagFn));
    TextIdentifier docQIdSnr = null;
    List<String> lines = new ArrayList<String>();

    for (String line; (line = dtagBr.readLine()) != null; ) {
      if (line.startsWith("<D")) {
        docQIdSnr = 
            TextIdentifier.fromDelimited(line.substring(3, line.length() - 1));
      } else if (line.startsWith("</D")) {
        printCandidates(lines, 
            ql.getQueryById(docQIdSnr.getQueryId()), docQIdSnr);
        docQIdSnr = null;
        lines.clear();
      } else {
        lines.add(line);
      }
    }
    dtagBr.close();
  }

  private static void printCandidates(List<String> lines,
      Query query, TextIdentifier docQIdSnr) {
    
    List<String> alternatives = new ArrayList<String>();
    if (!query.getName().equals("")) {
      alternatives.add(query.getName());
    }
    alternatives.addAll(query.getAliases());
    
    StringBuilder sb = new StringBuilder();
    String sep = "";
    for (int lineNr = 0; lineNr < lines.size(); lineNr++) {
      String[] lineParts = lines.get(lineNr).split(" ");
      sb.append(sep).append(lineParts[0]); sep = " ";
    }
    // Check for matches of the query or aliases.
    String sentence = sb.toString();
    
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
    
    String[] tokens = sentence.split(" ");
    for (int i = 0; i < matchingStartQueries.size(); ++i) {
      int startQuery = matchingStartQueries.get(i);
      int endQuery = matchingEndQueries.get(i);
      
      StringBuffer slotSb = new StringBuffer();
      sep ="";
      for (int pos = startQuery; pos < endQuery; ++pos) {
        slotSb.append(sep); sep = " ";
        slotSb.append(tokens[pos]);
      }
      String slotVal = slotSb.toString().replace('\t', ' ');
      
      System.out.println(query.getId() + "\tquery\t" + slotVal + "\t" + 
        docQIdSnr.toValidString() + "\t" + startQuery + "\t" + endQuery + 
        "\t" + startQuery + "\t" + endQuery + "\t" + 
        sentence.replace('\t', ' '));
    }
  }
}
