package training;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class TitleOrgContexts {
  static int MAX_LENGTH = 100;
  static void printContexts(List<String> lines, String sentenceId) {
    String titleStartTag = "B-JOB_TITLE";
    String titleInTag = "I-JOB_TITLE";
    String orgStartTag = "B-ORGANIZATION";
    String orgInTag = "I-ORGANIZATION";
    
    // Collect all tokens and matching tags.
    List<Integer> matchingStartTitles = new ArrayList<Integer>();
    List<Integer> matchingEndTitles = new ArrayList<Integer>();

    List<Integer> matchingStartOrgs = new ArrayList<Integer>();
    List<Integer> matchingEndOrgs = new ArrayList<Integer>();

    StringBuilder sb = new StringBuilder();
    String sep = "";
    for (int lineNr = 0; lineNr < lines.size(); lineNr++) {
      String[] lineParts = lines.get(lineNr).split(" ");
      sb.append(sep).append(lineParts[0]); sep = " ";
      
      // Match titles.
      if (matchingStartTitles.size() != matchingEndTitles.size() &&
          !titleInTag.equals(lineParts[1])) {
        // A tagging ends with the first non-tagged element after tag start.
        matchingEndTitles.add(lineNr);
      }
      if (titleStartTag.equals(lineParts[1])) {
        matchingStartTitles.add(lineNr);
      }
      
      // Match organizations
      if (matchingStartOrgs.size() != matchingEndOrgs.size() &&
          !orgInTag.equals(lineParts[1])) {
        // A tagging ends with the first non-tagged element after tag start.
        matchingEndOrgs.add(lineNr);
      }
      if (orgStartTag.equals(lineParts[1])) {
        matchingStartOrgs.add(lineNr);
      }
    }
    // A tagging can also end at the end of a sentence.
    if (matchingStartTitles.size() != matchingEndTitles.size()) {
      matchingEndTitles.add(lines.size());
    }
    if (matchingStartOrgs.size() != matchingEndOrgs.size()) {
      matchingEndOrgs.add(lines.size());
    }

    
    if (matchingStartTitles.size() != matchingEndTitles.size() 
        || matchingStartOrgs.size() != matchingEndOrgs.size()) {
      throw new IllegalStateException("Unexpected tagging for: " + sentenceId);
    }
    
    String sentence = sb.toString();
    String[] tokens = sentence.split(" ");

    //Set<String> printedSlots = new HashSet<String>(matchingStartTags.size());
    
    for (int j = 0; j< matchingStartTitles.size(); ++j) {
      int absDist = Integer.MAX_VALUE;
      int closestStartTitle = -1;
      int closestEndTitle = -1;
      int closestStartOrg = -1;
      int closestEndOrg = -1;
      for (int i = 0; i < matchingStartOrgs.size(); ++i) {
        if (matchingEndOrgs.get(i) <= matchingStartTitles.get(j)) {
          int currAbsDist = Math.abs(matchingStartTitles.get(j) - matchingEndOrgs.get(i));
          if (currAbsDist < absDist) {
            absDist = currAbsDist;
            closestStartTitle = matchingStartTitles.get(j);
            closestEndTitle = matchingEndTitles.get(j);
            closestStartOrg = matchingStartOrgs.get(i);
            closestEndOrg = matchingEndOrgs.get(i);
          }
        } else if (matchingEndTitles.get(j) <= matchingStartOrgs.get(i)) {
          int currAbsDist = Math.abs(matchingStartOrgs.get(i) - matchingEndTitles.get(j));
          if (currAbsDist < absDist) {
            absDist = currAbsDist;
            closestStartTitle = matchingStartTitles.get(j);
            closestEndTitle = matchingEndTitles.get(j);
            closestStartOrg = matchingStartOrgs.get(i);
            closestEndOrg = matchingEndOrgs.get(i);
          }              
        }
      }
      if (closestStartTitle > -1) {
        StringBuffer slotSb = new StringBuffer();
        sep ="";
        for (int pos = closestStartTitle; pos < closestEndTitle; ++pos) {
          slotSb.append(sep); sep = " ";
          slotSb.append(tokens[pos]);
        }
        String slotVal = slotSb.toString().replace('\t', ' ');
        String candLine = "ORG\torg:position\t" + slotVal + "\t" + 
            sentenceId + "\t" + closestStartOrg + "\t" + closestEndOrg + 
            "\t" + closestStartTitle + "\t" + closestEndTitle + "\t" + 
            sentence.replace('\t', ' ');
        String pattern = PatternMetric.patternFromLine(candLine);
        System.out.println(pattern);
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
    if (args.length != 1) {
      System.err.println("java TitleOrgContexts <dtag>");
      System.err.println("dtag:           retrieved and tagged documents for all queries.");
      System.err.println("Contexts written to stdout.");
      return;
    }
    String tagFn = args[0];
    BufferedReader dtagBr = new BufferedReader(new FileReader(tagFn));
    String sentenceId = "";
    List<String> lines = new ArrayList<String>();

    // This processes the dscore doc retrieval, the queries and the sentences
    // in parallel.
    // The logic is: stay with the same query as long as the tagged sentences 
    // belong to the same document, and sentence numbers are going up.
    // If sentence numbers are not going up, or doc ids change, read the next
    // line of the dscore file and go to the corresponding query.
    for (String line; (line = dtagBr.readLine()) != null; ) {
      if (line.startsWith("<D")) {
        sentenceId = line.substring(3, line.length() - 1);
      } else if (line.startsWith("</D")) {
        if (lines.size() <= MAX_LENGTH) {
          printContexts(lines, sentenceId);
        }
        sentenceId = "";
        lines.clear();
      } else {
        lines.add(line);
      }
    }
    dtagBr.close();
  }
}
