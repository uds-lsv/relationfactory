package run;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import query.QueryList;
import query.QueryList.Query;
import util.Candidate;
import util.TextIdentifier;


public class AlternateNamesCandidates {
  
  static String normalize(String candidate) {
    return candidate.replaceAll("[^\\p{L}\\p{N}]", "").toLowerCase();
  }
  
  /**
   * 
   * This retrieves alternate names from a tagged file that:
   * 1) are tagged according to entity type (i.e. ORGANIZATION or PERSON)
   * 2) are an expansion of the query according to Wikipedia anchor text model
   *  
   * The alternate names found are written out in candidates format, so that
   * offsets can be added in a subsequent step.
   * A response can be created from them by AllCandidatesResponse.
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
        printAlternateNamesCandidates(lines, 
            ql.getQueryById(docQIdSnr.getQueryId()), docQIdSnr);
        docQIdSnr = null;
        lines.clear();
      } else {
        lines.add(line);
      }
    }
    dtagBr.close();
  }

  private static void printAlternateNamesCandidates(List<String> lines,
      Query query, TextIdentifier docQIdSnr) {
    String entTypeLower = query.getEnttype().toLowerCase();
    if (!(entTypeLower.equals("per")
        || entTypeLower.equals("org"))) {
      return;
    }
    String entTag = entTypeLower.equals("per") ?
        "PERSON" : "ORGANIZATION";
    Set<String> aliases = new HashSet<String>();
    for (String alias : query.getAliases()) {
      // Normalized form should be one of the normalized aliases.
      // Persons require white-space, since last name is not enough.
      if (!normalize(alias).equals(normalize(query.getName())) 
          && (entTypeLower.equals("org") || alias.contains(" "))) {
        aliases.add(normalize(alias));
      }
    }
    
    int entStart = 0;
    int entEndTok = 0;
    
    int qStart = 0;
    int qEnd = 0;
    List<Integer> aliasStart = new ArrayList<Integer>();
    List<Integer> aliasEnd = new ArrayList<Integer>();
    List<String> matchedAliases = new ArrayList<String>();
    
    StringBuilder sb = new StringBuilder();
    String sep = "";
    
    String currEntity = "";
    for (int lineNr = 0; lineNr < lines.size(); ++lineNr) {
//        String line : lines) {
      String[] fields = lines.get(lineNr).split(" ");
      String tok = fields[0];
      
      sb.append(sep).append(tok);
      sep = " ";
      
      String tag = fields[1];
      if (!currEntity.isEmpty() && !tag.equals("I-" + entTag)) {
        if (currEntity.equals(query.getName())) {
          qStart = entStart;
          qEnd = entEndTok + 1;      
        } else if (aliases.contains(normalize(currEntity))) {
          aliasStart.add(entStart);
          aliasEnd.add(entEndTok + 1);
          matchedAliases.add(currEntity);
        }
        currEntity = "";
        entStart = 0;
        entEndTok = -1;
      }
      if (tag.equals("B-" + entTag)) {
        currEntity = tok;
        entStart = lineNr;
        entEndTok = lineNr;
      } else if (tag.equals("I-" + entTag)) {
        currEntity += " " + tok;
        entEndTok = lineNr;
      }
    }
    // Don't forget potential matches at the last token.
    if (!currEntity.isEmpty()) {
      if (currEntity.equals(query.getName())) {
        qStart = entStart;
        qEnd = entEndTok + 1;      
      } else if (aliases.contains(normalize(currEntity))) {
        aliasStart.add(entStart);
        aliasEnd.add(entEndTok + 1);
        matchedAliases.add(currEntity);
      }
    }
    
    String sentenceText = sb.toString();
    for (int i = 0; i < matchedAliases.size(); ++i) {
      String rel = query.getEnttype().toLowerCase() + ":alternate_names";
      String slot = matchedAliases.get(i);
      Candidate cand = new Candidate(query.getId(), rel, slot, docQIdSnr.toValidString(), qStart, qEnd, 
          aliasStart.get(i), aliasEnd.get(i), sentenceText);
      System.out.println(cand);
    }
  }
}
