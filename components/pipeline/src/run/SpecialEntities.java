package run;

import matcher.TagTokenSeq;

import java.io.*;
import java.util.*;

/**
 * Created by beroth on 7/29/15.
 */
/*
Replaces the instable "specialents" from this command:
paste -d' ' $1.conll.tmp $1.sequor.tmp > $1.pasted |\
           $TAC_ROOT/components/ner/bin/specialents \
           $SPECIALENT/JOB_TITLE $SPECIALENT/NORP:RELIGION\
           $SPECIALENT/CAUSE_DEATH $SPECIALENT/CHARGES > $1.ent.tmp
This script uses dtag format.
**/

public class SpecialEntities {
  static void printMatches(List<String> lines, String sentenceId,
                             Map<String, List<TagTokenSeq>> prefixTokenToTagSeqs) {
    // 1. Build up auxiliary data structures.
    String[] tags = new String[lines.size()];
    String[] tokens = new String[lines.size()];
    int[] startInds = new int[lines.size() + 1];
    startInds[0] = 0;
    StringBuffer tokenBr = new StringBuffer();

    for (int i = 0; i < lines.size(); i++) {
      String line = lines.get(i);
      String[] parts = line.split(" ");
      if (parts.length != 2) {
        System.err.println("Illegal line: " + line);
        parts = new String[]{"","O"};
      }
      tokens[i] = parts[0];
      tokenBr.append(parts[0] + " ");
      startInds[i+1] = startInds[i] + parts[0].length() + 1;
      tags[i] = parts[1];
    }
    String tokensCat = tokenBr.toString();

    // 2. Annotate tags.
    for (int i = 0; i < lines.size(); i++) {
      String tok = tokens[i];
      if (tags[i].equals("O") && prefixTokenToTagSeqs.containsKey(tok)) {
        for (TagTokenSeq tts : prefixTokenToTagSeqs.get(tok)) {
          if (startInds.length > i + tts.numTokens) {
            int tokSeqStart = startInds[i];
            int tokSeqEnd = startInds[i + tts.numTokens];
            if (tokensCat.substring(tokSeqStart, tokSeqEnd).equals(tts.concatenated)) {
              // We have a match. Annotate tag.
              boolean isFirst = true;
              for (int j=i; j < i + tts.numTokens; ++j) {
                String tag;
                if (isFirst) {
                  tag = "B-" + tts.seqTag;
                  isFirst = false;
                } else {
                  tag = "I-" + tts.seqTag;
                }
                tags[j] = tag;
              }
              continue;
            }
          }
        }
      }
    }

    // 3. Write out new dtag.
    System.out.println("<D="+sentenceId+">");
    for (int i = 0; i < lines.size(); i++) {
      System.out.println(tokens[i] + " " + tags[i]);
    }
    System.out.println("</D>");
  }

  public static void main(String[] args) throws IOException {

    Map<String, List<TagTokenSeq>> prefixToTags = new HashMap<String, List<TagTokenSeq>>();

    // Read mapping tokens <-> tags.
    // For faster matching, anchor sequences with first token.
    for (String tagMapFN: args) {
      File tagMapFile = new File(tagMapFN);
      // Files for tags must have the same name as the tag.
      String tag = tagMapFile.getName();
      BufferedReader tagMapBr = new BufferedReader(new InputStreamReader(new FileInputStream(tagMapFile), "UTF-8"));
      for (String line; (line = tagMapBr.readLine()) != null; ) {
        String[] parts = line.split(" ");
        String firstToken = parts[0];
        String concatenated = line + " ";
        int numTokens = parts.length;
        TagTokenSeq tts = new TagTokenSeq();
        tts.concatenated = concatenated;
        tts.numTokens = numTokens;
        tts.seqTag = tag;
        List<TagTokenSeq> ttsList;
        if (prefixToTags.containsKey(firstToken)) {
          ttsList = prefixToTags.get(firstToken);
        } else {
          ttsList = new ArrayList<TagTokenSeq>();
        }
        ttsList.add(tts);
        prefixToTags.put(firstToken, ttsList);
      }
      tagMapBr.close();
    }

    BufferedReader dtagBr = new BufferedReader(new InputStreamReader(System.in));

    //TextIdentifier docQIdSnr = null;
    String docQIdSnr = null;

    List<String> lines = new ArrayList<String>();

    for (String line; (line = dtagBr.readLine()) != null; ) {
      if (line.startsWith("<D")) {
        docQIdSnr = line.substring(3, line.length() - 1);
        //docQIdSnr =
        //    TextIdentifier.fromDelimited(line.substring(3, line.length() - 1));
      } else if (line.startsWith("</D")) {
          printMatches(lines, docQIdSnr, prefixToTags);
        docQIdSnr = null;
        lines.clear();
      } else {
        lines.add(line);
      }
    }
    dtagBr.close();
  }
}
