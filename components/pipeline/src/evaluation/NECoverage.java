package evaluation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multiset;

public class NECoverage {
  /**
   * Statistics about coverage of named-entity tagging.
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 3) {
      System.err.println("java NECoverage <rel_config> <dtag> <key>");
      System.err.println("rel_config: info about relations (and their tags).");
      System.err.println("dtag:       tagged documents.");
      System.err.println("key:        keyfile to match slot values with.");
      System.err.println("The per-relation coverage statistics are written to stdout.");
      return;
    }
    String relConfigFn = args[0];
    String tagFn = args[1];
    String keyFn = args[2];
    
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

    Multimap<String, String> docidToKeyLines = HashMultimap.create();
    BufferedReader br = new BufferedReader(new FileReader(keyFn));
    for (String line; (line = br.readLine()) != null;) {
      // 22 SF500:org:alternate_names APW_ENG_20070813.0797.LDC2009T13 3 0 LLC
      String[] parts = line.split(" ");
      if (parts[3].equals("1")) {
        String docid = parts[2];
        docidToKeyLines.put(docid, line);
      }
    }
    br.close();
    
    br = new BufferedReader(new FileReader(tagFn));
    StringBuffer docBuffer = new StringBuffer();
    StringBuffer tagBuffer = new StringBuffer();
    Multiset<String> numSlotsPerRel = HashMultiset.create();
    Multiset<String> numCorrectSlotsPerRel = HashMultiset.create();
    
    String currDocid = "";
    for (String line; (line = br.readLine()) != null; ) {
      if (line.startsWith("<D")) {
        String sentenceId = line.substring(3, line.length() - 1);
        int lastDotIdx = sentenceId.lastIndexOf('.');
        String docid = sentenceId.substring(0, lastDotIdx);
        if (!docid.equals(currDocid)) {
          String docStr = docBuffer.toString();
          docBuffer = new StringBuffer();
          String tagStr = tagBuffer.toString();
          tagBuffer = new StringBuffer();
          
          for (String keyLine : docidToKeyLines.get(currDocid)) {
            // 22 SF500:org:alternate_names APW_ENG_20070813.0797.LDC2009T13 3 0 LLC
            String[] klParts = keyLine.split(" ", 6);
            String slot = " " + klParts[5] + " ";
            String rel = klParts[1].split(":", 2)[1];
            if (docStr.contains(slot)) {
              numSlotsPerRel.add(rel);
              boolean correctlyTagged = false;

              for (String tag : relToArgtags.get(rel)) {
                String taggedSlot = tagSlot(slot, tag);
                if (tagStr.contains(taggedSlot)) {
                  correctlyTagged = true;
                }
              }
              if (correctlyTagged) {
                numCorrectSlotsPerRel.add(rel);
              }
            }
          }
          currDocid = docid;
        }
      } else if (line.startsWith("</D")) {
        docBuffer.append(" ");
        tagBuffer.append(" ");
      } else {
        String word = line.split(" ")[0];
        String tag = line.split(" ")[1];
        docBuffer.append(" ").append(word);
        tagBuffer.append(" ").append(word).append(":").append(tag);
      }
    }
    br.close();
  }

  private static String tagSlot(String slot, String tag) {
    StringBuffer slotSb = new StringBuffer();
    String sep = "";
    String tagPrefix = "B-";
    for (String word : slot.split(" ")) {
      slotSb
        .append(sep)
        .append(word)
        .append(":")
        .append(tagPrefix)
        .append(tag);
      sep = " ";
      tagPrefix = "I-";
    }
    return slotSb.toString();
  }
}
