package entity_expansion;


import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * This expands en entity according to co-occurrence statics in a Wikipedia 
 * dump.
 * It is memory friendly thanks to hashing.
 * 
 * see run.Expand for an example usage.
 * 
 * @author Benjamin Roth
 *
 */
public class MaxLinkEntityExpander {
  Map<Integer, Integer> textHashToMaxTargetHash = new HashMap<Integer, Integer>();
  Map<Integer, String> targetHashToMaxAnchor = new HashMap<Integer, String>();
  Map<Integer, String> targetHashToSecondMaxAnchor = new HashMap<Integer, String>();
  
  private static final int MINCOUNT = 2;
  
  private static final Pattern SEPARATOR_PATTERN = Pattern.compile(" ");
  
  public MaxLinkEntityExpander(String LinkStatisticsFn) throws IOException {
    Map<Integer, Integer> textHashToMaxCount = new HashMap<Integer, Integer>();
    
    // First read mapping from text to articles. Then, for reachable articles
    // establish mapping to most frequent anchor text. (Saves memory).
    BufferedReader br = new BufferedReader(new FileReader(LinkStatisticsFn));
    for (String line; (line = br.readLine()) != null;) {
      String[] lineParts = SEPARATOR_PATTERN.split(line, 3);
      if (lineParts.length != 3) {
        continue;
      }
      int count = Integer.parseInt(lineParts[0]);
      if (count < MINCOUNT) {
        continue;
      }
      Integer targetHash = lineParts[1].hashCode();
      int textHash =  lineParts[2].hashCode();
      if (!textHashToMaxCount.containsKey(textHash) ||
          textHashToMaxCount.get(textHash) < count) {
        textHashToMaxTargetHash.put(textHash, targetHash);
        textHashToMaxCount.put(textHash, count);
      }
    }
    br.close();
    br = new BufferedReader(new FileReader(LinkStatisticsFn));
    Map<Integer, Integer> targetHashToMaxCount = new HashMap<Integer, Integer>();
    Map<Integer, Integer> targetHashToSecondMaxCount = new HashMap<Integer, Integer>();
    
    Set<Integer> reachableTargets = 
        new HashSet<Integer>(textHashToMaxTargetHash.values());
    for (String line; (line = br.readLine()) != null;) {
      String[] lineParts = SEPARATOR_PATTERN.split(line, 3);
      if (lineParts.length != 3) {
        continue;
      }
      Integer targetHash = lineParts[1].hashCode();
      if (!reachableTargets.contains(targetHash)) {
        continue;
      }
      int count = Integer.parseInt(lineParts[0]);
      String anchorText =  new String(lineParts[2]);
      if (!targetHashToMaxCount.containsKey(targetHash) ||
          targetHashToMaxCount.get(targetHash) < count) {
        // Second most frequent is set to previous most frequent. 
        if (targetHashToMaxCount.containsKey(targetHash)) {
          targetHashToSecondMaxAnchor.put(targetHash, 
              targetHashToMaxAnchor.get(targetHash));
          targetHashToSecondMaxCount.put(targetHash, 
              targetHashToMaxCount.get(targetHash));
        }
        // Most frequent is updated.
        targetHashToMaxAnchor.put(targetHash, anchorText);
        targetHashToMaxCount.put(targetHash, count);
      } else if (!targetHashToSecondMaxCount.containsKey(targetHash) ||
          targetHashToSecondMaxCount.get(targetHash) < count) {
        // Its more frequent than second most but not than most frequent.
        targetHashToSecondMaxAnchor.put(targetHash, anchorText);
        targetHashToSecondMaxCount.put(targetHash, count);       
      }
    }
    br.close();
  }
  
  public String expand(String text) {
    int textHash = text.hashCode();
    Integer maxTargetHash;
    if (textHashToMaxTargetHash.containsKey(textHash)) {
      maxTargetHash = textHashToMaxTargetHash.get(textHash);
    } else {
      return text;
    }
    if (targetHashToMaxAnchor.containsKey(maxTargetHash)) {
      return targetHashToMaxAnchor.get(maxTargetHash);
    } else {
      return text;
    }
  }
  
  public String expandToSecond(String text) {
    int textHash = text.hashCode();
    Integer maxTargetHash;
    if (textHashToMaxTargetHash.containsKey(textHash)) {
      maxTargetHash = textHashToMaxTargetHash.get(textHash);
    } else {
      return text;
    }
    if (targetHashToSecondMaxAnchor.containsKey(maxTargetHash)) {
      return targetHashToSecondMaxAnchor.get(maxTargetHash);
    } else {
      return text;
    }
  }
  
  public boolean isMapped(String text) {
    int textHash = text.hashCode();
    return textHashToMaxTargetHash.containsKey(textHash);
  }
  
  public boolean isFirstOrSecond(String text, boolean lowerCase) {
    int textHash = text.hashCode();
    String compareText = lowerCase ? text.toLowerCase() : text;
    Integer maxTargetHash;
    if (textHashToMaxTargetHash.containsKey(textHash)) {
      maxTargetHash = textHashToMaxTargetHash.get(textHash);
    } else {
      return false;
    }
    Set<String> expansions = new HashSet<String>(2);
    if (targetHashToMaxAnchor.containsKey(maxTargetHash)) {
      String expanded = targetHashToMaxAnchor.get(maxTargetHash);
      if (lowerCase) {
        expanded = expanded.toLowerCase();
      }
      expansions.add(expanded);
    }
    if (targetHashToSecondMaxAnchor.containsKey(maxTargetHash)) {
      String expanded = targetHashToSecondMaxAnchor.get(maxTargetHash);
      if (lowerCase) {
        expanded = expanded.toLowerCase();
      }
      expansions.add(expanded);
    }
    return expansions.contains(compareText);
  }
  
  public String alternative(String text) {
    String alternative = expand(text);
    if (text.equals(alternative)) {
      alternative = expandToSecond(text);
    }
    return alternative;
  }
}
