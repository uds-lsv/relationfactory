package entity_expansion;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;


public class TopNLinkEntityExpander {
  Map<Integer, Integer> textHashToMaxTargetHash = new HashMap<Integer, Integer>();
  Map<Integer, TopN<String>> targetHashToMaxAnchors = new HashMap<Integer, TopN<String>>();
  
  private static final int MINCOUNT = 2;
  
  private static final Pattern SEPARATOR_PATTERN = Pattern.compile(" ");
  
  /**
   * 
   * @param LinkStatisticsFn
   * @param maxN
   * @param requireLinkBack whether the expanded texts have to be linked
   * back to the same article (with maximum probaility) as the query text.
   * @throws IOException
   */
  public TopNLinkEntityExpander(String LinkStatisticsFn, int maxN, 
      boolean requireLinkBack) throws IOException {
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
      
      
      Integer anchorTextHash =  anchorText.hashCode();
      if(requireLinkBack && 
          !targetHash.equals(textHashToMaxTargetHash.get(anchorTextHash))) {
        // Only expansions that are linked back to articles are allowed.
        continue;
      }
      
      if (!targetHashToMaxAnchors.containsKey(targetHash)) {
        targetHashToMaxAnchors.put(targetHash, new TopN<String>(maxN));
      }
      targetHashToMaxAnchors.get(targetHash).add(anchorText, count);
    }
    br.close();
  }
  
  public TopNLinkEntityExpander(String LinkStatisticsFn, int maxN) 
      throws IOException {
    // TODO: change default to true, if it works better.
    this(LinkStatisticsFn, maxN, false);
  }

  public List<String> expand(String text) {
    int textHash = text.hashCode();
    Integer maxTargetHash;
    if (textHashToMaxTargetHash.containsKey(textHash)) {
      maxTargetHash = textHashToMaxTargetHash.get(textHash);
    } else {
      return new ArrayList<String>();
    }
    if (targetHashToMaxAnchors.containsKey(maxTargetHash)) {
      List<String> retList = 
          targetHashToMaxAnchors.get(maxTargetHash).elementList();
      //retList.remove(text);
      return retList;
    } else {
      return new ArrayList<String>();
    }
  }

}
