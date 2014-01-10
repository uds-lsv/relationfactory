package evaluation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import query.QueryList;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multiset;

public class SentenceDistances {

  /**
   * 
   * @param queryAliases a Collection of query aliases, starting and ending 
   * with the separator " ".
   * @param slot slot string to be matched, starting and ending with " ";
   * @param sentences A list of strings holding the sentence tokens separated 
   * by " ". The sentences also have to start and end with the separator " ".
   * @return
   */
  public static int closest(Collection<String> queryAliases, String slot, 
      List<String> sentences) {
    if (!slot.startsWith(" ") || !slot.endsWith(" ")) {
      throw new IllegalArgumentException(
          "Slot has to start and end with \" \"");      
    }
    for (String qStr : queryAliases) {
      if (!qStr.startsWith(" ") || !qStr.endsWith(" ")) {
        throw new IllegalArgumentException(
            "Query (or alias) has to start and end with \" \"");      
      }       
    }
    int lastQueryMatch = -1;
    int lastSlotMatch = -1;
    int minDist = Integer.MAX_VALUE;
    for (int sentNr = 0; sentNr < sentences.size(); ++sentNr) {
      String sentence = sentences.get(sentNr);
      if (!sentence.startsWith(" ") || !sentence.endsWith(" ")) {
        throw new IllegalArgumentException(
            "Sentence has to start and end with \" \"");
      }
      for (String qStr : queryAliases) {
        if (sentence.contains(qStr)) {
          int dist = lastSlotMatch - sentNr;
          if (Math.abs(dist) < minDist) {
            minDist = dist;
          }
          lastQueryMatch = sentNr;
        }
      }
      if (sentence.contains(slot)) {
        int dist = sentNr - lastQueryMatch;
        if (Math.abs(dist) < minDist) {
          minDist = dist;
        }        
        lastSlotMatch = sentNr;
      }
    }
    return minDist;
  }
  
  
  public static void main(String[] args) throws IOException {
    if (args.length != 3) {
      System.err.println("java SentenceDistances <query_expanded> <drank> <key>");
      System.err.println("query_expanded: query file (holding query string and aliases).");
      System.err.println("drank:          ranked documents.");
      System.err.println("key:            keyfile to be compared with.");
      System.err.println("A statistics over the distances between queries and slots is written to stdout.");
      return;
    }
    
    QueryList ql = new QueryList(args[0]);
    String drankFn = args[1];
    String keyFn = args[2];
    
    Multimap<String, String> qidToAliases = HashMultimap.create();
    for (QueryList.Query q : ql.getQueries()){
      qidToAliases.put(q.getId(), " " + q.getName() + " ");
      for (String alias : q.getAliases()) {
        qidToAliases.put(q.getId(), " " + alias + " ");
      }
    }
    
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
    
    br = new BufferedReader(new FileReader(drankFn));

    List<String> sentences = new ArrayList<String>();
    
    StringBuffer sentenceBuffer = new StringBuffer();
    String currDocid = "";
    Multiset<Integer> distCounts = HashMultiset.create();

    for (String line; (line = br.readLine()) != null; ) {
      if (line.startsWith("<D")) {
        sentenceBuffer = new StringBuffer();
        String sentenceId = line.substring(3, line.length() - 1);
        int lastDotIdx = sentenceId.lastIndexOf('.');
        String docid = sentenceId.substring(0, lastDotIdx);
        if (!docid.equals(currDocid)) {
          for (String keyLine : docidToKeyLines.get(currDocid)) {
            // 22 SF500:org:alternate_names APW_ENG_20070813.0797.LDC2009T13 3 0 LLC
            String[] klParts = keyLine.split(" ", 6);
            String slot = " " + klParts[5] + " ";
            String qid = klParts[1].split(":")[0];
            int closestDistance = closest(qidToAliases.get(qid), slot, sentences);
            distCounts.add(closestDistance);
          }
          sentences.clear();
          currDocid = docid;
        }
      } else if (line.startsWith("</D")) {
        sentenceBuffer.append(" ");
        sentences.add(sentenceBuffer.toString());
      } else {
        // Split line for compatibility with dtag.
        sentenceBuffer.append(" ").append(line.split(" ")[0]);
      }
    }
    br.close();
    
    int numNotContained = distCounts.count(Integer.MAX_VALUE);
    int numSame = distCounts.count(0);
    int numOneBefore = distCounts.count(-1);
    int numOneAfter = distCounts.count(1);

    int numFarBefore = 0;
    int numFarAfter = 0;
    for (Integer d : distCounts.elementSet()) {
      if (d < 1) {
        numFarBefore += distCounts.count(d);
      } else if (d > 1 && d != Integer.MAX_VALUE) {
        numFarAfter += distCounts.count(d);
      }
    }
    System.out.println("pair statistics:");
    System.out.println("not contained: " + numNotContained);
    System.out.println("slot >1 before: " + numFarBefore);
    System.out.println("slot 1 before: " + numOneBefore);
    System.out.println("same sentence: " + numSame);
    System.out.println("slot 1 after: " + numOneAfter);
    System.out.println("slot >1 after: " + numFarAfter);
  }
}
