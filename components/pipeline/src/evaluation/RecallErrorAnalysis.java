package evaluation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import util.Candidate;
import util.OffsetPair;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;

public class RecallErrorAnalysis {
  static boolean verbose = true;
  /**
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    String missedContextsFn = args[0];
    String dscoreFn = args[1];
    // Candidates file with all query matches. (sentences are in this set no 
    // matter whether slots are tagged or not)
    String queryMatchCandidatesFn = args[2];
    String taggedCandidatesFn = args[3];
    
    // queryid:docid
    Set<String> retrievedQueryDocs = new HashSet<String>();
    
    BufferedReader br = new BufferedReader(new FileReader(dscoreFn));
    for (String line; (line = br.readLine()) != null;) {
      String[] parts = line.split(" ");
      retrievedQueryDocs.add(parts[0] + ":" + parts[2].split(":",2)[1]);
    }
    br.close();
    
    Multimap<String, OffsetPair> queryDocToMatchSentenceOffsets = HashMultimap.create();
    
    br = new BufferedReader(new FileReader(queryMatchCandidatesFn));
    for (String line; (line = br.readLine()) != null;) {
      Candidate cand = Candidate.fromDelimLine(line);
      if (!"query".equals(cand.getRel())) {
        throw new IllegalArgumentException("Expected relation 'query' in query candidates file.");
      }
      
      String queryDoc = cand.getQid() + ":" + cand.getTextId().getDocId();
      
      String sentenceOffsets = cand.getTextId().getSentenceOffsets();
      int sentStart = Integer.parseInt(sentenceOffsets.split("-")[0]);
      int sentEnd = Integer.parseInt(sentenceOffsets.split("-")[1]);
      
      OffsetPair sentOffsets = new OffsetPair(sentStart, sentEnd);
      queryDocToMatchSentenceOffsets.put(queryDoc, sentOffsets);        
    }
    br.close();
    
    Multimap<String, OffsetPair> queryRelDocToSlotOffsets = HashMultimap.create();

    // All answers for this query, irrespective of relation.
    Multimap<String, OffsetPair> queryDocToSlotOffsets = HashMultimap.create();

    br = new BufferedReader(new FileReader(taggedCandidatesFn));
    for (String line; (line = br.readLine()) != null;) {
      Candidate cand = Candidate.fromDelimLine(line);
      if ("query".equals(cand.getRel())) {
        throw new IllegalArgumentException("Expected TAC relation, found 'query' in query candidates file.");
      }
      
      String queryRelDoc = cand.getQid() + ":"  + cand.getRel() + ":" + cand.getTextId().getDocId();
      String queryDoc = cand.getQid() + ":" + cand.getTextId().getDocId();
      
      String slotOffsets = cand.getTextId().getFillerOffsets();
      int slotStart = Integer.parseInt(slotOffsets.split("-")[0]);
      int slotEnd = Integer.parseInt(slotOffsets.split("-")[1]);
      
      OffsetPair slotOffsetPair = new OffsetPair(slotStart, slotEnd);
      queryRelDocToSlotOffsets.put(queryRelDoc, slotOffsetPair);

      queryDocToSlotOffsets.put(queryDoc, slotOffsetPair);
    }
    br.close();    

    int numMissedDocs = 0;
    int numMissedQuery = 0;
    int numCrossSentence = 0;
    int numInexactTagMatch = 0;
    int otherTagMatch = 0;
    int numNoTagMatch = 0;

    //int numExactTagMatch = 0;
    int numOther = 0;

    
    br = new BufferedReader(new FileReader(missedContextsFn));
    for (String line; (line = br.readLine()) != null;) {
      //System.out.println(line);
      // qid + ":" + enttype + ":" + slotrel + ":" + doc_id + ":" + predoff + ":" + entityoff + ":" + filleroff + ":" + answerString;
      String[] parts = line.split(":",8);
      String qid = parts[0];
      String enttype = parts[1];
      String relNoType = parts[2];
      String rel = enttype + ":" + relNoType;
      String docid = parts[3];
      //String[] sentOffsets =  parts[4].split(",");
      String[] queryOffsets = parts[5].split(",");
      String[] slotOffsets = parts[6].split(",");
      String answer = parts[7];
      
      if (!retrievedQueryDocs.contains(qid + ":" + docid)) {
        if (numMissedDocs < 10 || verbose) {
          System.out.println("[DOC]\t" + line);
        }       
        // docid in drank? 
        numMissedDocs += 1;
      } else {
        // Sentence in candidates? i.e.: 
        // Gold query offsets within some candidate sentence offsets?
        
        // There can be several query offsets in the gold key:
        // We count a query as not matched if none the the query offsets is  
        // in the candidate sentences.
        
        boolean isQueryMatched = false;

        

        for (String qo : queryOffsets) {
          //System.out.println(qo);
          if (qo.isEmpty() && "alternate_names".equals(relNoType)) {
            // Alternate names does not need query match.
            isQueryMatched = true;
            break;
          }
          int qStart = Integer.parseInt(qo.split("-")[0]);
          int qEnd = Integer.parseInt(qo.split("-")[1]);
          Collection<OffsetPair> sentenceOffsets = 
              queryDocToMatchSentenceOffsets.get(qid + ":" + docid);
          if(isInSomeRange(qStart, qEnd, sentenceOffsets)) {
            isQueryMatched = true;
            break;
          }
        }
        if (!isQueryMatched) {
          if (numMissedQuery < 10 || verbose) {
            System.out.println("[QUERY]\t" + line);
          }
          numMissedQuery += 1;
        } else {
          // Slot filler in some sentence with query? I.e.:
          // Slot offsets within query candidate sentence offsets?
          boolean isSlotInCands = false;
          for (String so : slotOffsets) {
            int sStart = Integer.parseInt(so.split("-")[0]);
            int sEnd = Integer.parseInt(so.split("-")[1]);
            Collection<OffsetPair> sentenceOffsets = 
                queryDocToMatchSentenceOffsets.get(qid + ":" + docid);
            if(isInSomeRange(sStart, sEnd, sentenceOffsets)) {
              isSlotInCands = true;
              break;
            }
          }
          
          if (!isSlotInCands) {
            if (numCrossSentence < 10 || verbose) {
              System.out.println("[CROSS]\t" + line);
            }
            numCrossSentence += 1;
          } else {
            // Slot is in same sentence with query.
            // TODO: type of overlap with tagged slot offsets:
            // None: tagging missing
            // Inexact: inexact tagging
            // Exact: other 
            
            boolean matchesExact = false;
            boolean hasOverlap = false;
            boolean matchesOtherTag = false;
            
            for (String so : slotOffsets) {
              int slStart = Integer.parseInt(so.split("-")[0]);
              int slEnd = Integer.parseInt(so.split("-")[1]);
              // Slot candidates retrieved and tagged for this query and relation.
              Collection<OffsetPair> candSlotOffsets = 
                  queryRelDocToSlotOffsets.get(qid + ":" + rel + ":" + docid);
              
              if(almostExactMatch(slStart, slEnd, candSlotOffsets)) {
                matchesExact = true;
              } else if (overlap(slStart, slEnd, candSlotOffsets)) {
                hasOverlap = true;
              } else {
                Collection<OffsetPair> allSlotOffsets =
                    queryDocToSlotOffsets.get(qid + ":" + docid);
                matchesOtherTag = almostExactMatch(slStart, slEnd, allSlotOffsets);
              }
            }
            if (matchesExact) {
              if (numOther < 10 || verbose) {
                System.out.println("[OTHER]\t" + line);
              }
              numOther += 1;
            } else if (hasOverlap) {
              if (numInexactTagMatch < 10 || verbose) {
                System.out.println("[INEXACT]\t" + line);
              }
              numInexactTagMatch += 1;
            } else if(matchesOtherTag) {
              if (otherTagMatch < 10 || verbose) {
                System.out.println("[OTHERTAG]\t" + line);
              }
              otherTagMatch += 1;
            } else {
              if (numNoTagMatch < 10 || verbose) {
                System.out.println("[NOTAG]\t" + line);
              }
              numNoTagMatch += 1;
            }
          }          
        }
      }
    }
    br.close();

    float sum = numMissedDocs + numMissedQuery + numCrossSentence + numInexactTagMatch + numNoTagMatch + numOther;
    sum/=100;
    System.out.println("[_STAT]\t1 Doc not retrieved:\t" + numMissedDocs + "\t" + (numMissedDocs/sum) + "%");
    System.out.println("[_STAT]\t2 Query not matched:\t" + numMissedQuery + "\t" + (numMissedQuery/sum) + "%");
    System.out.println("[_STAT]\t3 Slot not in query sentence:\t" + numCrossSentence + "\t" + (numCrossSentence/sum) + "%");
    System.out.println("[_STAT]\t4 Slot tag inexact:\t" + numInexactTagMatch + "\t" + (numInexactTagMatch/sum) + "%");
    System.out.println("[_STAT]\t5 Wrong slot tag:\t" + otherTagMatch + "\t" + (otherTagMatch/sum) + "%");
    System.out.println("[_STAT]\t6 Slot not tagged:\t" + numNoTagMatch + "\t" + (numNoTagMatch/sum) + "%");
    System.out.println("[_STAT]\t7 Other (validation):\t" + numOther + "\t" + (numOther/sum) + "%");
  }

  private static boolean isInSomeRange(int qStart, int qEnd,
      Collection<OffsetPair> candSentenceOffsets) {
    // TODO: make configurable to have AND instead of OR matching semantics.
    for (OffsetPair p : candSentenceOffsets) {
      if (qStart >= p.start && qEnd<=p.end) {
        return true;
      }
    }
    return false;
  }
  
  private static boolean exactMatch(int goldSlotStart, int goldSlotEnd,
      Collection<OffsetPair> candSlotOffsets) {
    // TODO: make configurable to have AND instead of OR matching semantics.
    for (OffsetPair p : candSlotOffsets) {
      if (goldSlotStart == p.start && goldSlotEnd == p.end) {
        return true;
      }
    }
    return false;
  }
  
  private static boolean almostExactMatch(int goldSlotStart, int goldSlotEnd,
      Collection<OffsetPair> candSlotOffsets) {
    // TODO: make configurable to have AND instead of OR matching semantics.
    for (OffsetPair p : candSlotOffsets) {
      if (Math.abs(goldSlotStart - p.start) <= 1 && Math.abs(goldSlotEnd - p.end) <= 1) {
        return true;
      }
    }
    return false;
  }
  
  private static boolean overlap(int goldSlotStart, int goldSlotEnd,
      Collection<OffsetPair> candSlotOffsets) {
    // TODO: make configurable to have AND instead of OR matching semantics.
    for (OffsetPair p : candSlotOffsets) {
      if ((p.start <= goldSlotStart && goldSlotStart <= p.end) ||
          (p.start <= goldSlotEnd && goldSlotEnd <= p.end)||
          (goldSlotStart <= p.start && p.start <= goldSlotEnd) ||
          (goldSlotStart <= p.end && p.end <= goldSlotEnd)) {
        return true;
      }
    }
    return false;
  }
}
