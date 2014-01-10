package matcher;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.HashSet;


import rerac.protos.Corpus.Document;
import util.DocumentExtractor;

public abstract class Matcher {
  
  public static final int MAX_MATCH_LENGTH = 5;
  
  // Small trick to spare repetitive computations.
  private String lastDocId = null;
  private List<Integer> lastIndices = null;
  
  public abstract boolean isMatch(Document doc, int start, int end);
  /**
   * This normalizes the match to a canonical form, i.e. although occurrences
   * of a certain matching entity may be different, they all should be mapped to
   * the same canonical form. (This might be too difficult in reality, but it is
   * the goal).
   * TODO: maybe let's make the method taking a String as an argument, instead 
   * of a document?
   * 
   * @param doc
   * @param start
   * @param end
   * @return
   */
  public String normalize(Document doc, int start, int end) {
    return DocumentExtractor.textFromTokens(doc, start, end);
  }
  
  /**
   * This returns, in a list, start and end positions of maximal matches in the
   * document.
   * 
   * @param doc
   * @return
   */
  public List<Integer> maximalMatches(Document doc, int maxLen) {
    List<Integer> startEnds = new ArrayList<Integer>();
    int maxEnd = 0;
    int lastStart = -1;
    for (int start = 0; start < doc.getTokenCount(); ++start) {
      for (int end = start + 1; 
            end <= Math.min(start + maxLen , doc.getTokenCount()); ++end) {
        if (end > maxEnd && isMatch(doc, start, end)) {
          if (start == lastStart) {
            startEnds.set(startEnds.size() - 1, end);
          } else {
            startEnds.add(start);
            startEnds.add(end);
          }
          maxEnd = end;
          lastStart = start;
        }
      }
    }
    return startEnds;
  }
  
  public List<Integer> maximalMatches(Document doc, boolean optimize) {
    if (optimize) {
      // DocId changed: update cached id and result.
      if (!doc.getId().equals(lastDocId)) {
        lastIndices = maximalMatches(doc, MAX_MATCH_LENGTH);
        lastDocId = doc.getId();
      }
      // Return cached result.
      return lastIndices;
    }
    return maximalMatches(doc, MAX_MATCH_LENGTH);
  }
  
  /**
   * This calls the maximalMatches with optimization (caching) on.
   * Be aware that in order to work correctly with caching, different 
   * succeeding documents have to have distinct id's.
   * 
   * @param doc
   * @return 
   */
  public List<Integer> maximalMatches(Document doc) {
    return maximalMatches(doc, true);
  }
  
  public List<String> maximalStringMatches(Document doc, boolean optimize) {
    List<Integer> idxs = maximalMatches(doc, optimize);
    List<String> retList = new ArrayList<String>(idxs.size() / 2);
    for (int i = 0; i < idxs.size() / 2; ++i) {
      retList.add(DocumentExtractor.textFromTokens(doc, idxs.get(i * 2), idxs.get(i * 2 + 1)));
    }
    return retList;
  }

  
  /**
   * This returns alternatives to the given String, assuming it is a match.
   * Depending on the implementation, the matchString is returned as well.
   * 
   * @param matchString
   * @return
   */
  public Set<String> alternatives(String matchString) {
    Set<String> retSet = new HashSet<String>();
    retSet.add(matchString);
    return retSet;
  }
}
