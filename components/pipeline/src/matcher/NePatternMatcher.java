package matcher;

import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

import util.Pair;

import rerac.protos.Corpus.Document;
import util.DocumentExtractor;

public class NePatternMatcher extends Matcher {

  Pattern pattern; 
  
  boolean optimize;
  Set<Pair<Integer>> matchesForId = new HashSet<Pair<Integer>>();
  Set<Pair<Integer>> nonMatchesForId = new HashSet<Pair<Integer>>();
  String lastDocId = null;

  public NePatternMatcher(String neRegex, boolean optimize) {
    pattern = Pattern.compile(neRegex);
    this.optimize = optimize;
  }
  
  public NePatternMatcher(String neRegex) {
    this(neRegex, true);
  }
  
  @Override
  public boolean isMatch(Document doc, int start, int end) {
    if (optimize) {
      if (!doc.getId().equals(lastDocId)) {
        matchesForId.clear();
        nonMatchesForId.clear();
        lastDocId = doc.getId();
      }
      Pair<Integer> p = new Pair<Integer>(start, end);
      if (matchesForId.contains(p)) {
        return true;
      } else if (nonMatchesForId.contains(p)){
        return false;
      } else {
        String neString = DocumentExtractor.annotationFromTokens(doc, 
            DocumentExtractor.NE, " ", start, end);
        java.util.regex.Matcher m = pattern.matcher(neString);
        if (m.matches()) {
          matchesForId.add(p);
          return true;
        } else {
          nonMatchesForId.add(p);
          return false;
        }
      }
    }
    String neString = DocumentExtractor.annotationFromTokens(doc, 
        DocumentExtractor.NE, " ", start, end);
    java.util.regex.Matcher m = pattern.matcher(neString);
    return m.matches();
  }
}
