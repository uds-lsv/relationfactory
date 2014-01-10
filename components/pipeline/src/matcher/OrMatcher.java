package matcher;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import util.DocumentExtractor;
import rerac.protos.Corpus.Document;

public class OrMatcher extends Matcher {
  
  List<Matcher> matchers = new ArrayList<Matcher>();
  Set<String> filterSet = null;
  
  public void addMatcher(Matcher m) {
    matchers.add(m);
  }
  
  public void addForbiddenArg(String forbidden) {
    if (null == filterSet) {
      filterSet= new HashSet<String>();
    }
    filterSet.add(forbidden);
  }
  
  public void setForbidden(Set<String> forbidden) {
    filterSet = forbidden;
  }

  @Override
  public boolean isMatch(Document doc, int start, int end) {
    for (Matcher m : matchers) {
      if (m.isMatch(doc, start, end)) {
        boolean blacklisted = false;
        if (filterSet != null) {
          blacklisted = 
              filterSet.contains(DocumentExtractor.textFromTokens(doc, start, end)) ||
              filterSet.contains(m.normalize(doc, start, end));
        }
        return !blacklisted;
      }
    }
    return false;
  }

  @Override
  public String normalize(Document doc, int start, int end) {
    for (Matcher m : matchers) {
      if (m.isMatch(doc, start, end)) {
        return m.normalize(doc, start, end);
      }
    }
    return DocumentExtractor.textFromTokens(doc, start, end);
  }
  
  public Set<String> alternatives(String matchString) {
    Set<String> retSet = new HashSet<String>();
    retSet.add(matchString);
    for (Matcher m : matchers) {
      retSet.addAll(m.alternatives(matchString));
    }
    return retSet;
  }

}
