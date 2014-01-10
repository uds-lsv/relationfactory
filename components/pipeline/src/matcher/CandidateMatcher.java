package matcher;

import rerac.protos.Corpus.Document;
import util.DocumentExtractor;

public class CandidateMatcher extends Matcher {
  private int argNr;
  
  public CandidateMatcher(int someArgNr) {
    this.argNr = someArgNr;
  }

  @Override
  public boolean isMatch(Document doc, int start, int end) {
    for (String rel : DocumentExtractor.relations(doc)) {
      if (DocumentExtractor.getArgStart(doc, rel, argNr) == start &&
          DocumentExtractor.getArgEnd(doc, rel, argNr) == end) {
        return true;
      }
    }
    return false;
  }

}
