package matcher;

import java.util.List;

import rerac.protos.Corpus.Document;

public interface ContextPatternMatcher {
  public List<String> arguments(Document sentence, boolean optimize);
}
