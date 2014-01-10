package features;

import rerac.protos.Corpus.Document;

public interface DocumentTagger {
  public static final String POS = "POS";
  public Document tag(Document doc);
}
