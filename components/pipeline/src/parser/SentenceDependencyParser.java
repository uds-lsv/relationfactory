package parser;

import java.util.List;

import rerac.protos.Corpus.Document;

public interface SentenceDependencyParser {
  public static final String DEP = "DEP";
  public Document parse(Document doc);
  public DependencyTree parseToDepTree(List<String> tokens);
}
