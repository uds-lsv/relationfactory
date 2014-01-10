package corpus;

public class RawCorpusDocument {
  String id = "";
  String path = "";
  String rawContent = "";
  
  RawCorpusDocument() { }
  
  RawCorpusDocument(String docid) {
    this.id = docid;
  }
  
  RawCorpusDocument(String id, String docfn, String content) {
    this.id = id;
    this.path = docfn;
    this.rawContent = content;
  }

  @Override
  public String toString() {
    return "CorpusDocument [id=" + id + ", path=" + path + ", rawContent="
        + rawContent + "]";
  }

  public String getPath() {
    return path;
  }
  
  public String getRawContent() {
    return rawContent;
  }
  
  public String getId() {
    return id;
  }
}