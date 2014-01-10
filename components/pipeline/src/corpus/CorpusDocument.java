package corpus;

public class CorpusDocument {
  String id = "";
  String path = "";
  String rawContent = "";
  String text = "";
  String title = "";
  String type = "";
  
  CorpusDocument() { }
  
  CorpusDocument(String docid) {
    this.id = docid;
  }
  
  CorpusDocument(String id, String docfn, String content) {
    this.id = id;
    this.path = docfn;
    this.rawContent = content;
  }

  @Override
  public String toString() {
    return "CorpusDocument [id=" + id + ", path=" + path + ", rawContent="
        + rawContent + ", text=" + text + ", title=" + title + ", type=" + type
        + "]";
  }

  public String getPath() {
    return path;
  }
  
  public String getRawContent() {
    return rawContent;
  }

  public String getBody() {
    return text;
  }

  public String getTitle() {
    return title;
  }

  public String getType() {
    return type;
  }

  public String getId() {
    return id;
  }
}