package util;

/**
 * This represents an identifier for a span of text.
 * It can identify a document, which query it belongs to, a sentence from it,
 * and the sentence and argument offsets from a candidate.
 * 
 * The identifier String representation is of the form:
 * 
 * DOC_ID:QUERY_ID:SENT_NR:FILLER_OFFSETS:QUERY_OFFSETS:SENT_OFFETS
 *  
 * @author Benjamin Roth
 *
 */
public class TextIdentifier {
  private String docId;
  private String queryId = null;
  private String sentNr = null;
  private String offsets = null;
  
  public TextIdentifier(String docId, String queryId, String sentNr, 
      String offsets) {
    this.docId = docId;
    this.queryId = queryId;
    this.sentNr = sentNr;
    this.offsets = offsets;
  }
  public TextIdentifier(String docId, String queryId, int sentNr, 
      String offsets) {
    this.docId = docId;
    this.queryId = queryId;
    this.sentNr = Integer.toString(sentNr);
    this.offsets = offsets;
  }

  public TextIdentifier(TextIdentifier id) {
    this.docId = id.docId;
    this.queryId = id.queryId;
    this.sentNr = id.sentNr;
    this.offsets = id.offsets;
  }
  
  public static TextIdentifier fromDelimited(String delimitedId) {
    String[] parts = delimitedId.split(":", 4);
    String docId = parts[0];
    if (parts.length == 1) {
      return new TextIdentifier(docId, null, null, null);
    }
    String queryId = parts[1];
    if (parts.length == 2) {
      return new TextIdentifier(docId, queryId, null, null);
    }
    String sentNr = parts[2];
    if (parts.length == 3) {
      return new TextIdentifier(docId, queryId, sentNr, null);
    }
    String offsets = parts[3];
    return new TextIdentifier(docId, queryId, sentNr, offsets);
  }
  
  // <D=...>
  public static TextIdentifier fromDocLine(String docline) {
    docline = docline.trim().substring(3, docline.length() - 1);
    return fromDelimited(docline);
  }
  
  /**
   * Use this to get representations that can be used as identifiers in the 
   * system. Use toString to get more information about possibly incomplete
   * TextIdentifier objects.
   * 
   * @return
   */
  public String toValidString() {
    if (docId == null) {
      throw new IllegalStateException("TextIdentifier has no docid.");
    }
    StringBuffer sb = new StringBuffer(docId);
    if (queryId == null) {
      if (sentNr != null || offsets != null) {
        throw new IllegalStateException("TextIdentifier has no queryId but subsequent fields: " + this.toString());
      }
      return sb.toString();
    }
    sb.append(":").append(queryId);
    if (sentNr == null) {
      if (offsets != null) {
        throw new IllegalStateException("TextIdentifier has no sentNr but subsequent fields: " + this.toString());
      }
      return sb.toString();
    }
    sb.append(":").append(sentNr);
    if (offsets == null) {
      return sb.toString();
    }
    sb.append(":").append(offsets);
    return sb.toString();
  }
  
  @Override
  public String toString() {
    StringBuffer sb = new StringBuffer(docId);
    sb.append(":").append(queryId == null ? "" : queryId);
    sb.append(":").append(sentNr == null ? "" : sentNr);
    sb.append(":").append(offsets == null ? "" : offsets);
    return sb.toString();
  }
  
  public String getQueryId() {
    return queryId;
  }
  
  public TextIdentifier setSentNr(int sNr) {
    this.sentNr = Integer.toString(sNr);
    return this;
  }
  
  public TextIdentifier setOffsets(String offsets) {
    this.offsets = offsets;
    return this;
  }
  
  public String upToQueryId() {
    StringBuffer sb = new StringBuffer(docId);
    sb.append(":").append(queryId);
    return sb.toString();
  }
  public String upToSnr() {
    StringBuffer sb = new StringBuffer(docId);
    sb.append(":").append(queryId);
    sb.append(":").append(sentNr);
    return sb.toString();
  }
  public String getDocId() {
    return docId;
  }
  public int getSentNr() {
    return Integer.parseInt(sentNr);
  }
  public String getFillerOffsets() {
    return offsets.split(":", 3)[0];
  }
  public String getQueryOffsets() {
    return offsets.split(":", 3)[1];
  }
  public String getSentenceOffsets() {
    return offsets.split(":", 3)[2];
  }
  public void setQueryId(String qstr) {
    this.queryId = qstr;    
  }

  public int getSentenceStart() {
    return Integer.parseInt(getSentenceOffsets().split("-")[0]);
  }

  public int getSentenceEnd() {
    return Integer.parseInt(getSentenceOffsets().split("-")[1]);
  }

  public int getFillerStart() {
    return Integer.parseInt(getFillerOffsets().split("-")[0]);
  }

  public int getFillerEnd() {
    return Integer.parseInt(getFillerOffsets().split("-")[1]);
  }
}
