package util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.GZIPInputStream;

import org.apache.commons.lang3.StringUtils;

public class DTagged {
  private List<TaggedSentence> sentences = 
      new ArrayList<DTagged.TaggedSentence>();
  
  public class TaggedSentence{
    private List<String> tokens;
    private List<String> tags;
    private TextIdentifier id;
    public TaggedSentence(List<String> tokens, List<String> tags, 
        TextIdentifier id) {
      if (tags.size() != tokens.size()) {
        throw new IllegalArgumentException("Number of tokens does not match that of tags.");
      }
      this.tokens = tokens;
      this.tags = tags;
      this.id = id;
    }
    public List<String> getTokens() {
      return tokens;
    }
    public List<String> getTags() {
      return tags;
    }
    public TextIdentifier getId() {
      return id;
    }
    public TaggedSentence withNewId(TextIdentifier newId) {
      return new TaggedSentence(tokens, tags, newId);
    }
    @Override
    public String toString() {
      StringBuffer sb = new StringBuffer();
      sb.append("<D=" + id.toValidString() + ">\n");
      for (int i = 0; i < tokens.size(); ++i) {
        sb.append(tokens.get(i) + " " + tags.get(i) + "\n");
      }
      sb.append("</D>\n");
      return sb.toString();
    }
  }
  
  private DTagged() {
    
  }
  
  public Iterable<String> getTokens() {
    int numTokens = 0;
    for (TaggedSentence s : sentences){
      numTokens += s.getTokens().size();
    }
    List<String> tokens = new ArrayList<String>(numTokens);
    for (TaggedSentence s : sentences){
      tokens.addAll(s.getTokens());
    }
    return tokens;
  }
  
  public Iterable<String> getTags() {
    int numTags = 0;
    for (TaggedSentence s : sentences){
      numTags += s.getTags().size();
    }
    List<String> tags = new ArrayList<String>(numTags);
    for (TaggedSentence s : sentences){
      tags.addAll(s.getTags());
    }
    return tags;
  }
  
  public Iterable<TaggedSentence> getSentences() {
    return sentences;
  }
  
  public String toString() {
    StringBuffer sb = new StringBuffer();
    for (TaggedSentence s : sentences) {
      sb.append(s.toString());
    }
    return sb.toString();
  }
  
  /**
   * This returns from a dtag file consecutive sentences belonging to the same 
   * document.
   * 
   * @return
   */
  public Iterable<DTagged> getDocs() {
    List<DTagged> docs = new ArrayList<DTagged>();
    String currDocId = null;
    DTagged currDTagged = null;
    
    for (TaggedSentence s : sentences) {
      String sDocId = s.getId().getDocId();
      if (!sDocId.equals(currDocId)) {
        // Added a new doc to list.
        // The following sentences are added to this doc.
        currDTagged = new DTagged();
        docs.add(currDTagged);
        currDocId = sDocId;
      }
      currDTagged.add(s);
    }
    return docs;
  }
  
  private void add(TaggedSentence s) {
    sentences.add(s);
  }

  public static DTagged readDtag(File tagFile) throws IOException {
    BufferedReader dtagBr = new BufferedReader(new InputStreamReader(
        new GZIPInputStream(new FileInputStream(tagFile)), "UTF-8"));
    DTagged retDocs = DTagged.fromBufferedReader(dtagBr);
    dtagBr.close();
    return retDocs;
  }
  
  public static DTagged fromBufferedReader(BufferedReader dtagBr) throws IOException {
    DTagged retDocs = new DTagged();
    TextIdentifier docQIdSnr = null;
    List<String> currTags = new ArrayList<String>();
    List<String> currTokens = new ArrayList<String>();
    
    // Stay with the same query as long as the tagged sentences 
    // belong to the same document, and sentence numbers are going up.
    // If sentence numbers are not going up, or doc ids change, read the next
    // line of the dscore file and go to the corresponding query.
    for (String line; (line = dtagBr.readLine()) != null; ) {
      if (line.startsWith("<D")) {
        docQIdSnr = 
            TextIdentifier.fromDelimited(line.substring(3, line.length() - 1));
      } else if (line.startsWith("</D")) {
        retDocs.add(retDocs.new TaggedSentence(currTokens, currTags, docQIdSnr));
        docQIdSnr = null;
        currTags = new ArrayList<String>();
        currTokens = new ArrayList<String>();
      } else {
        String[] parts = line.split(" ");
        String token;
        String tag;
        if (parts.length != 2) {
          if (parts.length < 2) {
            throw new IllegalArgumentException("line does not consist of token + tag and cannot be processed: \n" + line);
          } else {
            token = StringUtils.join(parts, ' ', 0, parts.length - 1);
            tag = parts[parts.length - 1];
            System.err.println("\nWarning: token seems to contain whitespace." +
                "\n" + line +
                "\n id:" + docQIdSnr.toString());
          }
        } else {
          token = parts[0];
          tag = parts[1];
        }
        currTokens.add(token);
        currTags.add(tag);
      }
    }
    return retDocs;
  }

  public static DTagged fromString(String s) throws IOException {
    BufferedReader dtagBr = new BufferedReader(new StringReader(s));
    DTagged retDocs = DTagged.fromBufferedReader(dtagBr);
    dtagBr.close();
    return retDocs;
  }
  
  public String getDocIdFromFirstSentence() {
    String id = null;
    if (sentences.size() != 0) {
      id = sentences.get(0).getId().getDocId();
    } else {
      throw new IllegalStateException("Cannot get doc id without sentences");      
    }
    return id;
  }

}
