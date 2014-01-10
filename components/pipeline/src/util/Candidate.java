package util;

import java.util.Arrays;

import com.google.common.base.Joiner;

public class Candidate {
  private static final String FORMAT_OUT = "%s\t%s\t%s\t%s\t%d\t%d\t%d\t%d\t%s";
  
  private OffsetPair targetOffsets;
  private OffsetPair fillerOffsets;
  private String sentence;
  // initialized directly from tokens, based on target offsets.
  private String target = null;
  // initialized directly from tokens, based on filler offsets.
  private String filler = null;
  // initialized from given string. Might not be a substring of sentence.
  private String canonicalFiller;
  private String identifier;
  private String qid;
  private String rel;
  private String[] tokens = null;
  
  public Candidate(String target, String filler) {
    this.target = target;
    this.filler = filler;
  }
  
  public Candidate() {  }
  
  public Candidate(Candidate c) {
    this.targetOffsets = c.targetOffsets;
    this.fillerOffsets = c.fillerOffsets;
    this.sentence = c.sentence;
    this.target = c.target;
    this.filler = c.filler;
    this.canonicalFiller = c.canonicalFiller;
    this.identifier = c.identifier;
    this.qid = c.qid;
    this.rel = c.rel;
    this.tokens = c.tokens;
  }
  
  public Candidate(String qid, String rel, String canonicalFiller, String docid, 
      int targetStart, int targetEnd, int fillerStart, int fillerEnd, 
      String sentence) {  
    this.targetOffsets = new OffsetPair(targetStart, targetEnd);
    this.fillerOffsets = new OffsetPair(fillerStart, fillerEnd);
    this.sentence = sentence;
    this.canonicalFiller = canonicalFiller;
    this.identifier = docid;
    this.qid = qid;
    this.rel = rel;
  }
  
  public static Candidate fromDelimLine(String candLine) {
    String[] candParts = candLine.split("\\t", 9);
    if (candParts.length != 9) {
      throw new IllegalArgumentException("Invalid line for candidate, " + 
          candParts.length + " fields: " + candLine);
    }
    return new Candidate(candParts[0], candParts[1], candParts[2], candParts[3],
        Integer.parseInt(candParts[4]), Integer.parseInt(candParts[5]), 
        Integer.parseInt(candParts[6]), Integer.parseInt(candParts[7]),
        candParts[8]);
  }
  
  public String toString() {
    return String.format(FORMAT_OUT, qid, rel, filler, identifier, 
        targetOffsets.start, targetOffsets.end, fillerOffsets.start, 
        fillerOffsets.end, sentence);
  }
  
  public String getQid() {
    return qid;
  }
  
  public String getRel() {
    return rel;
  }
  
  public TextIdentifier getTextId() {
    return TextIdentifier.fromDelimited(identifier);
  }
  
  @Deprecated 
  /**
   * use getTextId() instead.
   * TODO: store TextId and make setter.
   * @return
   */
  public String getIdentifier() {
    return identifier;
  }

  public void setIdentifier(String docid) {
    this.identifier = docid;
  }

  public int getTargetStart() {
    return targetOffsets.start;
  }

  public int getTargetEnd() {
    return targetOffsets.end;
  }

  public int getFillerStart() {
    return fillerOffsets.start;
  }

  public int getFillerEnd() {
    return fillerOffsets.end;
  }

  public String getSentence() {
    return sentence;
  }
  
  public String getCanonicalFiller() {
    return canonicalFiller;
  }

  public String getTarget() {
    if (target == null) {
      // important: use getTokens() b/c lazy init!
      target = Joiner.on(' ').join(Arrays.copyOfRange(getTokens(), 
          targetOffsets.start, targetOffsets.end));
    }
    return target;
  }

  public String getFiller() {
    if (filler == null) {
      // important: use getTokens() b/c lazy init!
      filler = Joiner.on(' ').join(Arrays.copyOfRange(getTokens(), 
          fillerOffsets.start, fillerOffsets.end));
    }
    return filler;
  }

  public String[] getTokens() {
    if (tokens == null) {
      tokens = sentence.split(" ");
    }
    return tokens;
  }

}