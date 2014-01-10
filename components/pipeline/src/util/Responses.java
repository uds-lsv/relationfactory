package util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import query.QueryList;
import query.QueryList.Query;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;

import entity_expansion.MaxLinkEntityExpander;

public class Responses {

  QueryList ql;
  private final static double MIN_SCORE = 0.0001;

  private static String formatScore(double score) {
    if (score > 0.0 && score < MIN_SCORE) {
      // If very small positive, round up to smallest printable positive score.
      score = MIN_SCORE;
    }
    return String.format("%1.4f", score);
  }
  public class Response2012 {
    //SF502   org:alternate_names     lsv     NIL
    //SF503   org:alternate_names     lsv     APW_ENG_20080421.1260.LDC2009T13        Badr Brigade    0       0       0       0       0.37530365
    final String responseLine;
    final String[] fields;

    public Response2012(String responseLine) {
      this.responseLine = responseLine;
      this.fields = responseLine.split("\t");
      if (fields.length != 4 && fields.length != 10) {
        throw new IllegalArgumentException(
            "response must have 4 or 10 fields, found " + fields.length + " :\n"
        + responseLine);
      }
    }
    
    public Response2012(String qid, String rel, String teamId, String textId, 
        String slotVal, int fillerOffsetStart, int fillerOffsetEnd,
        int contextOffsetStart, int contextOffsetEnd, double score) {
      this(qid + "\t" + rel + "\t" + teamId + "\t" + textId + "\t"
          + slotVal + "\t" + fillerOffsetStart + "\t" + fillerOffsetEnd + "\t" + 
          contextOffsetStart + "\t" + contextOffsetEnd + "\t" + 
          formatScore(score));
    }

    public int hashCode() {
      return responseLine.hashCode();
    }

    public boolean equals(Object obj) {
      if (obj == null)
        return false;
      if (obj == this)
        return true;
      if (!(obj instanceof Response2012))
        return false;
      Response2012 cf = (Response2012) obj;
      return this.responseLine.equals(cf.responseLine);
    }

    public double getScore() {
      return Double.parseDouble(fields[9]);
    }

    public String getQid() {
      return fields[0];
    }

    public String getRelation() {
      return fields[1];
    }

    public boolean isNIL() {
      return fields.length == 4 && fields[3].equals("NIL");
    }
    
    public TextIdentifier getTextId() {
      return TextIdentifier.fromDelimited(fields[3]);
    }
    
    @Override
    public String toString() {
      return responseLine;
    }

    public Response2012 withNewScore(double score) {
      int lastTab = responseLine.lastIndexOf('\t');
      return 
          new Response2012(responseLine.substring(0, lastTab) + '\t' + 
              formatScore(score));
    }
    
    public String getSlotFill() {
      return fields[4];
    }

  }
  // qid + "\t" + rel -> {response_line}
  Multimap<String, Response2012> tupleToResponses = HashMultimap.create();

  public Responses(QueryList ql) {
    this.ql = ql;
  }
  
  /* This takes all the information necessary for 2012 responses.
   * 2013 responses "smuggle in" offset information via the text id.
   */
  public void addResponse2012(String qid, String rel, String teamId, String textId, 
      String slotVal, int fillerOffsetStart, int fillerOffsetEnd,
      int contextOffsetStart, int contextOffsetEnd, double confidence) {
    Response2012 response = new Response2012(qid, rel, teamId, textId, slotVal, 
        fillerOffsetStart, fillerOffsetEnd, contextOffsetStart, 
        contextOffsetEnd, confidence);
    String tuple = qid + "\t" + rel;
    tupleToResponses.put(tuple, response);
  }
  
  public void readFile2012(String responseFn) throws IOException {
    LineNumberReader responseBr = 
        new LineNumberReader(new FileReader(responseFn));
    readResponse2012(responseBr);
    responseBr.close();
  }
  
  public void readResponse2012(BufferedReader responseBr) throws IOException {
    // TODO Auto-generated method stub
    for (String responseLine; (responseLine = responseBr.readLine()) != null;) {
      Response2012 r = new Response2012(responseLine);
      if (r.isNIL()) {
        continue;
      }
      if (r.getScore() <= 0.0) {
        throw new IllegalArgumentException("Score <= 0 in reponse: " + r.toString());
      }
      String tuple = r.getQid() + "\t" + r.getRelation();
      tupleToResponses.put(tuple, r);
    }
  }
  
  /**
   * This writes out the best response for single slots and all responses for
   * list slots.
   * Scores are normalized to lie between 0 and 1.
   * No redundancy elimination whatsoever is done, so literally same slots may
   * be written out.
   * 
   * @param ql
   * @param tupleToResponses
   * @param teamId
   * @param bw
   * @throws IOException
   */
  public void writeResponse(String teamId, 
      BufferedWriter bw) throws IOException {
    // Get Best response for each query-relation tuple.
    Map<String, Response2012> tupleToBestResponse = new HashMap<String, Response2012>();
    for (String tuple : tupleToResponses.keySet()) {
      for (Response2012 response : tupleToResponses.get(tuple)) {
        if (!tupleToBestResponse.containsKey(tuple)) {
          tupleToBestResponse.put(tuple, response);
        } else {
          Response2012 bestResponseSoFar = tupleToBestResponse.get(tuple);
          double bestScoreSoFar = bestResponseSoFar.getScore();
          if (response.getScore() > bestScoreSoFar) {
            tupleToBestResponse.put(tuple, response);
          }
        }
      }
    }
    // Write responses for slots.
    for (Query q : ql.getQueries()) {
      // Only best response for single slot relations.
      for (String relation : q.getSingleRelations()) {
        String tuple = q.getId() + "\t" + relation;
        if (tupleToBestResponse.containsKey(tuple)) {
          Response2012 response = tupleToBestResponse.get(tuple);
          double score = response.getScore();
          // Score must lie between 0 and 1.
          score = Math.min(1.0, score);
          bw.append(response.withNewScore(score).toString());
          bw.newLine();
        } else {
          bw.append(tuple + "\t" + teamId + "\tNIL");
          bw.newLine();
        }
      }
      // All responses for list slot relations.
      for (String relation : q.getListRelations()) {
        String tuple = q.getId() + "\t" + relation;
        if (tupleToBestResponse.containsKey(tuple)) {
          Response2012 bestResponseForTuple = tupleToBestResponse.get(tuple);
          double bestScoreForTuple = bestResponseForTuple.getScore();
          double normScoreBy = Math.max(1.0, bestScoreForTuple);
          for (Response2012 response : tupleToResponses.get(tuple)) {
            double score = response.getScore() / normScoreBy;
            bw.append(response.withNewScore(score).toString());
            bw.newLine();
          }  
        } else {
          bw.append(tuple + "\t" + teamId + "\tNIL");
          bw.newLine();
        }
      }
    }
  }
  
  public void eliminateRedundancy(MaxLinkEntityExpander mle, 
      Map<String, String> sentIdTitleToOrg){
    // Remove responses that are redundant with query entity.
    Multimap<String, Response2012> tupleToNoQueryResponses = 
        HashMultimap.create();
    for (Query q : ql.getQueries()) {
      Set<String> normalizedAliases = new HashSet<String>();
      normalizedAliases.add(normalize(q.getName(), mle, ""));
      for (String al : q.getAliases()) {
        normalizedAliases.add(normalize(al, mle, ""));
      }
      
      for (String rel : q.getRelations()) {
        String tuple = q.getId() + "\t" + rel;
        if (rel.endsWith("alternate_names")) {
          // Take all responses for alternate names.
          tupleToNoQueryResponses.putAll(tuple, tupleToResponses.get(tuple));
          continue;
        }
        // For other relations filter out slot fillers that equal the query.
        for (Response2012 r : tupleToResponses.get(tuple)) {
          String normalizedSlotfill = normalize(r.getSlotFill(), mle, "");
          if (!normalizedAliases.contains(normalizedSlotfill)) {
            tupleToNoQueryResponses.put(tuple, r);
          }
        }
      }
    }
    tupleToResponses = tupleToNoQueryResponses;
    
    // Remove responses that are redundant with other responses.
    Map<String, Response2012> tripleToResponse = new HashMap<String, Response2012>();
    Map<String, Double> tripleToMaxScore = new HashMap<String, Double>();
    for (Response2012 response : tupleToResponses.values()) {
      TextIdentifier idWithOffSets = response.getTextId();
      String canonicalFill = 
          normalize(response.getSlotFill(), mle, response.getRelation(), 
              sentIdTitleToOrg, idWithOffSets.upToSnr());
      String tripleKey = response.getQid() + "\t" + response.getRelation() + "\t" + canonicalFill;
      if (!tripleToMaxScore.containsKey(tripleKey) ||
          response.getScore() > tripleToMaxScore.get(tripleKey)) {
        tripleToMaxScore.put(tripleKey, response.getScore());
        tripleToResponse.put(tripleKey, response);
      }
    }
    tupleToResponses.clear();
    for (Response2012 response : tripleToResponse.values()) {
      String tuple = response.getQid() + "\t" + response.getRelation();
      tupleToResponses.put(tuple, response);
    }
  }
  
  private static String normalize(String fill, MaxLinkEntityExpander mle, 
      String rel, Map<String, String> sentIdTitleToOrg, String sentId) {
    String canonicalFill = fill.trim();
    if (rel.contains("alternate_names") || null == mle) {
      // Special case: Alternate names are not expanded.
      canonicalFill = canonicalFill.replaceAll("[^\\p{L}\\p{N}]", "").toLowerCase();
    } else if (rel.equals("per:title") && sentIdTitleToOrg != null) {
      // Special case: additionally get organization title belongs to, normalize 
      // and keep both.
      canonicalFill = mle.isMapped(fill) ? 
          mle.expand(fill) : mle.expand(fill.toLowerCase());
      canonicalFill = canonicalFill.replaceAll("[^\\p{L}\\p{N}]", "").toLowerCase();
      String sentIdTitle = sentId + "\t" + fill;
      if (sentIdTitleToOrg.containsKey(sentIdTitle)) {
        String org = sentIdTitleToOrg.get(sentIdTitle);
        String canonicalOrg = mle.isMapped(org) ? 
            mle.expand(org) : mle.expand(org.toLowerCase());
        canonicalOrg = canonicalOrg.replaceAll("[^\\p{L}\\p{N}]", "").toLowerCase();
        // append canonical as part of canonical fill.
        canonicalFill += "\t" + canonicalOrg;
      }
    } else {
      // Default case: map by link-expander, lowercase.
      canonicalFill = mle.isMapped(fill) ? 
          mle.expand(fill) : mle.expand(fill.toLowerCase());
      canonicalFill = canonicalFill.replaceAll("[^\\p{L}\\p{N}]", "").toLowerCase();          
    }
    return canonicalFill;
  }
  
  public static String normalize(String fill, MaxLinkEntityExpander mle, 
      String rel) {
    return normalize(fill, mle, rel, null, null);
  }
  
  public void writeResponse(BufferedWriter bw) throws IOException {
    writeResponse("lsv", bw);
  }
  
  public Collection<Response2012> getResponses2012() {
    return tupleToResponses.values();
  }

  public void addResponseFromCandidate(Candidate c, String teamId, double score) {
    addResponse2012(c.getQid(), c.getRel(), teamId, c.getTextId().toString(), 
        c.getFiller(), 0, 0, 0, 0, score);
  }

  public void removeSlots(String disallowedSlotsFn) throws IOException {
    Multimap<String, String> relToDisallowedSlotVals = HashMultimap.create();
    BufferedReader br = new BufferedReader(new FileReader(disallowedSlotsFn));
    for (String line; (line = br.readLine()) != null;) {
      if (line.startsWith("#") || line.isEmpty()) {
        continue;
      }
      String[] relSlot = line.split(" ", 2);
      relToDisallowedSlotVals.put(relSlot[0], normalize(relSlot[1]));
    }
    br.close();
    // Remove responses that contain disalloed slot.
    Multimap<String, Response2012> tupleToAllowedResponses = 
        HashMultimap.create();
    for (Query q : ql.getQueries()) {
      for (String rel : q.getRelations()) {
        Collection<String> disallowed = relToDisallowedSlotVals.get(rel);
        String tuple = q.getId() + "\t" + rel;
        for (Response2012 r : tupleToResponses.get(tuple)) {
          String normalizedSlotfill = normalize(r.getSlotFill());
          if (!disallowed.contains(normalizedSlotfill)) {
            tupleToAllowedResponses.put(tuple, r);
          }
        }
      }
    }
    tupleToResponses = tupleToAllowedResponses;
  }

  private String normalize(String fill) {
    return normalize(fill, null, "");
  }


}
