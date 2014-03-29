package run;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;

import util.Candidate;
import util.DscoreFile;
import util.OffsetPair;
import util.TextIdentifier;
import corpus.CorpusReader;
import corpus.RawCorpusDocument;

public class MatchRealOffsets {

  static Logger logger = Logger.getLogger(MatchRealOffsets.class.getName());

  private static final String whitespace_chars =  ""       /* dummy empty string for homogeneity */
      + "\\u0009" // CHARACTER TABULATION
      + "\\u000A" // LINE FEED (LF)
      + "\\u000B" // LINE TABULATION
      + "\\u000C" // FORM FEED (FF)
      + "\\u000D" // CARRIAGE RETURN (CR)
      + "\\u0020" // SPACE
      + "\\u0085" // NEXT LINE (NEL) 
      + "\\u00A0" // NO-BREAK SPACE
      + "\\u1680" // OGHAM SPACE MARK
      + "\\u180E" // MONGOLIAN VOWEL SEPARATOR
      + "\\u2000" // EN QUAD 
      + "\\u2001" // EM QUAD 
      + "\\u2002" // EN SPACE
      + "\\u2003" // EM SPACE
      + "\\u2004" // THREE-PER-EM SPACE
      + "\\u2005" // FOUR-PER-EM SPACE
      + "\\u2006" // SIX-PER-EM SPACE
      + "\\u2007" // FIGURE SPACE
      + "\\u2008" // PUNCTUATION SPACE
      + "\\u2009" // THIN SPACE
      + "\\u200A" // HAIR SPACE
      + "\\u2028" // LINE SEPARATOR
      + "\\u2029" // PARAGRAPH SEPARATOR
      + "\\u202F" // NARROW NO-BREAK SPACE
      + "\\u205F" // MEDIUM MATHEMATICAL SPACE
      + "\\u3000" // IDEOGRAPHIC SPACE
      ;        
  /* A \s that actually works for Java’s native character set: Unicode */
  private static final String     WHITESPACE_CLASS = 
      "["  + whitespace_chars + "]";

  // maximum length of sentence in chars. 
  // For sentences longer than this, no matching will take place.
  private static int SENTENCE_MAXCHARS = 2000;

  // Map: DocId -> path
  private Map<String, String> docpaths;
  // Map: DocId -> Sentence -> List of candidates with this sentence
  // Only used by printCandidatedWithOffsets ff.
  private Map<String, Map<String, List<Candidate>>> candidates;
  private int nonmatched = 0;

  public MatchRealOffsets(String dscoreFn) throws IOException {
    docpaths = new DscoreFile(dscoreFn).idToFileMap();
  }

  public void printCandidatesWithOffsets(BufferedReader candidatesBr) 
      throws IOException {
    candidates = new HashMap<String, Map<String,List<Candidate>>>();
    int numCands = 0;
    for (String candLine; (candLine = candidatesBr.readLine()) != null;) {
      Candidate cand = null;
      try {
        cand = Candidate.fromDelimLine(candLine);
      } catch (IllegalArgumentException e) {
        logger.fatal("Line in candidates file was invalid, skipping: " + 
            candLine);
        throw new IllegalArgumentException("Line in candidates file was invalid, skipping: " + 
            candLine);
      }
      // strip sentence number
      TextIdentifier candId = TextIdentifier.fromDelimited(cand.getIdentifier());
      String sentence = cand.getSentence();
      Map<String, List<Candidate>> toMatchInDoc = 
          candidates.get(candId.getDocId());
      if (toMatchInDoc == null) {
        toMatchInDoc = new HashMap<String, List<Candidate>>();
        candidates.put(candId.getDocId(), toMatchInDoc);
      }

      List<Candidate> matchEntries = toMatchInDoc.get(sentence);
      if (matchEntries == null) {
        matchEntries = new ArrayList<Candidate>();
        toMatchInDoc.put(sentence, matchEntries);
      }
      numCands++;
      matchEntries.add(cand);
    }
    logger.debug("Number of candidates read in: " + numCands);
    logger.debug("Number of docids in candidates: " + candidates.size());
    if (candidates == null || candidates.size() == 0) {
      logger.error("No candidates loaded.");
      //throw new IllegalStateException("No candidates loaded, abort.");
    }
    Set<String> filesToOpen = new HashSet<String>();

    for (String docid : candidates.keySet()) {
      String path = docpaths.get(docid);
      if (path == null || path.isEmpty()) {
        throw new IllegalStateException("Path empty for docid " + docid);
      }
      filesToOpen.add(path);
    }

    BufferedWriter out = new BufferedWriter(new OutputStreamWriter(System.out, 
        "UTF-8"));
    for (String path : filesToOpen) {
      logger.debug("Reading files in " + path);
      for (RawCorpusDocument doc : CorpusReader.readDocumentsRaw(path)) {
        // Found document in file which was not selected by indexing.
        // this is ok (multiple docs per file)
        if (!docpaths.containsKey(doc.getId())) {
          if (candidates.containsKey(doc.getId())) {
            throw new IllegalStateException("No docpath given for id " + doc.getId());
          }
          continue;
        }
        try {
          writeMatchesInDocument(doc.getRawContent(), doc.getId(), out);
        } catch (IllegalArgumentException e) {
          throw new IllegalArgumentException("Problem matching " +
              "in document at " + path + ": " + e.getMessage());
        }
      }
    }
    out.flush();
    logger.info("Non-matched sentences " + nonmatched); //+ ", multimatched " + 
    //multimatched);
  }

    /*
  public List<OffsetPair> matchSentenceAndArgsFromDocid(String docId, 
      String sentence, String... args) {
    String docfn = docpaths.get(docId);
    if (docfn == null) {
      throw new IllegalArgumentException("no doc filename given for docId " + docId);
    }
    RawCorpusDocument doc = readDocumentFromFile(docfn, docId);
    return matchSentenceAndArgs(doc.getRawContent(), sentence, args);
  }
  */

  public static List<OffsetPair> matchSentenceAndArgs(String docSgml, 
      String sentence, String... args) {
    if (sentence.isEmpty() || docSgml.isEmpty()) {
      throw new IllegalArgumentException("empty sentence or empty sgml");
    }
    if (sentence.length() > SENTENCE_MAXCHARS) {
      logger.warn("Sentence longer than " + SENTENCE_MAXCHARS + ". Skipping.");
      return null;
    }
    OffsetPair sentOffsets = offsetsMatch(docSgml, sentence);
    if (sentOffsets == null) {
      throw new IllegalArgumentException("sentence not found in sgml:\n" +
          sentence + 
          "\nsgml:\n" + docSgml);
    }
    List<OffsetPair> ret = new LinkedList<OffsetPair>();
    ret.add(sentOffsets);
    String sentenceOrig = docSgml.substring(sentOffsets.start, 
        sentOffsets.end + 1 // correcting for TAC-style offset
        );
    for (String arg : args) {
      ret.add(offsetsMatch(sentenceOrig, arg));
    }
    return ret;
  }

  private static Matcher match(String haystack, String needle) {
    //Pattern.quote(needle)
    //String regex = "\\Q" + needle.replaceAll("(\\W+)", "\\\\E\\\\W*\\\\Q") + 
    //    "\\E";
    //String regex = "\\Q" + needle.replaceAll("(\\s+)", "\\\\E\\\\s*\\\\Q") + 
    //    "\\E";
    StringBuilder regexBuilder = new StringBuilder();
    for (String part : needle.split(" ")) {
      if (regexBuilder.length() > 0) {
        //regexBuilder.append("[^\\w.!?]*");
        regexBuilder.append(WHITESPACE_CLASS + "*");
      }
      //part = part.replaceAll("[^\\w.?!]", "\\E[^\\\\w.?!]*\\Q");
      part = part.replace("&", "\\E(&amp;|&)\\Q")
          .replace("<", "\\E(&lt;|<)\\Q")
          .replace(">", "\\E(&gt;|>)\\Q")
          .replaceAll("(\"|``|''|')", "\\\\E(&quot;|&apos;|$1|\")\\\\Q")
          //.replaceAll("'", "\\E(&apos;|')\\Q")
          /*.replace("''", "\\E(&quot;|[^\\w]{1,2})\\Q")
          .replace("``", "\\E(&quot;|[^\\w]{1,2})\\Q")*/
          .replace("\uFFFD", "\\E.?\\Q")
          .replace("...@", " ... @"); // undo email splitting
      if (!part.startsWith("\\Q")) {
        regexBuilder.append("\\Q");
      }
      regexBuilder.append(part);
      if (!part.endsWith("\\E")) {
        regexBuilder.append("\\E");
      }
    }
    /*String regex = "\\Q" + needle.replaceAll("([^\\w.]+)", "\\\\E[^\\\\w.]*\\\\Q") + 
        "\\E";*/
    String regex = regexBuilder.toString();
    /*regex = regex.replace("&", "&amp;").replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace("''", "&quot;")
        .replace("``", "&quot;")
        .replace("\"", "&quot;")
        .replace("'", "&apos;");*/
    //regex = regex.replace("\\Q\\E", "");
    logger.debug("Search for: " + needle);
    logger.debug("As Regex: " + regex);
    Matcher m = Pattern.compile(regex).matcher(haystack);
    return m;
  }

  private static OffsetPair offsetsMatch(String haystack, String needle) {
    if (needle.isEmpty()) {
      logger.info("Empty needle!");
      return null;
    }

    Matcher m = match(haystack, needle);
    if (!m.find()) {
      //m = match(haystack, needle);
      //if (!m.find()) {
      return null;
      //}
    }

    // The offset interval is inclusive, i.e. the end offset points at the 
    // last character of the matched substring -> subtract 1.
    OffsetPair ret = new OffsetPair(m.start(), m.end() - 1);
    /*if (m.find()) {
      logger.debug("matched multiple times! " + needle);
      multimatched++;
    }*/
    return ret;
  }

  private void writeMatchesInDocument(String doc, String id, Writer out) 
      throws IOException {
    assert(candidates != null);
    if (id.isEmpty()) {
      throw new IllegalArgumentException("Document without id, ignoring: " + doc);
    }
    if (doc.isEmpty()) {
      throw new IllegalArgumentException("Document without content, ignoring: " + id);
    }
    Map<String, List<Candidate>> sentences = candidates.get(id);
    if (sentences == null) {
      logger.debug("No sentences for document id " + id);
      return;
    }
    logger.debug("Working on document " + id);
    for (Entry<String, List<Candidate>> entry : sentences.entrySet()) {
      String sentence = entry.getKey();
      logger.debug("Working on sentence " + sentence);
      List<Candidate> cands = entry.getValue();
      if (cands.size() == 0) {
        logger.warn("No candidates for sentence " + sentence + " in " + 
            doc);
      }
      String[] args = new String[cands.size() * 2];
      for (int i = 0; i < cands.size(); i++) {
        args[i * 2] = cands.get(i).getTarget();
        args[i * 2 + 1] = cands.get(i).getFiller();
      	logger.debug("Will search for query=" + args[i * 2] + " and slot=" + args[i * 2 + 1]);
      	if (cands.get(i).getTarget().isEmpty()) {
      	  if (cands.get(i).getRel().endsWith("alternate_names")) {
      	    logger.info("Empty query: " + cands.get(i));
      	  } else {
      	    logger.warn("Empty query: " + cands.get(i));
      	  }
      	}
      }
      List<OffsetPair> offsets = null;
      try {
        offsets = matchSentenceAndArgs(doc, sentence, 
            args);
      } catch (IllegalArgumentException e) {
        logger.warn(e);
        nonmatched++;
        continue;
      }
      if (offsets == null) {
        logger.warn("No match for sentence " + sentence + " in " + doc);
        nonmatched++;
        continue;
      }
      assert(offsets.size() == args.length + 1);
      OffsetPair sentOffsets = offsets.get(0);
      if (sentOffsets == null) {
        logger.warn("No match for sentence " + sentence + " in " + doc);
        continue;
      }

      for (int i = 0; i < cands.size(); i++) {
        OffsetPair queryOffsets = offsets.get(i * 2 + 1);
        String queryOffsetStr;
        String sentOffsetStr = sentOffsets.start + "-" + sentOffsets.end;
        if (queryOffsets == null) {
          if (cands.get(i).getRel().endsWith("alternate_names")) {
            logger.info("No match for query " + args[i * 2] + " in sentence " + 
                sentence);
            // Don't report sentence offsets for alternate names without queries.
            // sentOffsetStr = "";
          } else {
            logger.warn("No match for query " + args[i * 2] + " in sentence " + 
                sentence);            
          }
          queryOffsetStr = "";
        } else {
          queryOffsetStr = (sentOffsets.start + queryOffsets.start) 
              + "-" + (sentOffsets.start + queryOffsets.end);
        }
        OffsetPair fillerOffsets = offsets.get(i * 2 + 2);
        if (fillerOffsets == null) {
          logger.warn("No match for filler " + args[i * 2 + 1] + " in sentence " + 
              sentence);
          continue;
        }
        String fillerOffsetStr = (sentOffsets.start + fillerOffsets.start) + 
            "-" + (sentOffsets.start + fillerOffsets.end);

        Candidate outCandidate = new Candidate(cands.get(i));
        TextIdentifier idWithOffsets = 
            TextIdentifier.fromDelimited(outCandidate.getIdentifier());
        idWithOffsets.setOffsets(fillerOffsetStr + 
            ":" + queryOffsetStr + 
            ":" + sentOffsetStr);
        logger.debug(idWithOffsets.toString());
        outCandidate.setIdentifier(idWithOffsets.toValidString());
        out.write(outCandidate + "\n");
      }
    }
  }

  private RawCorpusDocument readDocumentFromFile(String docfn, 
      String docidToSelect) {
    for (RawCorpusDocument doc : CorpusReader.readDocumentsRaw(docfn)) {
      if (doc.getId().equals(docidToSelect)) {
        return doc;
      }
    }
    return null;
  }

  /**
   * Original candidates are read from stdin, with offset written to stdout.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 1) {
      System.err.println("java MatchRealOffsets <dscore>");
      return;
    }
    MatchRealOffsets mro = new MatchRealOffsets(args[0]);
    BufferedReader br = new BufferedReader(
        new InputStreamReader(System.in, "UTF-8"));
    mro.printCandidatesWithOffsets(br);
    br.close();
  }

}
