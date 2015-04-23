package training;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import com.google.common.collect.HashMultiset;
import com.google.common.collect.Multiset;

/**
 * This is the superclass for getting good patterns (according to some metric)
 * from training data.
 * 
 * Inheriting classes have to implement the score(String pattern) method that
 * defines how good a pattern is estimated to be.
 * 
 * @author Benjamin Roth
 *
 */
public abstract class PatternMetric {
  public static final int ARG1_ID = 0;
  public static final int REL_ID = 1;
  public static final int ARG2_ID = 2;
  // 3: doc id / sentence number
  public static final int BEG_ARG1 = 4;
  public static final int END_ARG1 = 5;
  public static final int BEG_ARG2 = 6;
  public static final int END_ARG2 = 7;
  public static final int SENTENCE_ID = 8;
  public static final int NUM_FIELDS = 9;
  public static final String ARG1 = "$ARG1";
  public static final String ARG2 = "$ARG2";
  
  /**
   * This gets a pattern definition as used in the pattern matcher file, i.e.
   * REL_NAME PATTERN
   * all white space separated.
   * 
   * @param line
   * @return pattern definition
   */
  public static String patternFromLine(String line){
    String[] lineFields = line.split("\t");

    if (lineFields.length != NUM_FIELDS) {
      throw new IllegalArgumentException(
          "Incorrect number of fields on line :\n" + line);
    }

    int begArg1 = Integer.parseInt(lineFields[BEG_ARG1]);
    int endArg1 = Integer.parseInt(lineFields[END_ARG1]);
    int begArg2 = Integer.parseInt(lineFields[BEG_ARG2]);
    int endArg2 = Integer.parseInt(lineFields[END_ARG2]);
    
    int patternBeg = endArg1;
    int patternEnd = begArg2;
    String argAppearsFirst = ARG1;
    String argAppearsSecond = ARG2;

    if (begArg1 > begArg2) {
      // if agr2 precedes arg1:
      patternBeg = endArg2;
      patternEnd = begArg1;
      argAppearsFirst = ARG2;
      argAppearsSecond = ARG1;
    }

    // extract the pattern:
    StringBuffer pattern = new StringBuffer(lineFields[REL_ID] + " " + argAppearsFirst + " ");
    String[] patternContext = lineFields[SENTENCE_ID].split("\\s+");
    
    // In case of overlap, patternEnd <= patternBeg, and the pattern will be
    // "ARG1 ARG2" or "ARG2 ARG1" depending on which pattern starts first.
    for (int i = patternBeg; i < patternEnd; i++) {
      if (patternContext[i].equals("*")) {
        // escape the star symbol (*)
        pattern.append("\\*");
      } else {
        pattern.append(patternContext[i]);
      }
      pattern.append(" ");
    }
    pattern.append(argAppearsSecond);
    return pattern.toString();
  }


  public static String patternShortenedFromTokens(String[] tokens,
                                                  int begArg1, int endArg1, int begArg2, int endArg2) {
    int patternBeg = endArg1;
    int patternEnd = begArg2;
    String argAppearsFirst = ARG1;
    String argAppearsSecond = ARG2;

    if (begArg1 > begArg2) {
      // if agr2 precedes arg1:
      patternBeg = endArg2;
      patternEnd = begArg1;
      argAppearsFirst = ARG2;
      argAppearsSecond = ARG1;
    }

    // extract the pattern:
    StringBuffer pattern = new StringBuffer(argAppearsFirst + " ");
    for (int i = patternBeg; i < patternEnd; i++) {
      if (i-patternBeg < 2 || patternEnd - i <= 2) {
        // positions near beginning or end
        pattern.append(tokens[i]);
      } else {
        //position in the middle
        int skippedOver = patternEnd - patternBeg - 4;
        int logBin = (int) (Math.log(skippedOver) / Math.log(2));
        pattern.append("[" + logBin + "]");
        i += skippedOver -1;
      }
      pattern.append(" ");
    }
    pattern.append(argAppearsSecond);
    return pattern.toString();

  }


  public static String patternShortenedFromLine(String line){
    String[] lineFields = line.split("\t");

    if (lineFields.length != NUM_FIELDS) {
      throw new IllegalArgumentException(
          "Incorrect number of fields on line :\n" + line);
    }

    int begArg1 = Integer.parseInt(lineFields[BEG_ARG1]);
    int endArg1 = Integer.parseInt(lineFields[END_ARG1]);
    int begArg2 = Integer.parseInt(lineFields[BEG_ARG2]);
    int endArg2 = Integer.parseInt(lineFields[END_ARG2]);

    String rel = lineFields[REL_ID];

    String[] tokens = lineFields[SENTENCE_ID].split("\\s+");

    String pattern = patternShortenedFromTokens(tokens, begArg1, endArg1, begArg2, endArg2);

    return rel + " " + pattern;
  }

  /**
   * This scores a pattern line as used in the pattern matcher file, i.e.
   * REL_NAME PATTERN
   * all white space separated.
   */
  protected abstract double score(String pattern);
  
  public double scoreCandidateSentence(String candidate) {
    return score(patternFromLine(candidate));
  }
  
  /**
   * This returns all patterns that occur with a minimum frequency and a minimum
   * score in the given tab-separated training data.
   * 
   * @param br
   * @param minFreq
   * @param minScore
   * @return
   * @throws IOException
   */
  public Set<String> getAllPatterns(BufferedReader br, int minFreq, 
      double minScore) throws IOException {
    Multiset<String> patternCount = HashMultiset.create();
    for (String line; (line = br.readLine()) != null;) {
      String pattern = patternFromLine(line);
      // Speedup: pattern does not need to be scored if already scored. 
      if (patternCount.contains(pattern) ||
          score(pattern) >= minScore) {
        //System.out.println(pattern);
        patternCount.add(pattern);
      }
    }
    Set<String> patterns = new HashSet<String>();
    for (String pattern : patternCount.elementSet()) {
      if (patternCount.count(pattern) >= minFreq) {
        patterns.add(pattern);
      }
    }
    return patterns;
  }
}
