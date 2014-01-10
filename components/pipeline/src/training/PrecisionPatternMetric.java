package training;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.HashMap;
import java.util.Map;

import com.google.common.collect.HashMultiset;

public class PrecisionPatternMetric extends PatternMetric {
  public static final boolean UNIQUE = false;
  Map<String, HashMap<String, HashMultiset<String>>> posDataPatt;
  
  public PrecisionPatternMetric(String trainFile) throws IOException {
    posDataPatt = getPatterns(trainFile);
  }

  /**
   * Reads in a training data file and extracts patterns. Each pattern candidate
   * is stored together with the relation and arguments it occurs with.
   * 
   * @param trainFile
   * @return extracted patterns
   * @throws IOException
   */
  public static Map<String, HashMap<String, HashMultiset<String>>> getPatterns(
      String trainFile) throws IOException {

    // TODO (beroth): better way of naming maps: fooToBarToBaz
    Map<String, HashMap<String, HashMultiset<String>>> patterns = 
        new HashMap<String, HashMap<String, HashMultiset<String>>>();
    LineNumberReader br = new LineNumberReader(new FileReader(trainFile));

    for (String line; (line = br.readLine()) != null;) {
      // reading in the training data file and extracting patterns
      line = line.trim();

      if (!line.startsWith("#") && !line.isEmpty()) {
        String[] lineFields = line.split("\t");

        if (lineFields.length != NUM_FIELDS) {
          System.err.println("Incorrect number of fields on line "
              + br.getLineNumber());
          continue;
        }

        int begArg1 = Integer.parseInt(lineFields[BEG_ARG1]);
        int endArg1 = Integer.parseInt(lineFields[END_ARG1]);
        int begArg2 = Integer.parseInt(lineFields[BEG_ARG2]);
        int endArg2 = Integer.parseInt(lineFields[END_ARG2]);

        if (begArg1 >= endArg2 || begArg2 >= endArg1) {
          /*
           * if there is no argument overlap, determine the exact pattern
           * location:
           */
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
          String[] patternContext = lineFields[SENTENCE_ID].split("\\s+");
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
          String patternKey = pattern.toString();

          // extract the arguments of the pattern
          String arg1 = lineFields[ARG1_ID];
          String arg2 = lineFields[ARG2_ID];
          // TODO (beroth): I think this reordering is not what we want/need -- the 
          // pattern already should determine which of the arguments comes first.
          String argPair = (argAppearsFirst.equals(ARG1)) ? (arg1 + '#' + arg2)
              : (arg2 + '#' + arg1);

          // store the pattern, relation and arguments
          if (!patterns.containsKey(patternKey)) {
            // TODO (beroth): rename to match map semantics.
            HashMap<String, HashMultiset<String>> relations = 
                new HashMap<String, HashMultiset<String>>();
            HashMultiset<String> relArgs = HashMultiset.create();
            relArgs.add(argPair);
            relations.put(lineFields[REL_ID], relArgs);
            patterns.put(patternKey, relations);
          } else {
            // TODO (beroth): rename to match map semantics.
            HashMap<String, HashMultiset<String>> entry = patterns
                .get(patternKey);
            if (!entry.containsKey(lineFields[REL_ID])) {
              HashMultiset<String> relArgs = HashMultiset.create();
              relArgs.add(argPair);
              entry.put(lineFields[REL_ID], relArgs);
            } else {
              entry.get(lineFields[REL_ID]).add(argPair);
            }
          }
        }
      }
    }
    br.close();
    return patterns;
  }

  /**
   * A helper method returning the correct count of the elements in a
   * HashMultiset argSet, given the unique pattern arguments option.
   */
  private static double getFreq(HashMultiset<String> argSet, boolean unique) {
    return (double)((unique) ? argSet.elementSet().size() : argSet.size());
  }
  
  @Override
  protected double score(String relPattern) {
    String rel = relPattern.split(" ", 2)[0];
    String pattern = relPattern.split(" ", 2)[1];
    HashMap<String, HashMultiset<String>> relations = posDataPatt.get(pattern);
    if (null == relations) {
      System.err.println(relPattern);
    }
    if (null == relations.get(rel)) {
      System.err.println(relPattern);
      System.err.println(relations);
    }
    // Count for all relations.
    double pattCount = 0.0;
    for (String relation : relations.keySet()) {
      pattCount += getFreq(relations.get(relation), UNIQUE);
    }
    return getFreq(relations.get(rel), UNIQUE) / pattCount;
  }
  
  
  public static void main(String[] args) throws IOException {
    if (args.length != 4) {
      System.err.println("PrecisionPatternMetric <train_sentences> <min_freq> <min_score> <print_score=true|false>");
      return;
    }
    String trainFn = args[0];
    int minFreq = Integer.parseInt(args[1]);
    double minScore = Double.parseDouble(args[2]);
    boolean printScore = Boolean.parseBoolean(args[3]);
    
    PrecisionPatternMetric patternMiner = new PrecisionPatternMetric(trainFn);
    BufferedReader br = new BufferedReader(new FileReader(trainFn));
    for (String minedPattern : patternMiner.getAllPatterns(br, minFreq, minScore)) {
      if (printScore) {
        System.out.print(patternMiner.score(minedPattern) + " ");
      }
      System.out.println(minedPattern);
    }
    br.close();
  }
}
