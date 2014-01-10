package matcher;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;


import rerac.protos.Corpus.Document;
import util.DocumentExtractor;

public class StarContextPatternMatcher implements ContextPatternMatcher {
  Matcher argOneMatcher;
  Matcher argTwoMatcher;
  List<Pattern> patterns = new ArrayList<Pattern>();
  // This is used to replace a star by a regex that matches 1-4 words.
  final static String STAR_REGEX = "[^ ]+( [^ ]+){0,3}";
  // This matches escaped and unescaped stars.
  final static Pattern STAR_MATCH_PATTERN = Pattern
      .compile("(^| )(\\\\?\\*)( |$)");
  boolean isReflexive = false;
  int[] diagnoseStat;
  int newsFactor = 1;

  public StarContextPatternMatcher(Matcher argOneMatcher, 
      Matcher argTwoMatcher, List<String> patterns) {
    //Pattern star = Pattern.compile(Pattern.quote("*"));
    this.argOneMatcher = argOneMatcher;
    this.argTwoMatcher = argTwoMatcher;
    for (String pStr : patterns) {
      if (pStr.contains("$ARG0")) {
        isReflexive = true;
      }
      
      // Deal with stars in patterns.
      java.util.regex.Matcher m = STAR_MATCH_PATTERN.matcher(pStr);
      StringBuffer sb = new StringBuffer();
      int start = 0;
      while (m.find(start)) {
        String star = m.group(2);
        if ("*".equals(star)) {
          // Unescaped star: expand to token sequence regex.
          String quoted = Pattern.quote(pStr.substring(start, m.start(2)));
          sb.append(quoted);
          sb.append(STAR_REGEX);
        } else if ("\\*".equals(star)) {
          // Escaped star: include as star-token.
          String quoted = Pattern.quote(
              pStr.substring(start, m.start(2)) + "*");
          sb.append(quoted);
        } else {
          throw new IllegalStateException("Impossible regex match.");
        }
        start = m.end(2);
      }
      sb.append(Pattern.quote(pStr.substring(start)));
      String regexStr = sb.toString();
      
      // Insert optional determiner.
      regexStr = regexStr.replace("$ARG", "\\E" + "(the )?" + "\\Q$ARG");
      this.patterns.add(Pattern.compile(regexStr));
    }
    diagnoseStat = new int[this.patterns.size()];
  }

  /**
   * This returns argument Strings that matched a pattern (arg1 and arg2
   * alternating). Optimization turned on can save computation of valid argument
   * candidates. Optimization is dependent on document id, and will not work if
   * there are different (= not exactly the same) documents with the same id.
   * (Actually errors should only occur if those docs are read consecutively).
   * 
   * @param sentence
   * @param optimize
   * @return
   */
  public List<String> arguments(Document sentence, boolean optimize) {
    // We don't necessarily expect any arguments to match.
    List<String> args = new ArrayList<String>(0);
    List<Integer> argOneStartEnds = argOneMatcher.maximalMatches(sentence,
        optimize);
    List<Integer> argTwoStartEnds = argTwoMatcher.maximalMatches(sentence,
        optimize);
    for (int i1 = 0; i1 < argOneStartEnds.size() - 1; i1 += 2) {
      int argOneStart = argOneStartEnds.get(i1);
      int argOneEnd = argOneStartEnds.get(i1 + 1);
      for (int i2 = 0; i2 < argTwoStartEnds.size() - 1; i2 += 2) {
        int argTwoStart = argTwoStartEnds.get(i2);
        int argTwoEnd = argTwoStartEnds.get(i2 + 1);
        // No overlap.
        if (argTwoStart >= argOneEnd || argOneStart >= argTwoEnd) {
          int firstStart = argOneStart < argTwoStart ? argOneStart
              : argTwoStart;
          int secondStart = argOneStart > argTwoStart ? argOneStart
              : argTwoStart;
          int firstEnd = argOneEnd < argTwoEnd ? argOneEnd : argTwoEnd;
          int secondEnd = argOneEnd > argTwoEnd ? argOneEnd : argTwoEnd;
          String firstWildcard = argOneStart < argTwoStart ? " $ARG1 "
              : " $ARG2 ";
          String secondWildcard = argOneStart >= argTwoStart ? " $ARG1 "
              : " $ARG2 ";
          StringBuffer wildcarded = new StringBuffer();
          wildcarded.append(DocumentExtractor.textFromTokens(sentence, 0,
              firstStart));
          wildcarded.append(firstWildcard);
          wildcarded.append(DocumentExtractor.textFromTokens(sentence,
              firstEnd, secondStart));
          wildcarded.append(secondWildcard);
          wildcarded.append(DocumentExtractor.textFromTokens(sentence,
              secondEnd, sentence.getTokenCount()));
          String compareTo = wildcarded.toString().trim().replace("  ", " ");
          if (isReflexive) {
            compareTo = compareTo.replace("$ARG1", "$ARG0").replace("$ARG2",
                "$ARG0");
          }
          for (int pNr = 0; pNr < patterns.size(); ++pNr) {
            Pattern p = patterns.get(pNr);
            java.util.regex.Matcher m = p.matcher(compareTo);
            if (m.find()) {
              args.add(DocumentExtractor.textFromTokens(sentence, argOneStart,
                  argOneEnd));
              args.add(DocumentExtractor.textFromTokens(sentence, argTwoStart,
                  argTwoEnd));
              diagnoseStat[pNr] += 1;
              break;
            }
          }

        }
      }
    }
    return args;
  }

  public boolean matches(String s) {
    for (int pNr = 0; pNr < patterns.size(); ++pNr) {
      Pattern p = patterns.get(pNr);
      java.util.regex.Matcher m = p.matcher(s);
      if (m.find()) {
        return true;
      }
    }
    return false;
  }

  public boolean hasNews() {
    int nm = 0;
    for (int n : diagnoseStat) {
      nm += n;
    }
    boolean hasNews = (nm % newsFactor) == 0;
    if (hasNews) {
      newsFactor *= 10;
    }
    return hasNews;
  }

  public void patternThinning() {
    int nm = 0;
    for (int n : diagnoseStat) {
      nm += n;
    }
    if (nm >= 100) {
      List<Pattern> frequentPatterns = new ArrayList<Pattern>(patterns.size());
      for (int i = 0; i < patterns.size(); ++i) {
        if ((diagnoseStat[i] * 100) / nm > 0) {
          diagnoseStat[frequentPatterns.size()] = diagnoseStat[i];
          frequentPatterns.add(patterns.get(i));
        }
      }
      patterns = frequentPatterns;
      diagnoseStat = Arrays.copyOf(diagnoseStat, patterns.size());
    }
  }

  public String diagnosis(boolean allPatterns) {
    StringBuffer sb = new StringBuffer();
    for (int pNr = 0; pNr < patterns.size(); ++pNr) {
      if (diagnoseStat[pNr] > 0 || allPatterns) {
        sb.append(diagnoseStat[pNr]);
        sb.append("\t");
        sb.append(patterns.get(pNr).toString());
        sb.append("\n");
      }
    }
    return sb.toString();
  }

  public String diagnosis() {
    return diagnosis(false);
  }

  public static void main(String[] args) {
    List<String> patterns = new ArrayList<String>();
    patterns.add("$ARG1 , who has a * in * from $ARG2");
    patterns.add("$ARG1 \\* $ARG2 in");
    patterns.add("\\* $ARG2 in $ARG1");
    patterns.add("$ARG1 $ARG2");
    StarContextPatternMatcher cpm = new StarContextPatternMatcher(null, null, patterns);
    System.out
        .println(cpm
            .matches("$ARG1 , who has a masters degree in computer engineering from the $ARG2 ."));
    System.out.println(cpm.matches("$ARG1 $ARG2 ."));
    System.out.println(cpm.matches("$ARG1 . $ARG2"));
    System.out.println(cpm.matches("$ARG1 * $ARG2 in New York"));
    System.out.println(cpm.matches("$ARG1 died $ARG2 in New York"));
    System.out.println(cpm.matches("* $ARG2 in $ARG1 *"));
    
    System.out.println(cpm.diagnosis(true));

  }

}
