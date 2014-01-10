package matcher;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

import rerac.protos.Corpus.Document;
import util.DocumentExtractor;

public class FastContextPatternMatcher implements ContextPatternMatcher {
  Matcher argOneMatcher;
  Matcher argTwoMatcher;
  Set<String> patterns = new HashSet<String>();

  public FastContextPatternMatcher(Matcher argOneMatcher, 
      Matcher argTwoMatcher, List<String> patterns) {
    //Pattern star = Pattern.compile(Pattern.quote("*"));
    this.argOneMatcher = argOneMatcher;
    this.argTwoMatcher = argTwoMatcher;
    for (String pStr : patterns) {
      if (pStr.contains("$ARG0")) {
        String pStr12 = pStr.replaceFirst(Pattern.quote("$ARG0 "), java.util.regex.Matcher.quoteReplacement("$ARG1 ")).
            replaceFirst(Pattern.quote(" $ARG0"), java.util.regex.Matcher.quoteReplacement(" $ARG2"));
        String pStr21 = pStr.replaceFirst(Pattern.quote("$ARG0 "), java.util.regex.Matcher.quoteReplacement("$ARG2 ")).
            replaceFirst(Pattern.quote(" $ARG0"), java.util.regex.Matcher.quoteReplacement(" $ARG1"));
        this.patterns.add(pStr12);
        this.patterns.add(pStr21);
      } else {
        this.patterns.add(pStr);
      }
    }
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
          int secondStart = argOneStart > argTwoStart ? argOneStart
              : argTwoStart;
          int firstEnd = argOneEnd < argTwoEnd ? argOneEnd : argTwoEnd;
          String firstWildcard = argOneStart < argTwoStart ? " $ARG1 "
              : " $ARG2 ";
          String secondWildcard = argOneStart >= argTwoStart ? " $ARG1 "
              : " $ARG2 ";
          StringBuffer wildcarded = new StringBuffer();
          wildcarded.append(firstWildcard);
          wildcarded.append(DocumentExtractor.textFromTokens(sentence,
              firstEnd, secondStart));
          wildcarded.append(secondWildcard);
          String compareTo = wildcarded.toString().trim().replace("  ", " ");
          if (patterns.contains(compareTo)) {
            args.add(DocumentExtractor.textFromTokens(sentence, argOneStart,
                argOneEnd));
            args.add(DocumentExtractor.textFromTokens(sentence, argTwoStart,
                argTwoEnd));
          }
        }
      }
    }
    return args;
  }
}
