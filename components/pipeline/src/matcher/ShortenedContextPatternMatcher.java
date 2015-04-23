package matcher;

import rerac.protos.Corpus;
import training.PatternMetric;
import util.DocumentExtractor;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by beroth on 4/23/15.
 */
public class ShortenedContextPatternMatcher extends FastContextPatternMatcher {

  public ShortenedContextPatternMatcher(Matcher argOneMatcher,
                                   Matcher argTwoMatcher, List<String> patterns) {
    super(argOneMatcher, argTwoMatcher, patterns);
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
  public List<String> arguments(Corpus.Document sentence, boolean optimize) {
    if(optimize) {
      throw new UnsupportedOperationException("Optimization not implemented.");
    }

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
          String[] tokens = DocumentExtractor.textFromTokens(sentence).split(" ");
          String compareTo = PatternMetric.patternShortenedFromTokens(tokens,
              argOneStart, argOneEnd, argTwoStart, argTwoEnd);
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