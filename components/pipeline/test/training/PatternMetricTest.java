package training;

import org.apache.log4j.Level;
import org.junit.BeforeClass;
import org.junit.Test;
import query.QueryList;
import testutil.JunitTestBase;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

/**
 * Created by beroth on 4/22/15.
 */
public class PatternMetricTest extends JunitTestBase {

  @BeforeClass
  public static void setUpClass() throws Exception {
    //PatternMetric.logger.setLevel(Level.ALL);
  }

  @Test
  public void testPatternFromLine() throws IOException {
    String context1 = "E1\trel\tE2\tdocid\t0\t2\t5\t7\tA B C D E F G H";
    assertEquals(PatternMetric.patternFromLine(context1), "rel $ARG1 C D E $ARG2");

    String context2 = "E1\trel\tE2\tdocid\t5\t7\t0\t2\tA B C D E F G H";
    assertEquals(PatternMetric.patternFromLine(context2), "rel $ARG2 C D E $ARG1");

    String context3 = "E1\trel\tE2\tdocid\t0\t2\t10\t12\tA B C X X X X X D E F G H";
    assertEquals(PatternMetric.patternFromLine(context3), "rel $ARG1 C X X X X X D E $ARG2");
  }

  @Test
  public void testShortenedPatternFromLine() throws IOException {
    String context1 = "E1\trel\tE2\tdocid\t0\t2\t5\t7\tA B C D E F G H";
    assertEquals(PatternMetric.patternShortenedFromLine(context1), "rel $ARG1 C D E $ARG2");

    String context2 = "E1\trel\tE2\tdocid\t5\t7\t0\t2\tA B C D E F G H";
    assertEquals(PatternMetric.patternShortenedFromLine(context2), "rel $ARG2 C D E $ARG1");

    String context3 = "E1\trel\tE2\tdocid\t0\t2\t7\t9\tA B C X X X E F G H";
    assertEquals(PatternMetric.patternShortenedFromLine(context3), "rel $ARG1 C X [0] X E $ARG2");

    String context4 = "E1\trel\tE2\tdocid\t7\t9\t0\t2\tA B C X X X E F G H";
    assertEquals(PatternMetric.patternShortenedFromLine(context4), "rel $ARG2 C X [0] X E $ARG1");

    String context5 = "E1\trel\tE2\tdocid\t0\t2\t8\t10\tA B C X X X X E F G H";
    assertEquals(PatternMetric.patternShortenedFromLine(context5), "rel $ARG1 C X [1] X E $ARG2");
  }
}