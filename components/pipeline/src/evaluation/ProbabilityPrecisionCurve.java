package evaluation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.cli.BasicParser;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

@Deprecated // WARNING: this is not a valid evaluation in most senses.
public class ProbabilityPrecisionCurve {
  // WARNING: this is not a valid evaluation in most senses.
  public static void main(String[] args) throws ParseException, IOException {
    Options options = new Options();
    options.addOption("k", true, "trec key.");
    options.addOption("r", true, "trec response, with score corresponding to probabilities.");
    options.addOption("c", false, "true: cumulative threshold evaluation, false: binned intervals");
    
    CommandLineParser parser = new BasicParser();
    CommandLine cl = parser.parse( options, args );
    
    boolean cumulative = cl.hasOption("c");
    
    double[] boundaries = 
        new double[]{0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1};
    
    int[] correct = new int[boundaries.length];
    int[] total = new int[boundaries.length];
    
    Set<String> keys = new HashSet<String>();
    BufferedReader br = new BufferedReader(new FileReader(cl.getOptionValue("k")));
    for (String line; (line = br.readLine()) != null;) {
      // per:title Q0 SF11:press_secretary 1
      keys.add(line);
    }
    br.close();
    
    br = new BufferedReader(new FileReader(cl.getOptionValue("r")));
    for (String line; (line = br.readLine()) != null;) {
      // org:top_members_employees       Q0      SF540:fenway_park       0       0.055944886566100485    weighted_patterns
      String[] parts = line.split("\t");
      String key = parts[0] + " " + parts[1] + " " + parts[2] + " 1";
      double prob = Double.parseDouble(parts[4]);
      boolean isCorrect = keys.contains(key);
      for (int i = 0; i < boundaries.length - 1; ++i) {
        if (prob >= boundaries[i] && (cumulative || prob < boundaries[i+1])) {
          total[i] += 1;
          correct[i] += isCorrect ? 1 : 0;
        }
      }
    }
    br.close();
    
    for (int i = 0; i < boundaries.length; ++i) {
      System.out.println(boundaries[i] + "\t" + correct[i] + " / " + total[i] + "\t" + (correct[i] / (double) total[i]));
    }
  }
}
