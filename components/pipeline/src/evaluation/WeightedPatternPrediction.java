package evaluation;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.cli.BasicParser;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import training.PatternMetric;


public class WeightedPatternPrediction {
  /**
   * This produces a prediction file according to weighted patterns.
   * 
   * TODO: move to run package?
   * @param args
   * @throws IOException
   * @throws ParseException
   */
  public static void main(String[] args) throws IOException, ParseException {
    Options options = new Options();
    options.addOption("c", true, "input (tab'ed) candidates file");
    options.addOption("o", true, "output prediction file");
    options.addOption("p", true, "weighted (fast) patterns (no star, no left-right context)");
    CommandLineParser parser = new BasicParser();
    CommandLine cl = parser.parse( options, args );
    
    // Read patterns.
    Map<String, Double> patToWeight = new HashMap<String, Double>();
    BufferedReader br = new BufferedReader(new FileReader(cl.getOptionValue("p")));
    for (String line; (line = br.readLine()) != null;) {
      String[] parts = line.split(" ",2);
      patToWeight.put(parts[1], Double.parseDouble(parts[0]));
    }
    br.close();
    BufferedWriter bw = new BufferedWriter(new FileWriter(cl.getOptionValue("o")));
    br = new BufferedReader(new FileReader(cl.getOptionValue("c")));
    for (String line; (line = br.readLine()) != null;) {
      String pattern = PatternMetric.patternFromLine(line);
      double score = patToWeight.containsKey(pattern) ? 
          patToWeight.get(pattern) : 0.0;
      String[] parts = line.split("\t");
      bw.append(parts[0] + "\t" +
          parts[1] + "\t" +
          parts[2] + "\t" +
          parts[3] + "\t" +
          parts[4] + "\t" +
          parts[5] + "\t" +
          parts[6] + "\t" +
          parts[7] + "\t" +
          score);
      bw.newLine();
    }
    br.close();
    bw.close();
  }

}
