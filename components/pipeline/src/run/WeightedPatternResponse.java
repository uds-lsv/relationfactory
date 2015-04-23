package run;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

import query.QueryList;
import training.PatternMetric;
import util.Candidate;
import util.Responses;

public class WeightedPatternResponse {
  public static void main(String[] args) throws IOException {
    if (args.length != 3 && args.length != 4 && args.length != 5) {
      System.err.println("WeightedPatternResponse " +
          "<query_expanded_xml> <candidates> <weighted_patterns> [<min_score>] [<shortened=true|false>]");
      System.err.println("A response is written to Stdout.");
      return;
    }
    
    QueryList ql = new QueryList(args[0]);
    String candidatesFn = args[1];
    String patFn = args[2];
    double minScore = 0.0;
    if (args.length >= 4) {
      minScore = Double.parseDouble(args[3]);
    }
    boolean shortened = false;
    if (args.length == 5) {
      shortened = args[4] == "true";
    }

      // Read patterns.
    Map<String, Double> patToWeight = new HashMap<String, Double>();
    BufferedReader br = new BufferedReader(new FileReader(patFn));
    for (String line; (line = br.readLine()) != null;) {
      String[] parts = line.split(" ",2);
      // Special case that can occur due to merging of relations:
      // two conflicting pattern weights - take bigger one.
      double w = Double.parseDouble(parts[0]);
      w = patToWeight.containsKey(parts[1]) ? 
          Math.max(w, patToWeight.get(parts[1])) : w;
      patToWeight.put(parts[1], w);
    }
    br.close();
    
    Responses r = new Responses(ql);

    br = new BufferedReader(new FileReader(candidatesFn));
    for (String line; (line = br.readLine()) != null;) {
      String pattern;
      if (shortened) {
        pattern = PatternMetric.patternShortenedFromLine(line);
      } else {
        pattern = PatternMetric.patternFromLine(line);
      }
      if (!patToWeight.containsKey(pattern)) {
        continue;
      }
      double score = patToWeight.get(pattern);
      if (score < minScore) {
        continue;
      }
      //SF2     org:date_dissolved      2007    AFP_ENG_20080408.0582.LDC2009T13.3      23      26      12      13      The number of trademark registration applications filed by foreigners totalled 103,000 in 2007 , making up 14.5 percent of the total , the China News Agency reported on its website , citing figures from an industry forum .
      Candidate cand = Candidate.fromDelimLine(line);
      r.addResponse2012(cand.getQid(), cand.getRel(), "lsv", 
          cand.getIdentifier(), cand.getFiller(), 0, 0, 0, 0, score);
    }
    br.close();
    
    BufferedWriter outWriter = new BufferedWriter(
        new OutputStreamWriter(System.out, Charset.forName("UTF-8").newEncoder()));
    r.writeResponse(outWriter);
    outWriter.flush();
  }
}
