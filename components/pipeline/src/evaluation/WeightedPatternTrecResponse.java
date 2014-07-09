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


public class WeightedPatternTrecResponse {
  static final String TREC_KEY_ALL = "all";
  static final String TREC_KEY_REL = "rel";
  static final String TREC_KEY_QUERY = "qid";
  
  public static void main(String[] args) throws IOException, ParseException {
    Options options = new Options();
    options.addOption("c", true, "input (tab'ed) candidates file");
    options.addOption("o", true, "output trec-response file");
    options.addOption("p", true, "weighted (fast) patterns (no star, no left-right context)");
    options.addOption("k", true, "key to group trec queries, one of 'all', 'rel' (default) and 'qid'");
    options.addOption("s", false, "flag to indicate that shortened patterns are to ne used");
    CommandLineParser parser = new BasicParser();
    CommandLine cl = parser.parse( options, args );
    String trecKeyType = TREC_KEY_REL;
    if (cl.hasOption("k")) {
      trecKeyType = cl.getOptionValue("k");
    }
    
    // Read patterns.
    Map<String, Double> patToWeight = new HashMap<String, Double>();
    BufferedReader br = new BufferedReader(new FileReader(cl.getOptionValue("p")));
    for (String line; (line = br.readLine()) != null;) {
      String[] parts = line.split(" ",2);
      patToWeight.put(parts[1], Double.parseDouble(parts[0]));
    }
    br.close();

    if (cl.hasOption("s")) {
      System.err.println("Using shortened patterns.");
    } else {
      System.err.println("Using plain patterns.");
    }

    Map<String, Double> responseBodyToMaxWeight = new HashMap<String, Double>();
    br = new BufferedReader(new FileReader(cl.getOptionValue("c")));
    for (String line; (line = br.readLine()) != null;) {
      String pattern;
      if (cl.hasOption("s")) {
        pattern = PatternMetric.patternShortenedFromLine(line);
      } else {
        pattern = PatternMetric.patternFromLine(line);
      }
      if (patToWeight.containsKey(pattern)) {
        //SF2     org:date_dissolved      2007    AFP_ENG_20080408.0582.LDC2009T13.3      23      26      12      13      The number of trademark registration applications filed by foreigners totalled 103,000 in 2007 , making up 14.5 percent of the total , the China News Agency reported on its website , citing figures from an industry forum .
        String[] parts = line.split("\t");
        String qid = parts[0];
        String relation = parts[1];
        String slotLower = parts[2].toLowerCase();
        // trec_top_file (retrieved docs) format:
        // 030  Q0  ZF08-175-870  0   4238   prise1
        // qid iter   docno      rank  sim   run_id
        // body is everything up to (excluding) sim
        String body;
        if (trecKeyType.equals(TREC_KEY_QUERY)) {
          body = qid + "\tQ0\t" + relation + ":" + slotLower.replace(" ", "_") + "\t0\t";  
        } else if (trecKeyType.equals(TREC_KEY_REL)) {
          body = relation + "\tQ0\t" + qid + ":" + slotLower.replace(" ", "_") + "\t0\t";          
        } else /*if (trecKeyType.equals(TREC_KEY_ALL))*/ {
          // one relevance set.
          body = "all\tQ0\t" + qid + ":" + relation + ":" + slotLower.replace(" ", "_") + "\t0\t";
        }
        double maxWeight = responseBodyToMaxWeight.containsKey(body) ? 
            responseBodyToMaxWeight.get(body) : 0.0;
        maxWeight = Math.max(patToWeight.get(pattern), maxWeight);
        responseBodyToMaxWeight.put(body, maxWeight);
      }
    }
    br.close();
    BufferedWriter bw = new BufferedWriter(new FileWriter(cl.getOptionValue("o")));
    for (String body : responseBodyToMaxWeight.keySet()) {
      bw.append(body + responseBodyToMaxWeight.get(body) + "\tweighted_patterns\n");
    }
    bw.close();
  }

}
