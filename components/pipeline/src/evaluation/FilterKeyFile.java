package evaluation;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.cli.BasicParser;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import training.PatternMetric;

public class FilterKeyFile {
  static final String FORMAT_TAC = "tac";
  static final String FORMAT_TREC = "trec";
  static final String TREC_KEY_ALL = "all";
  static final String TREC_KEY_REL = "rel";
  static final String TREC_KEY_QUERY = "qid";
  
  /**
   * This filters a TAC key-file according to whether answers can be obtained
   * given a set of candidates or not. (i.e. answers that are not in the set
   * of candidates are thrown out). When writing the key, there is the choice 
   * between TAC and TREC key format. For the TREC style per-query (ranking) 
   * evaluations, the choice exist on how to group elements for aggregate
   * scores (per relation, query id, or everything together).
   * 
   * @param args
   * @throws ParseException
   * @throws IOException
   */
  public static void main(String[] args) throws ParseException, IOException {
    Options options = new Options();
    options.addOption("i", true, "input key file");
    options.addOption("c", true, "input (tab'ed) candidates file");
    options.addOption("o", true, "output key file");
    options.addOption("p", true, "filter by (fast) patterns from (tab'ed) training data");
    options.addOption("f", true, "format, one of 'tac' (default) or 'trec'");
    options.addOption("k", true, "key to group trec queries, one of 'all', 'rel' (default) and 'qid'");
    options.addOption("s", false, "flag to indicate that shortened patterns are to ne used");
    CommandLineParser parser = new BasicParser();
    CommandLine cl = parser.parse( options, args );
    
    String format = FORMAT_TAC;
    if (cl.hasOption("f")) {
      format = cl.getOptionValue("f");
    }
    String trecKeyType = TREC_KEY_REL;
    if (cl.hasOption("k")) {
      trecKeyType = cl.getOptionValue("k");
    }
    
    Set<String> patternSet = null;
    // validate that block-size has been set
    if( cl.hasOption( "p" ) ) {
      patternSet = new HashSet<String>();
      BufferedReader br = new BufferedReader(new FileReader(cl.getOptionValue("p")));
      for (String line; (line = br.readLine()) != null;) {
        String pattern;
        if (cl.hasOption("s")) {
          pattern = PatternMetric.patternShortenedFromLine(line);
        } else {
          pattern = PatternMetric.patternFromLine(line);
        }
        patternSet.add(pattern);
      }
      br.close();
    }
    
    // attainable answers, lowercased
    Set<String> queryRelAnswers = new HashSet<String>();
    BufferedReader br = new BufferedReader(new FileReader(cl.getOptionValue("c")));
    for (String line; (line = br.readLine()) != null;) {
      String pattern;
      if (cl.hasOption("s")) {
        pattern = PatternMetric.patternShortenedFromLine(line);
      } else {
        pattern = PatternMetric.patternFromLine(line);
      }
      if (patternSet != null && 
          !patternSet.contains(pattern)) {
        // line not matchable;
        continue;
      }
      //SF2     org:date_dissolved      2007    AFP_ENG_20080408.0582.LDC2009T13.3      23      26      12      13      The number of trademark registration applications filed by foreigners totalled 103,000 in 2007 , making up 14.5 percent of the total , the China News Agency reported on its website , citing figures from an industry forum .
      String[] parts = line.split("\t");
      String qid = parts[0];
      String rel = parts[1];
      String slotLower = parts[2].toLowerCase();
      queryRelAnswers.add(qid + "\t" + rel + "\t" + slotLower);
    }    
    br.close();
    
    br = new BufferedReader(new FileReader(cl.getOptionValue("i")));
    BufferedWriter bw = new BufferedWriter(new FileWriter(cl.getOptionValue("o")));
    
    // To avoid written out duplicate lines.
    Set<String> writtenLines = new HashSet<String>();
    for (String line; (line = br.readLine()) != null;) {
      //9 SF599:per:title LTW_ENG_20070710.0005.LDC2009T13  1 966 analyst 0 0:0 0 0:0
      String[] parts = line.split("\t");
      String qid = parts[1].split(":",2)[0];
      String relation = parts[1].split(":",2)[1];
      String relevance = parts[3];
      String slotLower = parts[5].toLowerCase();
      if (queryRelAnswers.contains(qid + "\t" + relation + "\t" + slotLower)) {
      if (format.equals(FORMAT_TREC) && relevance.equals("1")) {
        //text_qrels_file (key) format:
        //qid  iter  docno  rel
        String keyLine;
        if (trecKeyType.equals(TREC_KEY_QUERY)) {
          keyLine = qid + " Q0 " + relation + ":" + slotLower.replace(" ", "_") + " " + relevance;          
        } else if (trecKeyType.equals(TREC_KEY_REL)) {
          keyLine = relation + " Q0 " + qid + ":" + slotLower.replace(" ", "_") + " " + relevance;          
        } else /*if (trecKeyType.equals(TREC_KEY_ALL))*/ {
          // one relevance set.
          keyLine = "all Q0 " + qid + ":" + relation + ":" + slotLower.replace(" ", "_") + " " + relevance;
        }
        if (!writtenLines.contains(keyLine)) {
          bw.append(keyLine);
          bw.newLine();
          writtenLines.add(keyLine);
        }
      } else if (format.equals(FORMAT_TAC)) {
          bw.append(line);
          bw.newLine();
        }
      }
    }
    bw.close();
    br.close();
  }
}
