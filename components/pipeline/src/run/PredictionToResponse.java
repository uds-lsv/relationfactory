package run;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

import query.QueryList;
import query.QueryList.Query;
import util.Responses;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;

public class PredictionToResponse {
      
/**
 * Prints out a response from a prediction file as it is e.g. produced by the
 * classifier.
 * No redundancy elimination whatsoever is done, so literally same slots may
 * be written out. Therefore, RedundancyEliminator needs to be called in order
 * to obtain a final response.
 * 
 * 2013 compatible.
 * 
 * TODO: testing.
 * 
 * @param args
 * @throws IOException
 */
public static void main(String[] args) throws IOException {
  if (args.length != 3) {
    // TODO: remove 'teamid' and use standard teamid 'lsv' throughout.
    System.err.println("Response " +
        "<query_expanded_xml> <prediction> <team_id>");
    return;
  }
  
  QueryList ql = new QueryList(args[0]);
  String predFn = args[1];
  String teamId = args[2];
  
  Responses r = new Responses(ql);
  
  LineNumberReader spredBr = new LineNumberReader(new FileReader(predFn));
  for (String line; (line = spredBr.readLine()) != null;) {
    String[] fields = line.split("\t");
    if (fields.length != 9) {
      throw new IllegalArgumentException("Unexpected line " + 
          spredBr.getLineNumber() + " in score file:\n" + line);
    }
    
    double score = Double.parseDouble(fields[8]);
    
    if (score > 0) {
      String qid = fields[0];
      String relation = fields[1];
      String slot = fields[2];
      String sentenceId = fields[3];
      String tuple = qid + "\t" + relation;
      r.addResponse2012(qid, relation, teamId, sentenceId, slot, 0, 0, 0, 0, 
          score);
    }
  }
  spredBr.close();
  
  BufferedWriter outWriter = new BufferedWriter(
      new OutputStreamWriter(System.out, Charset.forName("UTF-8").newEncoder()));
  r.writeResponse(teamId, outWriter);
  outWriter.flush();
}
}
