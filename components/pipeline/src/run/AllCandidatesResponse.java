package run;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;

import query.QueryList;
import util.Candidate;
import util.Responses;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;

public class AllCandidatesResponse {
  /**
   * This writes out a response containing every slot filler in the candidates
   * file. A fixed, user-specified score is assigned.
   * Candidates are read from stdion, response is written to stdout.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 2) {
      System.err.println("AllCandidatesResponse <query_expanded_xml> <score>");
      return;
    }
    QueryList ql = new QueryList(args[0]);
    Double score = Double.parseDouble(args[1]);
    
    Responses r = new Responses(ql);
    
    BufferedReader candBr = new BufferedReader(
        new InputStreamReader(System.in, "UTF-8"));
    for (String line; (line = candBr.readLine()) != null;) {
      Candidate cand = Candidate.fromDelimLine(line);
      
      r.addResponse2012(cand.getQid(), cand.getRel(), "lsv", 
          cand.getIdentifier(), cand.getFiller(), 0, 0, 0, 0, score);
    }
    candBr.close();
    
    BufferedWriter outWriter = new BufferedWriter(
        new OutputStreamWriter(System.out, Charset.forName("UTF-8").newEncoder()));
    r.writeResponse(outWriter);
    outWriter.flush();
  }
}
