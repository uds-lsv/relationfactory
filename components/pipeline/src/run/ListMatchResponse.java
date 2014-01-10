package run;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.HashSet;
import java.util.Set;

import query.QueryList;
import util.Candidate;
import util.Responses;
import entity_expansion.MaxLinkEntityExpander;

public class ListMatchResponse {
  
  // A low default score since other methods that use context should be 
  // prioritized.
  private static final double DEFAULT_SCORE = 0.1;
  
  public static void main(String[] args) throws IOException {
    if (args.length != 4) {
      System.err.println("Arguments expected: 4; Arguments provided: " + args.length);
      System.err.println("ListMatchResponse <query_expanded_xml> <kb_slots> <link_stats> <candidates>");
      return;
    }
    QueryList ql = new QueryList(args[0]);
    String slotFn = args[1];
    MaxLinkEntityExpander mle = new MaxLinkEntityExpander(args[2]);
    String candFn = args[3];
    
    
    Set<String> normTriplesFromList = new HashSet<String>();
    BufferedReader br = new BufferedReader(new FileReader(slotFn));
    for (String line; (line = br.readLine()) != null;) {
      String[] relQidSlot = line.split("\t");
      String normTriple = relQidSlot[1] + "\t" + relQidSlot[0] + "\t" + 
          Responses.normalize(relQidSlot[2], mle, relQidSlot[0]);
      normTriplesFromList.add(normTriple);
    }
    br.close();
    
    Responses r = new Responses(ql);
    
    BufferedReader candBr = new BufferedReader(new FileReader(candFn));
    for (String line; (line = candBr.readLine()) != null;) {
      Candidate cand = Candidate.fromDelimLine(line);
      String normTriple = cand.getQid() + "\t" + cand.getRel() + "\t" + 
          Responses.normalize(cand.getFiller(), mle, cand.getRel());
      if (normTriplesFromList.contains(normTriple)) {
        r.addResponse2012(cand.getQid(), cand.getRel(), "lsv", cand.getIdentifier(), 
            cand.getFiller(), 0, 0, 0, 0, DEFAULT_SCORE);
      }
    }
    candBr.close();
    
    BufferedWriter outWriter = new BufferedWriter(
        new OutputStreamWriter(System.out, Charset.forName("UTF-8").newEncoder()));
    r.writeResponse(outWriter);
    outWriter.flush();
  }
}
