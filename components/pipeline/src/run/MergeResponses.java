package run;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import query.QueryList;
import util.Responses;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;

public class MergeResponses {
/**
 * This merges several responses.
 * No redundancy elimination whatsoever is done, so literally same slots may
 * be written out, even redundantly for the same sentence. Therefore, 
 * RedundancyEliminator needs to be called in order to obtain a final response.
 * 
 * @param args
 * @throws IOException
 */
public static void main(String[] args) throws IOException {
  if (args.length < 2) {
    System.err.println("MergeResponses " +
    		"<query_expanded_xml> <team_id> <response>*");
    System.err.println("A merged response is written to stdout.");
    System.err.println("If one or no responses is given, only NILs are filled up.");
    return;
  }
  
  QueryList ql = new QueryList(args[0]);

  Responses r = new Responses(ql);
  
  String teamId = args[1];
  List<String> responseFns = new ArrayList<String>(args.length - 2);
  for (int i = 2; i < args.length; ++i) {
    responseFns.add(args[i]);
  }

  for (String responseFn : responseFns) {
    r.readFile2012(responseFn);
  }
  
  BufferedWriter bw = new BufferedWriter(
      new OutputStreamWriter(System.out, Charset.forName("UTF-8").newEncoder()));
  r.writeResponse(teamId, bw);
  bw.flush();
}
}
