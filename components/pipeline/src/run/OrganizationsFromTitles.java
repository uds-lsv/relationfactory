package run;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import query.QueryList;
import util.Candidate;
import util.Responses;
import util.Responses.Response2012;

public class OrganizationsFromTitles {
  
private static Set<String> getInferredQidSidOrgs(QueryList ql, Responses rs, 
    String titleOrgFn) throws IOException {
  Map<String, String> sidTitleToOrg = new HashMap<String, String>();
  BufferedReader br = new BufferedReader(new FileReader(titleOrgFn));
  for (String line; (line = br.readLine()) != null;) {
    String[] sidTitleOrg = line.split("\t");
    if (sidTitleOrg.length != 3) {
      throw new IllegalArgumentException("Expected 3 columns in " + 
          titleOrgFn + ", found: " + sidTitleOrg.length);
    }
    sidTitleToOrg.put(sidTitleOrg[0] + "\t" + sidTitleOrg[1], sidTitleOrg[2]);
  }

  Set<String> inferredQidSidOrgs = new HashSet<String>();
  for (Response2012 r : rs.getResponses2012()) {
    if (r.getRelation().equals("per:title")) {
      String sidTitle = r.getTextId().upToSnr() + "\t" + r.getSlotFill();
      if (sidTitleToOrg.containsKey(sidTitle)) {
        String qidSidOrg = r.getQid() + "\t" + r.getTextId().upToSnr() + "\t" + 
            sidTitleToOrg.get(sidTitle);
        inferredQidSidOrgs.add(qidSidOrg);
      }
    }
  }
  return inferredQidSidOrgs;
}
  
public static void main(String[] args) throws IOException {
  QueryList ql = new QueryList(args[0]);
  String candsFN = args[1];
  String responseFn = args[2]; // Response must not be postprocessed.
  String titleOrgFn = args[3];

  Responses rs = new Responses(ql); 
  rs.readFile2012(responseFn);
  
  Set<String> inferredQidSidOrgs = getInferredQidSidOrgs(ql, rs, titleOrgFn);
  BufferedReader br = new BufferedReader(new FileReader(candsFN));
  for (String line; (line = br.readLine()) != null;) {
    Candidate cand = Candidate.fromDelimLine(line);
    if (cand.getRel().equals("per:employee_or_member_of")){
      String qidSidOrg = cand.getQid() + "\t" + cand.getTextId().upToSnr() + 
          "\t" + cand.getFiller();
      if (inferredQidSidOrgs.contains(qidSidOrg)) {
        rs.addResponseFromCandidate(cand, "lsv", 0.1);
      }
    }
  }
  br.close();
  
  BufferedWriter bw = new BufferedWriter(
      new OutputStreamWriter(System.out, Charset.forName("UTF-8").newEncoder()));
  rs.writeResponse(bw);
  bw.flush();
}
}
