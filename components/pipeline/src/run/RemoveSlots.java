package run;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;

import query.QueryList;
import util.Responses;

public class RemoveSlots {
  public static void main(String[] args) throws IOException {
    if (args.length != 2) {
      System.err.println("RemoveSlots <query_xml> <disallowed_slots>");
      return;
    }
    QueryList ql = new QueryList(args[0]);
    String disallowedSlotsFn = args[1];
    
    Responses r = new Responses(ql);
    BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
    r.readResponse2012(br);
    br.close();

    r.removeSlots(disallowedSlotsFn);
    BufferedWriter bw = new BufferedWriter(
        new OutputStreamWriter(System.out, Charset.forName("UTF-8").newEncoder()));
    r.writeResponse(bw);
    bw.flush();
  }
}
