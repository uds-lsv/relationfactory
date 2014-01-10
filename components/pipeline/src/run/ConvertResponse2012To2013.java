package run;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import util.TextIdentifier;

public class ConvertResponse2012To2013 {
  /**
   * This converts a response from 'pseudo' 2012 format into 2013 format.
   * The 'pseudo' 2012 format contains, instead of a bare document id, the
   * document_id + '.' + offset_annotation.
   * This offset annotation is then used to create the 2013 response.
   * 
   * The offset annotation is of the form:
   * 
   * filler_start-filler_end:query_start-query_end:sentence_start-sentence_end
   * 
   * There can be also two support sentences (the second one separated by 
   * ',').
   * 
   * For example, 'pseudo' 2012 format:
   * 
   * SF_ENG_079      org:top_members_employees       lsv     AFP_ENG_20080109.0097.LDC2009T13.3.434-444:425-630:400-500        Gerrie Nel      434     444     425     630     0.285211685892435
   * 
   * Is converted into 2013 format:
   * 
   * SF_ENG_079      org:top_members_employees       lsv     AFP_ENG_20080109.0097.LDC2009T13        Gerrie Nel      434-444     425-630     400-500     0.285211685892435
   * 
   * @param args
   * @throws IOException 
   */
  public static void main(String[] args) throws IOException {
    BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
    for (String line; (line = br.readLine()) != null;) {
      String[] parts = line.split("\t");
      if ("NIL".equals(parts[3])) {
        System.out.println(line);
      } else {
        //String[] docOffsets = parts[3].split(":");
        TextIdentifier tid = TextIdentifier.fromDelimited(parts[3]);
        System.out.println(parts[0] + "\t" + parts[1] + "\t" +parts[2] + "\t" + 
            tid.getDocId() + "\t" + parts[4] + "\t" + tid.getFillerOffsets() + "\t" + 
            tid.getQueryOffsets() + "\t" + tid.getSentenceOffsets() + "\t" + 
            parts[9]);
      }
    }
    br.close();
  }
}
