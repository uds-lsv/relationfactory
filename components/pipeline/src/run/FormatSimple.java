package run;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;

import query.QueryList;
import query.QueryList.Query;
import util.TextIdentifier;

public class FormatSimple {
  
  // maximum length of sentence in tokens. 
  // Sentences longer than this will be excluded.
  private static int SENTENCE_MAXTOKENS = 100;

  private static void writeLine(Writer out, String line, 
      TextIdentifier docQuerySnrId) throws IOException {
    String[] tokens = line.split(" ");
    if (tokens.length > SENTENCE_MAXTOKENS) {
      return;
    }
    out.write("<D=" + docQuerySnrId.toValidString() + ">\n");
    for (String token : tokens) {
      out.write(token + "\n");
    }
    out.write("</D>\n");
  }
  
  /**
   * A stream with one sentence per line is read in, sentences are printed out, formatted.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    BufferedWriter out = new BufferedWriter(new OutputStreamWriter(System.out, 
        "UTF-8"));
    BufferedReader br = new BufferedReader(new InputStreamReader(System.in, 
        "UTF-8"));
        
    int sNr = 1;
    TextIdentifier docQueryId = null;
    for (String line; (line = br.readLine()) != null;) {
      if (line.startsWith("#")) {
        docQueryId = TextIdentifier.fromDelimited(line.substring(1));
        if (docQueryId.getQueryId() == null) {
          docQueryId.setQueryId("");
        }
        sNr = 1;
      } else {
        assert(docQueryId != null);
        TextIdentifier docQuerySnrId = 
            new TextIdentifier(docQueryId).setSentNr(sNr);
        writeLine(out, line, new TextIdentifier(docQueryId).setSentNr(sNr));
        sNr += 1;
      }
    }
    out.flush();
    br.close();
  }
}