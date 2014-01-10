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

import org.apache.commons.collections.buffer.CircularFifoBuffer;

public class Format {
  
  // maximum length of sentence in tokens. 
  // Sentences longer than this will be excluded.
  private static int SENTENCE_MAXTOKENS = 100;
  
  private static boolean hasQueryMatch(Query q, String sentence) {
    sentence = " " + sentence + " ";
    if (sentence.contains(" " + q.getName() + " ")) {
      return true;
    }
    for (String alias : q.getAliases()) {
      if (sentence.contains(" " + alias + " ")) {
        return true;
      }
    }
    return false;
  }
  
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
   * A stream with one sentence per line is read in, sentences containing
   * a query token (or alias thereof) are retained and printed out, formatted.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length < 1 || args.length > 2) {
      System.err.println("Format <query_expanded.xml> [extraction_window:int]");
      System.err.println("A stream with one sentece per line is read in, sentences containing");
      System.err.println("a query token are retained and printed out, formatted.");
      System.err.println("The optional extraction_window parameter must be an integer between -1" +
          " and inf. It specifies the number of sentences printed out before and " +
          " after a sentence containing a query. E.g., extraction_window=1 will" +
          " print sentences immediately preceding and following query sentences." +
          " If set to -1, all sentences will be printed. Default is 0.");
      return;
    }
    BufferedWriter out = new BufferedWriter(new OutputStreamWriter(System.out, 
        "UTF-8"));
    BufferedReader br = new BufferedReader(new InputStreamReader(System.in, 
        "UTF-8"));
    QueryList ql = new QueryList(args[0]);
    int extractionWin = 0;
    if (args.length > 1) {
      try {
        extractionWin = Integer.parseInt(args[1]);
        if (extractionWin < -1) {
          throw new NumberFormatException();
        }
      } catch(NumberFormatException e) {
        System.err.println("Extraction window must be an integer between -1" +
        		" and inf.");
        return;
      }
    }
    
    // look behind
    CircularFifoBuffer lastSentences = null;
    if (extractionWin > 0) {
      lastSentences = new CircularFifoBuffer(extractionWin);
    }
    // look forward
    int currentExtractionWin = 0;
    
    int sNr = 1;
    TextIdentifier docQueryId = null;
    for (String line; (line = br.readLine()) != null;) {
      if (line.startsWith("#")) {
        docQueryId = TextIdentifier.fromDelimited(line.substring(1));
        if (docQueryId.getQueryId() == null || docQueryId.getQueryId().
            isEmpty()) {
          throw new IllegalArgumentException("TextIdentifier without query id:"
              + line);
        }
        sNr = 1;
        if (lastSentences != null) {
          lastSentences.clear();
        }
        currentExtractionWin = 0;
      } else {
        assert(docQueryId != null);
        TextIdentifier docQuerySnrId = 
            new TextIdentifier(docQueryId).setSentNr(sNr);
        try {
          if (extractionWin == -1 || 
              hasQueryMatch(ql.getQueryById(docQuerySnrId.getQueryId()), line)) {
            // old sentences in buffer
            if (lastSentences != null) {
              int oldSNr = sNr - lastSentences.size();
              while (lastSentences.size() > 0) {
                Object lineStrObject = lastSentences.remove();
                writeLine(out, (String)lineStrObject, 
                    new TextIdentifier(docQueryId).setSentNr(oldSNr++));
              }
            }
            // current sentence
            writeLine(out, line, new TextIdentifier(docQueryId).setSentNr(sNr));
            currentExtractionWin = extractionWin;
          } else {  // no match in sentence
            // within look-forward window of last query-containing sentence
            if (currentExtractionWin > 0) {
              writeLine(out, line, 
                  new TextIdentifier(docQueryId).setSentNr(sNr));
              --currentExtractionWin;
            } else {  // possibly in look-behind window
              if (lastSentences != null) {
                lastSentences.add(line);
              }
            }
          }
        } catch (IllegalArgumentException e) {
          throw new IllegalArgumentException("Error processing sentence with id:" +
          		" \n" + docQueryId + "\n"  + docQuerySnrId + "\n" + e);
        }
        sNr += 1;
      }
    }
    out.flush();
    br.close();
  }
}
