package run;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ExtractText {
  /**
   * Extracts text from one document, specified by filename and doc id.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 2) {
      System.err.println("java ExtractText <filename> <docid>");
      return;
    }
    
    String filename = args[0];
    String docid = args[1];

    BufferedReader br = new BufferedReader(new FileReader(filename));
    StringBuilder text = new StringBuilder();
    String id = "";
    String sep = "";
    for (String line; (line = br.readLine()) != null;) {
      if (line.startsWith("<")) {
        if (line.startsWith("<DOCID>")) {
          // line.substring("<DOCID> ".length(), line.length() - " </DOCID>".length());
          id = line.substring(8, line.length() - 9);
        } else if (line.startsWith("<DOC id=")) {
          // <DOC id="WPB_ENG_20100301.0003" type="story">
          id = line.split("\"",3)[1];
        } else if (line.startsWith("</DOC>")) {
          if (id.equals(docid)) {
            System.out.println(text.toString());
            break;
          }
          text = new StringBuilder();
          id = "";
          sep = "";
        } else {
          sep = "\n\n";
        }
      } else {
        text.append(sep); sep = " ";
        text.append(line);
      }
    }
    br.close();
  }
}