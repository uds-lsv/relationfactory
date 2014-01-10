package run;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class TaggingCorrector {
  static final String NO_TAG = "O";
  
  // Entity must start with letter, number or dollar (for money amounts).
  static final Pattern ILLEGAL_ENTITY_START = Pattern.compile("^[^\\p{L}\\w$]");
  // Inside entity now brackets or tokens starting with '.'
  static final Pattern ILLEGAL_ENTITY_TOKEN_START = Pattern.compile("^[().]");

  //  static final Pattern ILLEGAL_ENTITY_TOKEN_END = Pattern.compile("[()]");
//  static final Pattern STARTS_WITH_NON_LETTER = Pattern.compile("^[^\\p{L}\\w]");
//  static final Pattern ENDS_WITH_NON_LETTER = Pattern.compile("[^\\p{L}\\w.]$");

  
  public static void main(String[] args) throws IOException {
    // Read in dtag file from stdin
    BufferedReader in = new BufferedReader(new InputStreamReader(System.in, 
        "UTF-8"));
    BufferedWriter out = new BufferedWriter(new OutputStreamWriter(System.out, 
        "UTF-8"));
    
    List<String> lines = new ArrayList<String>();
    for (String line; (line = in.readLine()) != null; ) {
      if (line.startsWith("<D")) {
        out.write(line);
        out.newLine();
      } else if (line.startsWith("</D")) {
        writeCorrectedLines(lines, out);
        out.write(line);
        out.newLine();
        lines.clear();
      } else {
        lines.add(line);
      }
    }
    out.flush();
  }

  // Original Python code for response correction:
    /*
nonletter_start = re.compile(ur'^\W+', re.UNICODE)
# trailing '.' is ok
nonletter_end = re.compile(ur'[^\w.]+$', re.UNICODE)
def main():
  for line in sys.stdin.xreadlines():
      line = line.strip()
      fields = line.split("\t")
      if len(fields) < 4:
          print >> sys.stderr, "unexpected line:"
          print >> sys.stderr, line
      docid = fields[3]
      orig_fill = "" if docid == "NIL" else fields[4]
      # remove everything after "()"
      clean_fill = re.split("[\(\)]",orig_fill)[0].strip()
      clean_fill = nonletter_start.sub('', clean_fill)
      clean_fill = nonletter_end.sub('', clean_fill)
      
      if docid == "NIL" or clean_fill == "":
          print line
      else:
          print "\t".join(fields[0:4] + [clean_fill] + fields[5:])
     */
  private static void writeCorrectedLines(List<String> lines, BufferedWriter out) throws IOException {
    String previousTag = NO_TAG;
    for (String l : lines) {
      String[] tokenTag = l.split(" ");
      String token = tokenTag[0];
      String tag = tokenTag[1];
      
      // correct B-I-O:
      // tag can only start with I- if before there was of same type before.
      // otherwise it must be "B-"
      if (tag.startsWith("I-")) {
        String tagType = tag.split("-",2)[1];
        if (previousTag.equals(NO_TAG) 
            || !tagType.equals(previousTag.split("-",2)[1])) {
          tag = "B-" + tagType;          
        }
      }
      
      // Correct bracketing, noisy tokens:
      // Everything that does not start or end with a letter (ending with '.'
      // permitted) is not tagged. 
      if (!tag.equals(NO_TAG)) {
        if ((tag.startsWith("B-") && ILLEGAL_ENTITY_START.matcher(token).find())
            || ILLEGAL_ENTITY_TOKEN_START.matcher(token).find()) {
          tag = NO_TAG;
        }
      }
      // Write out line with corrected tag.
      out.write(token + " " + tag);
      out.newLine();
      
      previousTag = tag;
    }
  }
}
