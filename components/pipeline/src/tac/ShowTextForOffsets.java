package tac;


import java.io.*;
import java.io.FileReader;
import java.util.*;
import java.util.zip.GZIPInputStream;

/**
 *
 * Version 1.2, July 25, 2013
 * Fixed crash when entity or justification offsets are empty
 *
 * Version 1.1, July 11, 2013
 * Added support for TSF outputs
 *
 * Version 1.0
 * Extracts the text corresponding to offsets in a SF or TSF output and prints it in a column-based format
 * It also implements the following validation checks on the offsets:
 * - Error: Start offsets cannot be negative (this exists in the validation script too)
 * - Warning: End offsets cannot be outside of the file size
 * - Warning: for SF, filler tokens (Column 5) should be included in the tokens from the filler mentions (Column 6)
 *            date fillers are excluded from this check because they might be normalized
 * - Warning: justification should not be larger than 1000 characters
 * This does NOT perform validation of the entire output format! Use the validation scripts first!
 * User: mihais
 * Date: 6/25/13
 */
public class ShowTextForOffsets {
  static int errCount = 0;
  static int warnCount = 0;

  public static void main(String [] args) throws Exception {
    if(args.length != 4) {
      usage();
    }

    String task = args[0].toLowerCase();
    if(! task.equals("sf") && ! task.equals("tsf")) {
      usage();
    }
    String corpusPath = args[1];
    Map<String, DocOffset> docOffsets = readDocOffsets(args[2]);
    String submissionFile = args[3];

    PrintStream err = new PrintStream(new FileOutputStream(submissionFile + ".offset_errlog"));
    showText(err, corpusPath, docOffsets, submissionFile, task);
    err.close();
    if(errCount == 0 && warnCount == 0)
      System.err.println("Done. Validation successful.");
    else
      System.err.println("Done. Validation failed with " + errCount + " errors and " + warnCount + " warnings.");
    System.err.println("Validation errors/warnings were saved in file " + submissionFile + ".offset_errlog");
  }

  static void usage() {
    System.err.println("Usage: ShowTextForOffsets SF|TSF <corpus path> <doc offset file> <submission file>");
    System.err.println("Please include \"data/English\" or \"data/Spanish\" in the above corpus path!");
    System.err.println("Please set the first parameter to either SF or TSF, depending on submission type.");
    System.exit(1);
  }

  /**
   * Extracts the text behind the given offsets and creates a new file with these texts printed in a tab-separated format
   * The output file is name <submissionFile>.expanded. It includes all columns from the original submission plus one for each text.
   * @param corpusPath Full corpus path (include "/data/English"!)
   * @param docOffsets Map with document offset information
   * @param submissionFile The submission file to be processed
   * @param task Task type: SF or TSF
   */
  static void showText(
    PrintStream err,
    String corpusPath,
    Map<String, DocOffset> docOffsets,
    String submissionFile,
    String task) throws Exception {

    BufferedReader is = new BufferedReader(new FileReader(submissionFile));
    PrintStream os = new PrintStream(new FileOutputStream(submissionFile + ".expanded"));

    String line;
    int lineCount = 0;
    while ((line = is.readLine()) != null) {
      lineCount ++;
      SubmissionLine parsedLine = (task.equalsIgnoreCase("sf") ? parseSFLine(line) : parseTSFLine(line));
      os.print(line);
      if(parsedLine != null) {
        DocOffset docOffset = docOffsets.get(parsedLine.docid);
        if(docOffset == null) {
          throw new RuntimeException("ERROR: could not find offset info for document " + parsedLine.docid + ". This is a bug; please report it to the task organizers.");
        }
        String fileName = corpusPath + File.separator + docOffset.fileName;
        String fileContent = readRaw(new File(fileName));

        validateLine(err, lineCount, parsedLine, fileContent, docOffset, task);

        for(Offset offset: parsedLine.orderedOffsets) {
          os.print("\t");
          os.print(extractText(fileContent, docOffset.offset, offset));
        }
      }
      os.println();
    }

    is.close();
    os.close();
    System.err.println("The texts corresponding to the offsets in this submission file were saved in file " + submissionFile + ".expanded");
  }

  static final int MAX_JUSTIFICATION_SIZE = 1000;

  /**
   * Validates the offsets in one submission line
   * @param err Error output stream
   * @param lineCount Position of this line in the submission file
   * @param line Line to be validated
   * @param fileContent Content of the file that contains this document
   * @param  docOffset Offset of this document in fileContent
   */
  static void validateLine(
    PrintStream err,
    int lineCount,
    SubmissionLine line,
    String fileContent,
    DocOffset docOffset,
    String task) {

    // Check 1: make sure all start offsets are positive and all end offsets are within the file size
    for(Offset o: line.orderedOffsets) {
      if(o.start < 0) {
        errLog(err, lineCount, "found negative start offset " + o.start + ". Please run the validation script before this!");
      }
      if(o.end < o.start) {
        errLog(err, lineCount, "found end offset " + o.end + " smaller than start offset " + o.start + ". Please run the validation script before this!");
      }

      int end = docOffset.offset + o.end;
      if(end >= fileContent.length()) {
        warnLog(err, lineCount, "found end offset " + o.end + " + " + docOffset.offset + " larger than size of file " + docOffset.fileName);
      }
    }

    // Check 2: for SF and non-date fillers, the filler tokens must be included in the corresponding filler text
    if(task.equalsIgnoreCase("sf") && ! line.slotName.contains("date")) {
      Set<String> fillerTokens = tokenSet(line.filler);
      StringBuilder fillerText = new StringBuilder();
      for(Offset o: line.fillerOffsets) {
        fillerText.append(extractText(fileContent, docOffset.offset, o));
        fillerText.append(" ");
      }
      Set<String> textTokens = tokenSet(fillerText.toString());
      // System.err.println("Comparing set " + fillerTokens + " with " + textTokens);
      boolean included = true;
      for(String t: fillerTokens) {
        if(! textTokens.contains(t)) {
          included = false;
          break;
        }
      }
      if(! included) {
        warnLog(err, lineCount, "Filler value [" + line.filler + "] is not included in text given by filler offsets: [" + fillerText.toString() + "]");
      }
    }

    // Check 3: justification should not be too large
    int justSize = 0;
    for(Offset o: line.justificationOffsets) {
      justSize += o.end - o.start + 1;
    }
    if(justSize > MAX_JUSTIFICATION_SIZE) {
      warnLog(err, lineCount, "justification size " + justSize + " exceeds " + MAX_JUSTIFICATION_SIZE + " characters. Are you sure it contains at most 2 sentences?");
    }
  }

  static Set<String> tokenSet(String text) {
    String lowerCased = text.toLowerCase();
    String alphas = lowerCased.replaceAll("[^a-zA-Z0-9]", " ");
    String [] tokens = alphas.split("\\s+");
    Set<String> uniqueTokens = new HashSet<String>();
    for(String t: tokens) {
      uniqueTokens.add(t);
    }
    return uniqueTokens;
  }

  static void errLog(PrintStream err, int lineCount, String msg) {
    err.println("ERROR in line " + lineCount + ": " + msg);
    errCount ++;
  }
  static void warnLog(PrintStream err, int lineCount, String msg) {
    err.println("WARNING in line " + lineCount + ": " + msg);
    warnCount ++;
  }

  /**
   * Extracts the text corresponding to a given segment offset
   * @param fileContent Content of the source file
   * @param docOffset Offset of this doc in its file
   * @param textOffset Offset of the text to be extracted, relative to doc start
   * @return The text corresponding to textOffset
   */
  static String extractText(
    String fileContent,
    int docOffset,
    Offset textOffset) {

    // text offsets are relative to doc start, so we must add docOffset
    int start = Math.max(0, textOffset.start);
    int actualStart = docOffset + start;
    // end is the position of the last character in segment, so we must add 1
    int actualEnd = Math.min(fileContent.length(), docOffset + textOffset.end + 1);
    String rawText = fileContent.substring(actualStart, actualEnd);
    // replace all white spaces with space, to keep the tab-separated format valid
    return rawText.replaceAll("\\s", " ");
  }

  /**
   * Parses one line in a SF submission
   * @param line One SF submission line
   * @return All offsets in this line, in the order in which they appear
   */
  static SubmissionLine parseSFLine(String line) {
    String [] bits = line.split("\t");
    if(bits.length < 4) {
      throw new RuntimeException("ERROR: invalid SF submission line: " + line + "! Please run the validation script first.");
    }
    String docid = bits[3];
    if(docid.equals("NIL")) return null;
    String slotName = bits[1];
    String filler = bits[4];
    List<Offset> offsets = new ArrayList<Offset>();
    List<Offset> fillerOffsets = parseOffsets(bits[5]); // filler offsets
    offsets.addAll(fillerOffsets);
    offsets.addAll(parseOffsets(bits[6])); // entity offsets
    List<Offset> justificationOffsets = parseOffsets(bits[7]); // justification offsets
    offsets.addAll(justificationOffsets);

    return new SubmissionLine(slotName, docid, filler, fillerOffsets, justificationOffsets, offsets);
  }

  /**
   * Parses one line in a TSF submission
   * @param line One TSF submission line
   * @return All offsets in this line, in the order in which they appear
   */
  static SubmissionLine parseTSFLine(String line) {
    String [] bits = line.split("\t");
    if(bits.length < 4) {
      throw new RuntimeException("ERROR: invalid TSF submission line: " + line + "! Please run the validation script first.");
    }
    String type = bits[3];
    if(type.equals("NIL")) return null;
    String docid = bits[4];
    String slotName = bits[1];
    String filler = bits[8];
    List<Offset> offsets = new ArrayList<Offset>();
    List<Offset> fillerOffsets = parseOffsets(bits[5]); // filler offsets
    offsets.addAll(fillerOffsets);
    offsets.addAll(parseOffsets(bits[6])); // entity offsets
    List<Offset> justificationOffsets = parseOffsets(bits[7]); // justification offsets
    offsets.addAll(justificationOffsets);
    offsets.addAll(parseOffsets(bits[9])); // provenance of temporal information

    return new SubmissionLine(slotName, docid, filler, fillerOffsets, justificationOffsets, offsets);
  }

  static List<Offset> parseOffsets(String offsetsString) {
    List<Offset> offsets = new ArrayList<Offset>();
    if(offsetsString == null) return offsets;
    // System.err.println("Parsing string [" + offsetsString + "]");
    offsetsString = offsetsString.trim();
    if(offsetsString.length() == 0) return offsets;
    String [] bits = offsetsString.split(",");
    if(bits.length > 0) {
      for(String bit: bits) {
        String [] offsetTokens = bit.split("\\-");
        if(offsetTokens.length != 2) {
          throw new RuntimeException("ERROR: invalid offsets: " + offsetsString + "! Please run the validation script first.");
        }
        offsets.add(new Offset(
          Integer.valueOf(offsetTokens[0]),
          Integer.valueOf(offsetTokens[1])));
      }
    }
    return offsets;
  }

  /**
   * Reads the document offsets from the given file
   * This file must be generated by BuildIdOffsets!
   * @param fn File with doc offsets
   * @return Map from document id to a pair of (file name, offset in file)
   * @throws Exception
   */
  static Map<String, DocOffset> readDocOffsets(String fn) throws Exception {
    System.err.println("Reading document offsets from file " + fn + "...");
    Map<String, DocOffset> offsets = new HashMap<String, DocOffset>();
    BufferedReader is = new BufferedReader(new FileReader(fn));
    String line;
    while((line = is.readLine()) != null) {
      String [] bits = line.split("\t");
      if(bits.length != 3) {
        throw new RuntimeException("ERROR: invalid line in doc offset file: " + line);
      }
      if(offsets.containsKey(bits[0])) {
        throw new RuntimeException("ERROR: found multiple entries for document " + bits[0]);
      }
      offsets.put(bits[0], new DocOffset(bits[1], Integer.valueOf(bits[2])));
    }
    is.close();
    System.err.println("Read offsets for " + offsets.size() + " documents.");
    return offsets;
  }

  static class DocOffset {
    String fileName;
    int offset;
    public DocOffset(String fn, int o) { fileName = fn; offset = o; }
  }

  static class SubmissionLine {
    String slotName;
    String docid;
    String filler;
    /** Offsets for filler */
    List<Offset> fillerOffsets;
    /** Offsets for justification */
    List<Offset> justificationOffsets;
    /** ALL offsets in this line, sorted in the order in which they appear in the line */
    List<Offset> orderedOffsets;
    public SubmissionLine(
      String sn,
      String d,
      String f,
      List<Offset> fo,
      List<Offset> jo,
      List<Offset> o) {
      slotName = sn;
      docid = d;
      filler = f;
      fillerOffsets = fo;
      justificationOffsets = jo;
      orderedOffsets = o;
    }
  }

  static class Offset {
    int start;
    int end;
    public Offset(int s, int e) { start = s; end = e; }
  }

  static final int BUFFER_SIZE = 4096;

  /**
   * Reads the entire file into a String, preserving newline characters seen in the file
   * We assume that the file encoding is UTF-8!!
   * @param file Input file
   * @return Content of file as String
   * @throws Exception
   */
  public static String readRaw(File file) throws Exception {
    InputStreamReader is = new InputStreamReader(
      new GZIPInputStream(new FileInputStream(file)),
      "UTF-8");
    StringBuilder sb = new StringBuilder();
    char [] chars = new char[BUFFER_SIZE];
    int n;
    while((n = is.read(chars)) != -1) {
      sb.append(new String(Arrays.copyOfRange(chars, 0, n)));
    }
    return sb.toString();

  }
}
