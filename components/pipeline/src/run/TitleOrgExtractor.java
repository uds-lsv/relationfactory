package run;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;

import util.TextIdentifier;

public class TitleOrgExtractor {
  static Logger logger = Logger.getLogger(TitleOrgExtractor.class.getName());  
  
  // SentenceId, Title, Org
  private final static String OUTPUT_LINE_FORMAT = "%s\t%s\t%s"; 
  
  private static Collection<Pattern> readPatterns(String patternFn) 
      throws IOException {
    // Read in patterns, one per line
    BufferedReader reader = new BufferedReader(new InputStreamReader(
        new FileInputStream(patternFn), "UTF-8"));
    List<Pattern> patterns = new ArrayList<Pattern>();
    for (String patternLine; (patternLine = reader.readLine()) != null; ) {
      StringBuilder patternBuilder = new StringBuilder();
      String[] parts = patternLine.split(" ");
      for (String part : parts) {
        if (patternBuilder.length() > 0) patternBuilder.append(" ");
        if (part.equals("JOB_TITLE") || part.equals("ORGANIZATION")) {
          patternBuilder.append("(B-" + part + "(?: I-" + part + ")*)");
        } else {
          patternBuilder.append("\\Q" + part + "\\E");
        }
      }
      patterns.add(Pattern.compile(patternBuilder.toString()));
      logger.debug("Added pattern " + patternBuilder.toString());
    }
    return patterns;
  }
  
	/**
	 * @param args
	 * @throws IOException 
	 * @throws FileNotFoundException 
	 */
	public static void main(String[] args) 
	    throws IOException {
	  
	  if (args.length != 1) {
	    System.err.println("Usage:");
	    System.err.println("run.TitleOrgExtractor " +
	    		"title_org_patterns < dtag-file");
	    return;
	  }
	  
	  String patternFn = args[0];
	  //String patternFn = "../../resources/manual_annotation/title_org_patterns";
	  Collection<Pattern> patterns = readPatterns(patternFn);
	  
	  // Read in dtag file from stdin
	  BufferedReader in = new BufferedReader(new InputStreamReader(System.in, 
	      "UTF-8"));
	  BufferedWriter out = new BufferedWriter(new OutputStreamWriter(System.out, 
        "UTF-8"));
	  StringBuilder docBuilder = null;
	  StringBuilder taggedDocBuilder = null;
	  String lastTag = null;
	  String docQuerySnrId = null;
	  
	  Map<Integer, Integer> tagStartOffsets = new HashMap<Integer, Integer>();
	  Map<Integer, Integer> tagEndOffsets = new HashMap<Integer, Integer>();
	  
	  // Transform each sentence into regex and match agains patterns. done.
	  for (String dtagLine; (dtagLine = in.readLine()) != null; ) {
	    if (dtagLine.startsWith("<D")) {
	      docBuilder = new StringBuilder();
	      taggedDocBuilder = new StringBuilder();
	      lastTag = null;
	      // "<D=".length(); without ">"
	      docQuerySnrId = dtagLine.substring(3, dtagLine.length() - 1);
	      
	      tagStartOffsets.clear();
	      tagEndOffsets.clear();
	      
	    }	else if (dtagLine.startsWith("</D")) {
	      assert(docBuilder != null);
	      assert(taggedDocBuilder != null);
	      // Matching here
	      String doc = docBuilder.toString();
	      String taggedDoc = taggedDocBuilder.toString();
	      for (Pattern pattern : patterns) {
	        Matcher m = pattern.matcher(taggedDoc);
	        while (m.find()) {
	          String title = "";
	          String org = "";
	          for (int groupIdx = 1; groupIdx <= m.groupCount(); groupIdx++) {
              Integer startIdx = tagStartOffsets.get(m.start(groupIdx));
              Integer endIdx = tagEndOffsets.get(m.end(groupIdx));
              if (startIdx == null) {
                logger.warn("startIdx was null");
                continue;
              }
              if (endIdx == null) {
                if (lastTag == null) {
                  logger.warn("endIdx was null for pattern: " + 
                      pattern.toString() + " and string: " + taggedDoc);
                  continue;
                }
                endIdx = doc.length();
              }
              String content = doc.substring(startIdx, endIdx);
              if (m.group(groupIdx).startsWith("B-JOB_TITLE")) {
                title = content;
              } else if (m.group(groupIdx).startsWith("B-ORGANIZATION")) {
                org = content;
              }
            }
	          out.write(
	              String.format(OUTPUT_LINE_FORMAT, docQuerySnrId, title, org) + 
	              "\n");
	        }
	      }
	    } else {
	      assert(docBuilder != null);
	      assert(taggedDocBuilder != null);
	      if (docBuilder.length() > 0) {
	        docBuilder.append(" ");
	        taggedDocBuilder.append(" ");
	      }
	      String[] lineParts = dtagLine.split(" ");
	      if (lineParts.length > 1 && !lineParts[1].equals("O")) {
	        String tag = lineParts[1];
	        if (!tag.startsWith("B-") && !tag.startsWith("I-")) {
	          throw new IllegalArgumentException("Illegal tag for line:\n" + dtagLine);
	        }
	        if (tag.startsWith("B-") || !tag.substring(2).equals(lastTag)) {
	          tagStartOffsets.put(taggedDocBuilder.length(), docBuilder.length());
	          if (lastTag != null) {
	            tagEndOffsets.put(Math.max(0, taggedDocBuilder.length() - 1), 
	                Math.max(0, docBuilder.length() - 1));
	          }
	        }
	        lastTag = tag;
	        taggedDocBuilder.append(tag);
	      } else {
	        if (lastTag != null) {
	          tagEndOffsets.put(Math.max(0, taggedDocBuilder.length() - 1), 
	              Math.max(0, docBuilder.length() - 1));
	        }
	        lastTag = null;
	        taggedDocBuilder.append(lineParts[0]);
	      }
	      docBuilder.append(lineParts[0]);
	    }
    }
	  out.flush();
	}

}
