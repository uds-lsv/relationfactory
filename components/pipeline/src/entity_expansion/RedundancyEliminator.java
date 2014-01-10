package entity_expansion;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;


public class RedundancyEliminator {
  /**
   * This removes redundancy by mapping to representatives using the 
   * MaxLinkEntityExpander and Wikipedia link text.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 3) {
      System.out.println("RedundancyEliminator <linkstat> <infile> <outfile>");
    }
    String linkStatFn = args[0];
    String inFile = args[1];
    String outFile = args[2];
    
    MaxLinkEntityExpander mle = new MaxLinkEntityExpander(linkStatFn);
    
    Map<String, String> tripleToResponse = new HashMap<String, String>();
    Map<String, Double> tripleToMaxScore = new HashMap<String, Double>();
    
    BufferedReader br = new BufferedReader(new FileReader(inFile));
    for (String line; (line = br.readLine()) != null;) {
      String[] lineParts = line.split("\t");
      boolean isNIL = lineParts[3].equals("NIL");
      if (isNIL) {
        tripleToResponse.put(lineParts[0] + "\t" +lineParts[1] + "\tNIL", line);
      } else {
        String fill = lineParts[4];
        String rel = lineParts[1];
        
        String canonicalFill = fill;
        if (rel.contains("alternate_names")) {
          canonicalFill = canonicalFill.replaceAll("[^\\p{L}\\p{N}]", "").toLowerCase();
        } else {
          canonicalFill = mle.isMapped(fill) ? 
              mle.expand(fill) : mle.expand(fill.toLowerCase());
          canonicalFill = canonicalFill.replaceAll("[^\\p{L}\\p{N}]", "").toLowerCase();          
        }
        
        Double score = Double.parseDouble(lineParts[9]);
        String tripleKey = lineParts[0] + "\t" + lineParts[1] + "\t" + 
            canonicalFill;
        if (!tripleToMaxScore.containsKey(tripleKey) ||
            score > tripleToMaxScore.get(tripleKey)) {
          tripleToMaxScore.put(tripleKey, score);
          tripleToResponse.put(tripleKey, line);
        }
      }
    }
    br.close();
    
    BufferedWriter bw = new BufferedWriter(new FileWriter(outFile));
    for (String response : tripleToResponse.values()) {
      bw.append(response);
      bw.newLine();
    }
    bw.close();
  }
}
