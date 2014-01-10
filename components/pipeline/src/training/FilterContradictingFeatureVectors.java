package training;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class FilterContradictingFeatureVectors {
  /**
   * This filters contradicting feature vectors, i.e. for two identical feature 
   * vectors, where one is positive and the other is negative, only the 
   * positive is kept. String hashes are used to determine identity.
   * 
   * @param args
   * @throws IOException 
   */
  public static void main(String[] args) throws IOException {
    String inputFn = args[0];
    String filteredFn = args[1];
    
    Set<Integer> positiveHashes = new HashSet<Integer>();

    BufferedReader br = new BufferedReader(new FileReader(inputFn));
    for (String line; (line = br.readLine()) != null;) {
      // label features
      String[] parts = line.split(" ", 2);
      String label = parts[0];
      int featureHash = parts[1].hashCode();
      if (isPositive(label)) {
        positiveHashes.add(featureHash);
      }
    }
    br.close();
    
    BufferedWriter bw = new BufferedWriter(new FileWriter(filteredFn));
    int numFiltered = 0;
    br = new BufferedReader(new FileReader(inputFn));
    for (String line; (line = br.readLine()) != null;) {
      // label features
      String[] parts = line.split(" ", 2);
      String label = parts[0];
      int featureHash = parts[1].hashCode();
      if (isPositive(label) || !positiveHashes.contains(featureHash)) {
        bw.write(line);
        bw.newLine();
      } else {
        numFiltered += 1;
      }
    }
    br.close();
    bw.close();
    System.err.println("Instances filtered: " + numFiltered);
  }

  private static boolean isPositive(String label) {
    return label.equals("+1") || label.equals("1");
  }
}
