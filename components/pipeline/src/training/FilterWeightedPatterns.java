package training;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class FilterWeightedPatterns {
public static void main(String[] args) throws IOException {
  String patternFn = args[0];
  String paramsFn = args[1];
  
  Map<String, Double> relToMinScore = new HashMap<String, Double>();
  BufferedReader br = new BufferedReader(new FileReader(paramsFn));
  for (String line; (line = br.readLine()) != null;) {
    String rel = line.split(" ")[0];
    Double param = Double.parseDouble(line.split(" ")[1]);
    relToMinScore.put(rel, param);
  }
  br.close();
  br = new BufferedReader(new FileReader(patternFn));
  for (String line; (line = br.readLine()) != null;) {
    String[] parts = line.split(" ",3);
    double score = Double.parseDouble(parts[0]);
    String rel = parts[1];
    if (!relToMinScore.containsKey(rel)) {
      System.err.println("No score for: " + rel);
    }
    
    if (score < relToMinScore.get(rel)) {
      continue;
    }
    System.out.println(line);
  }
  br.close();
}
}
