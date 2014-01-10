package annotation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class SentenceSubset {
  public static void main(String[] args) throws IOException {
    // SentenceSubset <num_sents> <sentences>
    // This prints out, for each relation, the number of sentences as indicated
    // in <num_sents>. For each argument pair maximally one sentence is printed 
    // out.
    String numSentsFn = args[0];
    String sentsFn = args[1];
    
    Map<String, Integer> relToNum = new HashMap<String, Integer>();
    
    BufferedReader br = new BufferedReader(new FileReader(numSentsFn));
    for (String line; (line = br.readLine()) != null;) {
      String rel = line.split("\\s+")[0];
      int num = Integer.parseInt(line.split("\\s+")[1]);
      relToNum.put(rel, num);
    }
    br.close();
    
    br = new BufferedReader(new FileReader(sentsFn));
    //String lastTriple = null;
    String lastArg1 = null;
    
    for (String line; (line = br.readLine()) != null;) {
      String[] parts = line.split("\\t");
      String arg1 = parts[0];
      String rel = parts[1];
      //String arg2 = parts[2];
      
//      String triple = arg1 + "\t" + rel + "\t" + arg2;
      if (arg1.equals(lastArg1)) {
        continue;
      }
      lastArg1 = arg1;
      
      if (relToNum.containsKey(rel) && relToNum.get(rel) > 0) {
        System.out.println(line);
        relToNum.put(rel, relToNum.get(rel) - 1);
      }
    }
    br.close();
  }
}
