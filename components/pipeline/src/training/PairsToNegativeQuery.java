package training;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import query.QueryList;

public class PairsToNegativeQuery {
  /**
   * This is used to contruct a query file for distant supervision from a list
   * of DS triples (rel, target, slot).
   * The known slots are stored in 'ignore_slotfiller'.
   * The resulting query.xml can be used for retrieval and candidate matching
   * (see also DistsupCandidates).
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    String pairsFn = args[0];
    String queryFn = args[1];

    QueryList ql = new QueryList();
    String currentTarget = null;
    QueryList.Query currentQuery = null;
    BufferedReader br = new BufferedReader(new FileReader(pairsFn));
    for (String line; (line = br.readLine()) != null;) {
      String[] parts = line.split("\\t");
      String rel = parts[0];
      String target = parts[1];
      String id = target.replace(" ", "_");
      String slot = parts[2];
      String entType = rel.split(":")[0].equals("org") ? "ORG" : "PER";
      
      if (!target.equals(currentTarget)) {
        System.out.println(target);
        currentTarget = target;
        currentQuery = ql.addNewQuery(id, target, entType);
      }
      currentQuery.getIgnoreSlotfillers().put(rel, slot);
    }
    br.close();
    
    BufferedWriter bw = new BufferedWriter(new FileWriter(queryFn));
    ql.writeTo(bw);
    bw.close();
  }
}
