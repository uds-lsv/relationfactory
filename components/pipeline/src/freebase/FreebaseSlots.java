package freebase;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;


import query.QueryList;
import query.QueryList.Query;

public class FreebaseSlots {

  /**
   * This writes out slot fillers found directly in Freebase, tab-separated in 
   * the format:
   * relation + "\t" + query_id + "\t" + slot
   * 
   * @param args
   * @throws IOException 
   */
  public static void main(String[] args) throws IOException {
    // FreebaseSlots <freebase_index> <tac_to_freebase_relmap> <query_expanded.xml> <slots_out>
    String fbIndexDir = args[0];
    String relationMapping = args[1];
    String queryFn = args[2];
    String slotsOutFn = args[3];
    
    QueryList ql = new QueryList(queryFn);
    Directory dir = FSDirectory.open(new File(fbIndexDir));
    IndexSearcher is = new IndexSearcher(dir, true);
    
    System.err.println("reading mapping from TAC to Freebase relations ...");
    // see: $TAC_ROOT/resources/manual_annotation/freebase_relations_types.txt
    Multimap<String, String[]> tacToFb = HashMultimap.create();
    BufferedReader br = new BufferedReader(new FileReader(relationMapping));
    for (String line; (line = br.readLine()) != null;) {
      line = line.trim();
      if (line.isEmpty() || line.startsWith("#")) {
        continue;
      }
      String[] tacAndFb = line.split(" ", 2);
      tacToFb.put(tacAndFb[0], tacAndFb[1].split(" "));
    }
    br.close();
    System.err.println("... done!");
    
    Set<String> answerTriples = new TreeSet<String>();
    
    System.err.println("Querying Freebase ...");
    for (Query q : ql.getQueries()) {
      System.err.println("qid: " + q.getId());
      for (String rel : q.getRelations()) {
        System.err.println(" rel: " + rel);
        int numSlot = 0;
        List<String> aliases = new ArrayList<String>(q.getAliases());
        aliases.add(q.getName());
        for (String alias : aliases) {
          for (String[] queryChain : tacToFb.get(rel)) {
            Set<String> slots = FreebaseQuery.getSlots(is, alias, queryChain);
            for (String slot : slots) {
              answerTriples.add(rel + "\t" + q.getId() + "\t" + slot);
              numSlot += 1;
            }
          }
        }
        if (numSlot > 0) {
          System.err.println("slots found: " + numSlot);
        }
      }
    }
    System.err.println("... querying done!");

    System.err.println("Writing out slots found ...");
    BufferedWriter bw = new BufferedWriter(new FileWriter(slotsOutFn));
    for (String triple : answerTriples) {
      bw.append(triple);
      bw.newLine();
    }
    bw.close();
    System.err.println("... done!");
  }

}
