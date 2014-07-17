package run;

import indexir.Indexing;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.apache.lucene.document.Document;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;

import query.QueryList;
import query.QueryList.Query;

public class SupportDocs {
  /**
   * Retrieves support documents accoring to query.
   * TODO: include in Retrieve.
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 4) {
      System.err.println("SupportDocs <query_xml> <index> <kb_dir> <dscore>");
      System.err.println("This retrieves the support docs that are indicated in the query.");
      System.err.println("The support documents are written into a dscore file.");
      return;
    }
    String queryFN = args[0];
    String indexDir = args[1];
    String kbDir = args[2];
    String responseFn = args[3];
    
    QueryList ql = new QueryList(queryFN);
    
    Directory dir = FSDirectory.open(new File(indexDir));
    IndexSearcher is = new IndexSearcher(dir, true);
    
    BufferedWriter bw = new BufferedWriter(new FileWriter(responseFn));
    for (Query q : ql.getQueries()) {
      String docId = q.getDocId();
      org.apache.lucene.search.Query luceneQuery = 
          new TermQuery(new Term(Indexing.ID, docId));
      TopDocs hits = is.search(luceneQuery, 1);
      if (hits.scoreDocs.length != 1) {
        throw new IllegalStateException("No supporting document for query: " + 
            q.getId());
      } 
      Document luceneDoc = is.doc(hits.scoreDocs[0].doc);
      String docFN = luceneDoc.get(Indexing.FILENAME);
      bw.append(q.getId() + " 0 " + docFN + ":" + docId + " 1 1.0 lucene\n");
      
      String kbId = q.getNodeId();
      if (!(kbId.isEmpty() || kbId.startsWith("NIL"))) {
        String kbFN = kbDir + "/" + kbId;
        if(new File(kbFN).isFile()) {
          bw.append(q.getId() + " 0 " + kbFN + ":" + kbId + " 2 1.0 lucene\n");        
        } else {
          System.err.println(q.getId() + " not in kb: " + kbFN);
        }
      }
    }
    bw.close();
  }
}
