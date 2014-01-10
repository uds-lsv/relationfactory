package run;

import indexir.IndexingStoreTagged;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;

import org.apache.lucene.document.Document;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;

import util.DTagged;
import util.DTagged.TaggedSentence;
import util.DscoreFile;
import util.DscoreFile.DscoreEntry;

public class TaggedStoredDocs {
  public static void main(String[] args) throws IOException {
    if (args.length != 2) {
      System.err.println("java TaggedStoredDocs <dscore> <indexdir>");
      return;
    }

    DscoreFile dscoreFile = new DscoreFile(args[0]);
    String indexDir   = args[1];
    Directory dir = FSDirectory.open(new File(indexDir));
    IndexSearcher is = new IndexSearcher(dir, true);
    //QueryParser parser = new QueryParser(Version.LUCENE_29, Indexing.CONTENTS, 
    //    new StandardAnalyzer(Version.LUCENE_29));
    
    BufferedWriter out = new BufferedWriter(new OutputStreamWriter(System.out, 
        "UTF-8"));
    
    for (String docid : dscoreFile.getDocIds()) {
      Query q = new TermQuery(new Term(IndexingStoreTagged.ID, docid));
      TopDocs hits = is.search(q, 1);
      if (hits.scoreDocs.length != 1) {
        throw new IllegalStateException("Number of documents retrieved !=1 for id: " + docid);
      }
      ScoreDoc scoreDoc = hits.scoreDocs[0];
      Document luceneDoc = is.doc(scoreDoc.doc);
      DTagged dtag = DTagged.fromString(luceneDoc.get(IndexingStoreTagged.DTAG));
      for (DscoreEntry entry : dscoreFile.getByDocId(docid)) {
        // Get all queries this document is relevant for, and write out.
        String qid = entry.qid;
        for (TaggedSentence s : dtag.getSentences()) {
          s.getId().setQueryId(qid);
          out.write(s.toString());
        }
      }
    }
    
    is.close();
    out.flush();
  }
}
