package run;

import indexir.Indexing;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.search.BooleanClause.Occur;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Version;

import query.QueryList;

public class Retrieve {
  // Number of documents to be minimally retrieved.
  public final static int MIN_RETRIEVE = 10;
  
  static double pmi(Query q1, Query q2, IndexSearcher is) throws IOException {
    BooleanQuery pairQuery = new BooleanQuery();
    pairQuery.add(q1, Occur.MUST);
    pairQuery.add(q2, Occur.MUST);
    double pmi = 0;
    int pairCount = is.search(pairQuery, 1).totalHits;
    if (pairCount > 0) {
      int arg1Count = is.search(q1,1).totalHits;
      int arg2Count = is.search(q2,1).totalHits;        
      int n = is.getIndexReader().numDocs();
      pmi = (Math.log(pairCount) - Math.log(arg1Count) - Math.log(arg2Count) + Math.log(n)) / Math.log(n);
      
      if (Double.isInfinite(pmi)) {
        System.out.println(pairCount);
        System.out.println(arg1Count);
        System.out.println(arg2Count);
        System.out.println(n);
        throw new IllegalStateException("Impossible score");
      }
    }
    return pmi;
  }
  
  /**
   * Retrieves documents for the given query. The query is expanded by an
   * alias term according to PPMI on the corpus.
   * 
   * @param args
   * @throws IOException
   * @throws ParseException
   */
  public static void main(String[] args) throws IOException, ParseException {
    if (args.length < 4) {
      System.err.println("java Retrieval <query_xml> <indexDir> <numResults> <reponse_file>");
      System.err.println("query_xml:  file containing query (optionally expanded).");
      System.err.println("indexDir:   directory where index is located.");
      System.err.println("numResults: max. number of hits returned.");
      System.err.println("reponse:    resulting trec_eval response.");
      return;
    }
    String queryXmlFn = args[0];
    String indexDir   = args[1];
    int numResults = Integer.parseInt(args[2]);
    String responseFn = args[3];
    
    QueryList ql = new QueryList(queryXmlFn);

    Directory dir = FSDirectory.open(new File(indexDir));
    IndexSearcher is = new IndexSearcher(dir, true);
    QueryParser parser = new QueryParser(Version.LUCENE_29, Indexing.CONTENTS, 
        new StandardAnalyzer(Version.LUCENE_29));
    
    BufferedWriter bw = new BufferedWriter(new FileWriter(responseFn));
    
    for (QueryList.Query q : ql.getQueries()) {
      String queryStr = "\"" + QueryParser.escape(q.getName()) + "\"";

      String expansion = "";
      double maxPPMI = 0;
      
      for (String alias : q.getAliases()) {
        String delimAlias = " " + alias + " "; 
        if (q.getEnttype().toLowerCase().equals("org") || alias.contains(" ")) {
          String expansionQstr = "\"" + QueryParser.escape(alias) + "\"";
          double expansionPMI = 
              pmi(parser.parse(queryStr), parser.parse(expansionQstr), is);
          // We want to have expansions that find new docs -> no superstrings.
          if (expansionPMI > maxPPMI && 
              !delimAlias.contains(" " + q.getName() + " ")) {
            expansion = expansionQstr;
            maxPPMI = expansionPMI;
          }
        }
      }
      
      if (maxPPMI > 0) {
        queryStr += " " + expansion + "^0.1";
      }
      
      Query query;
      query = parser.parse(queryStr);
      TopDocs hits = is.search(query, numResults);
      
      if (hits.scoreDocs.length <  Math.min(MIN_RETRIEVE, numResults)) {
        // Backoff: retrieve one document with any expansion.
        queryStr = "\"" + QueryParser.escape(q.getName()) + "\"";
        for (String alias : q.getAliases()) {
          queryStr += " \"" + QueryParser.escape(alias) + "\"^0.01";
        }
        query = parser.parse(queryStr);
        hits = is.search(query, Math.min(MIN_RETRIEVE, numResults));
      }
      
      for(int i=0; i < hits.scoreDocs.length; i++) {
        ScoreDoc scoreDoc = hits.scoreDocs[i];
        Document luceneDoc = is.doc(scoreDoc.doc);
        String fn = luceneDoc.get(Indexing.FILENAME);
        String docid = luceneDoc.get(Indexing.ID);
        bw.append(q.getId() + " 0 " + fn + ":" + docid + " " + 
            Integer.toString(i + 1) + " " + Double.toString(scoreDoc.score) + 
            " lucene\n");
      }
    }
    bw.close();
    is.close();
  }
}
