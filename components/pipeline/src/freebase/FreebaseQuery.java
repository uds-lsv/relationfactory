package freebase;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.BooleanClause.Occur;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.BooleanQuery.TooManyClauses;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.PhraseQuery;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TopDocs;

/**
 * This is an efficient query facility for Freebase, based on Lucene.
 * See FreebaseQuery.getSlots() for an example of how to Query Freebase.
 * See IndexFreebase for code that creates an index.
 * 
 * @author Benjamin Roth
 *
 */
public class FreebaseQuery {
  class Constraint {
    String[] tuple;
    String type;
    int inputNr;
    int outputNr;
    Constraint(String[] tuple, String type, int inputNr, int outputNr) {
      this.tuple = tuple;
      this.type = type;
      this.inputNr = inputNr;
      this.outputNr = outputNr;
    }

    /**
     * Returns a query for the pattern, if no input arguments.
     * If input arguments are given, it returns all matches for all arguments
     * (i.e. the argument queries are OR'ed together).
     * @param inputArgs
     * @return
     */
    Query getQuery(Set<String> inputArgs) {
      if (inputNr == -1 || inputArgs == null) {
        return FreebaseQuery.getQuery(tuple, type);
      } else {
        BooleanQuery q = new BooleanQuery();
        for (String arg : inputArgs) {
          String[] tupleCopy = Arrays.copyOf(tuple, tuple.length);
          tupleCopy[inputNr] = arg;
          try {
            q.add(FreebaseQuery.getQuery(tupleCopy, type), Occur.SHOULD);
          } catch (TooManyClauses e) {
            System.err.println("Too many clauses. Ignoring: " + type + " " + Arrays.toString(tupleCopy));
          }
        }
        return q;
      }
    }

    public void collect(IndexSearcher is, TopDocs hits, Set<String> results) throws IOException {
      if (outputNr == -1) {
        return;
      }
      for(int i=0; i < hits.scoreDocs.length; i++) {
        ScoreDoc scoreDoc = hits.scoreDocs[i];
        Document luceneDoc = is.doc(scoreDoc.doc);
        if (outputNr == 0) {
          results.add(luceneDoc.getField(IndexFreebase.MID).stringValue());
        } else {
          for (Field f : luceneDoc.getFields(type)) {
            String[] property = 
                IndexFreebase.TAB_PATTERN.split(f.stringValue(), -1);
            boolean tupleMatch = true;
            for (int tupPartNr = 1; tupleMatch && tupPartNr < tuple.length; 
                ++tupPartNr) {
              // get a specific part of constraints.
              String tupPart = tuple[tupPartNr];
              if (null != tupPart && !tupPart.equals(property[tupPartNr])) {
                tupleMatch = false;
              }
            }
            if (tupleMatch) {
              results.add(property[outputNr]);
            }
          }
        }
      }
    }
  }
  List<Constraint> constraints = new ArrayList<Constraint>();
  
  private static Query getQuery(String[] tuple, String type) {
    TermQuery midT = null;
    PhraseQuery pq = new PhraseQuery();
    pq.add(new Term(type, "^"),0);
    for (int i = 0; i < tuple.length; ++i) {
      if (tuple[i] == null) {
        continue;
      }
      if (i == 0) {
        midT = new TermQuery(new Term(IndexFreebase.MID, tuple[i]));
      } else if (i == 1 || i == 2 || 
          (i == 3 && type.equals(IndexFreebase.DESTINATION_AND_VALUE))) {
        pq.add(new Term(type, tuple[i]),i);            
      } else if (i == 3 && type.equals(IndexFreebase.VALUE)) {
        pq.add(new Term(type, tuple[i]),i - 1);            
      }
    }
    Query q;
    if (midT == null) { 
      q = pq; 
    } else {
      BooleanQuery bq = new BooleanQuery();
      bq.add(pq, Occur.MUST);
      bq.add(midT, Occur.MUST);
      q = bq;
    }
    return q;    
  }
  
  public void addConstraint(String[] tuple, String type, int inputNr, int outputNr) {
    constraints.add(new Constraint(tuple, type, inputNr, outputNr));
  }
  
  public Set<String> execute(IndexSearcher is, Set<String> inputArgs, int limit) throws IOException {
    BooleanQuery q = new BooleanQuery();
    for (Constraint c : constraints) {
      q.add(c.getQuery(inputArgs), Occur.MUST);
    }
    TopDocs hits = is.search(q, limit);
    Set<String> results = new HashSet<String>();
    for (Constraint c : constraints) {
      c.collect(is, hits, results);
    }
    return results;
  }
  
  public int numHits(IndexSearcher is, Set<String> inputArgs) throws IOException {
    BooleanQuery q = new BooleanQuery();
    for (Constraint c : constraints) {
      q.add(c.getQuery(inputArgs), Occur.MUST);
    }
    TopDocs hits = is.search(q, 1);
    return hits.totalHits;
  }
  
  /**
   * This returns the result from a query to a freebase index.
   * 
   * The following is an example how to query for the children of Nicholas Sarcozy:
   * 
   * Directory dir = FSDirectory.open(new File(indexDir));
   * IndexSearcher is = new IndexSearcher(dir, true);
   * Set<String> children = PairsFromFreebase.getSlots(is, "Nicolas Sarkozy", 
   *       new String[]{"/people/person", "/people/person/children", "/people/person"});
   * 
   * @param is index searcher on Freebase index.
   * @param entity string represenation / name of entity.
   * @param fbRels String array, chain of freebase relations, where first 
   * element is type restriction on query and last element is type restriction 
   * on result.
   * @return
   * @throws IOException
   */
  public static Set<String> getSlots(IndexSearcher is, String entity, 
      String[] fbRels) throws IOException {
    String targetType = fbRels[0];
    String slotType = fbRels[fbRels.length - 1];
    fbRels = Arrays.copyOfRange(fbRels, 1, fbRels.length - 1);
    
    
    List<FreebaseQuery> queries = new ArrayList<FreebaseQuery>();
    
    FreebaseQuery q = new FreebaseQuery();
    q.addConstraint(new String[]{null, "/type/object/name", "/lang/en", entity}, 
        IndexFreebase.DESTINATION_AND_VALUE, 3, -1);
    q.addConstraint(new String[]{null, "/type/object/type", targetType, null}, 
        IndexFreebase.DESTINATION, -1, -1);

    if (fbRels.length == 1 && "/type/text".equals(slotType)) {
      q.addConstraint(new String[]{null, fbRels[0], "/lang/en", null}, 
          IndexFreebase.DESTINATION_AND_VALUE, -1, 3);
    } else if (fbRels.length == 1 && 
        ("/type/datetime".equals(slotType) || 
        "/type/int".equals(slotType) ||
        "/type/uri".equals(slotType))) {
      q.addConstraint(new String[]{null, fbRels[0], null, null}, 
          IndexFreebase.VALUE, -1, 3);
    } else {
      q.addConstraint(new String[]{null, fbRels[0], null, null}, 
          IndexFreebase.DESTINATION, -1, 2);      
    }
    queries.add(q);
    for (int i = 1; i < fbRels.length; ++i) {
      q = new FreebaseQuery();
      if ( i + 1 == fbRels.length && "/type/text".equals(slotType)) {
        q.addConstraint(new String[]{null, fbRels[i], "/lang/en", null}, 
            IndexFreebase.DESTINATION_AND_VALUE, 0, 3);
      } else if ( i + 1 == fbRels.length && 
          ("/type/datetime".equals(slotType) || 
          "/type/int".equals(slotType) ||
          "/type/uri".equals(slotType))) {
        q.addConstraint(new String[]{null, fbRels[i], null, null}, 
            IndexFreebase.VALUE, 0, 3);
      } else {
        q.addConstraint(new String[]{null, fbRels[i], null, null}, 
            IndexFreebase.DESTINATION, 0, 2);      
      }
      queries.add(q);
    }
    Set<String> intermediate = null;
    for (FreebaseQuery query : queries) {
      intermediate = query.execute(is, intermediate, 10);
    }
    boolean isValueType =
        "/type/text".equals(slotType) ||
        "/type/datetime".equals(slotType) || 
        "/type/int".equals(slotType) ||
        "/type/uri".equals(slotType);
    Set<String> result;
    if (isValueType) {
      result = intermediate;
    } else {
      result = new HashSet<String>();
      q = new FreebaseQuery();
      q.addConstraint(new String[]{null, "/type/object/name", "/lang/en", null}, 
          IndexFreebase.DESTINATION_AND_VALUE, 0, 3);
      q.addConstraint(new String[]{null, "/type/object/type", slotType, null}, 
          IndexFreebase.DESTINATION, 0, -1);
      result.addAll(q.execute(is, intermediate, 10));
      q = new FreebaseQuery();
      q.addConstraint(new String[]{null, "/common/topic/alias", "/lang/en", null}, 
          IndexFreebase.DESTINATION_AND_VALUE, 0, 3);
      q.addConstraint(new String[]{null, "/type/object/type", slotType, null}, 
          IndexFreebase.DESTINATION, 0, -1);
      result.addAll(q.execute(is, intermediate, 10));
    }

    return result;
  }
}
