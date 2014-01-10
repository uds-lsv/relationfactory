package freebase;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.apache.lucene.document.Field;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.memory.PatternAnalyzer;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.Version;

public class IndexFreebase {
  public static final Pattern TAB_PATTERN = Pattern.compile("\\t");
  public static final String REFERRED_NAME = "index_freebase";
  public static final String MID = "mid";
  public static final String DESTINATION = "dest";
  public static final String VALUE = "val";
  public static final String DESTINATION_AND_VALUE = "dest_val";
  
  public static void main(String[] args) throws IOException {
    String freebaseFN = args[0];
    String outputDir = args[1];
    Directory directory = FSDirectory.open(new File(outputDir));
    IndexWriter writer = new IndexWriter(directory, 
        new PatternAnalyzer(Version.LUCENE_29, Pattern.compile("\\t+"), false, null),
        IndexWriter.MaxFieldLength.UNLIMITED);
    // Clear index.
    writer.deleteAll();
    int lineCount = 0;
    System.out.println("Indexing ...");
    String lastMid = null;
    List<String> properties = new ArrayList<String>();
    BufferedReader br = new BufferedReader(new FileReader(freebaseFN));
    for (String line; (line = br.readLine()) != null;) {
      ++lineCount;
      if (lineCount % 1000000 == 0) {
        System.out.print(".");
      } 
      String[] lineParts = TAB_PATTERN.split(line, 2);
      String mid = lineParts[0];
      if (mid.equals("/m/049")) {
        continue;
      }
      if (!mid.equals(lastMid) && lastMid != null) {
        // TODO:
        indexMid(writer, lastMid, properties);
        properties = new ArrayList<String>();
      }
      lastMid = mid;
      if(!"/type/type/instance".equals(TAB_PATTERN.split(line, -1)[1])) {
        properties.add(lineParts[1]);
      }
    }
    // TODO:
    indexMid(writer, lastMid, properties);
    br.close();
    System.out.println();
    System.out.println("...done!");
    System.out.println("optimizing index...");
    writer.optimize();
    System.out.println("...done!");
    writer.close();
  }

  private static void indexMid(IndexWriter writer, String mid, List<String> properties) throws IOException {
    org.apache.lucene.document.Document luceneDoc = 
        new org.apache.lucene.document.Document();
    luceneDoc.add(
        new Field(MID, mid, Field.Store.YES, 
            Field.Index.NOT_ANALYZED));
    for (String prop : properties) {
      String[] propParts = TAB_PATTERN.split(prop, -1);
      if (propParts.length != 3) {
        throw new IllegalArgumentException(prop);
      }
      prop = "^\t" + prop.trim();
      if (propParts[1].isEmpty()) {
        luceneDoc.add(
            new Field(VALUE, prop, Field.Store.YES, 
                Field.Index.ANALYZED));        
      } else if (propParts[2].isEmpty()) {
        luceneDoc.add(
            new Field(DESTINATION, prop, Field.Store.YES, 
                Field.Index.ANALYZED));        
      } else {
        luceneDoc.add(
            new Field(DESTINATION_AND_VALUE, prop, Field.Store.YES, 
                Field.Index.ANALYZED));              
      }
    }
    writer.addDocument(luceneDoc);
  }
}
