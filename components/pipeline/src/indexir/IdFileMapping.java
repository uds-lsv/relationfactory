package indexir;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.apache.lucene.document.Document;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;


public class IdFileMapping {
  public static void main(String[] args) throws IOException {
    if (args.length != 2) {
      System.out.println("java IdFileMapping <indexDir> <mappingFile>");
      System.out.println("indexDir:              directory where index is located");
      System.out.println("mappingFile:           file to which doc ids and filesnames are written");
      return;
    }
    
    String indexDir = args[0];
    String mappingFn = args[1];
    
    BufferedWriter bw = new BufferedWriter(new FileWriter(mappingFn));
    
    Directory dir = FSDirectory.open(new File(indexDir));
    IndexReader ir = IndexReader.open(dir, true);
    System.out.println("Traversing index ...");

    int md = ir.maxDoc();
    double pctFinished = 0.0;
    double pctStep = 0.1;
    for (int i = 0; i < md; i++) {
      if ( i / (double) md - pctFinished > pctStep ) {
        pctFinished += pctStep;
        System.out.print("\r" + Math.round(pctFinished * 100) + "% ");
      }
      if (ir.isDeleted(i))
          continue;
      Document doc = ir.document(i);
      bw.append(doc.get(Indexing.ID));
      bw.append("\t");
      bw.append(doc.get(Indexing.FILENAME));
      bw.newLine();
    }
    bw.close();
    ir.close();
    System.out.println("... done!");
  }
}
