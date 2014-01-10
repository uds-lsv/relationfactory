package util;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import opennlp.tools.dictionary.serializer.EntryInserter;

public class DscoreFile {
  
  static public class DscoreEntry {
    public final String qid;
    public final String docid;
    public final String path;
    public final int strangeNumber;
    public final int entryNumber;
    public final double score;
    public final String engine;
    
    static final String SEP = " ";
    
    public DscoreEntry(String qid, String docid, String path,
        int strangeNumber, int entryNumber, double score, String engine) {
      this.qid = qid;
      this.docid = docid;
      this.path = path;
      this.strangeNumber = strangeNumber;
      this.entryNumber = entryNumber;
      this.score = score;
      this.engine = engine;
    }
    
    public static DscoreEntry fromDelimLine(String dscoreLine) {
      String[] cols = dscoreLine.split(SEP);
      if (cols.length < 6) {
        throw new IllegalArgumentException("Invalid line: " + dscoreLine);
      }
      String[] fnDocId = cols[2].split(":", 2);
      if (fnDocId.length < 2 || fnDocId[0].isEmpty() || fnDocId[1].isEmpty()) {
        throw new IllegalArgumentException("Invalid docid + path: " + 
            dscoreLine);
      }
      try {
        return new DscoreEntry(cols[0], fnDocId[1], fnDocId[0], 
            Integer.parseInt(cols[1]), Integer.parseInt(cols[3]), 
            Double.parseDouble(cols[4]), cols[5]); 
      } catch (NumberFormatException e) {
        throw new IllegalArgumentException("Invalid line: " + dscoreLine + 
            " " + e);
      }
    }
  }
  
  // docid -> list of entries
  private final Map<String, Collection<DscoreEntry>> docIdToDscoreLine = 
      new HashMap<String, Collection<DscoreEntry>>();
  // set of all paths
  private final Set<String> paths = new HashSet<String>();
  
  public DscoreFile(String fn) throws IOException {
    BufferedReader dscoreBr = new BufferedReader(new InputStreamReader(
        new FileInputStream(fn), "UTF-8"));
    try {
      for (String dscoreLine; (dscoreLine = dscoreBr.readLine()) != null;){
        DscoreEntry entry = DscoreEntry.fromDelimLine(dscoreLine);
        Collection<DscoreEntry> entries = docIdToDscoreLine.get(entry.docid);
        if (entries == null) {
          entries = new ArrayList<DscoreEntry>();
          docIdToDscoreLine.put(entry.docid, entries);
        }
        entries.add(entry);
        paths.add(entry.path);
      }
    } finally {
      dscoreBr.close();
    }
  }
  
  public Collection<String> getDocIds() {
    return  docIdToDscoreLine.keySet();
  }
  
  public Collection<DscoreEntry> getByDocId(String docid) {
    if (!docIdToDscoreLine.containsKey(docid)) {
      return new LinkedList<DscoreEntry>();
    }
    return  docIdToDscoreLine.get(docid);
  }
  
  public Map<String, String> idToFileMap() {
    Map<String, String> docpaths = new HashMap<String, String>();
    for (String docId : this.docIdToDscoreLine.keySet()) {
      assert(!docIdToDscoreLine.get(docId).isEmpty());
      // Take first entry.
      DscoreEntry ds = this.docIdToDscoreLine.get(docId).iterator().next();
      docpaths.put(docId, ds.path);
    }
    return docpaths;
  }
  
  public Collection<String> getPaths() {
    return paths;
  }
  
  // this might be useful at some point
  public static class DscoreEntryPathComp implements Comparator<DscoreEntry> {
    @Override
    public int compare(DscoreEntry d1, DscoreEntry d2) {
      int ret = d1.path.compareToIgnoreCase(d2.path);
      if (ret == 0) {
        return new Integer(d1.entryNumber).compareTo(
            new Integer(d2.entryNumber));
      }
      return ret;
    }
  }
}

