package run;
import java.io.BufferedInputStream;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;

import rerac.protos.Corpus.Document;
import rerac.protos.Corpus.Document.AnnotationType;
import rerac.protos.Corpus.Document.Compound;
import rerac.protos.Corpus.Document.CompoundGroup;
import util.DocumentExtractor;


public class ProtoToCandidates {
  /**
   * Writes out tab-separated candidates from protobuf file.
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 2) {
      System.err.println(
        "ProtoToCandidates <proto_doc> <candidates>");
      return;
    }
    String docFN = args[0];
    String candsFN = args[1];
    
    BufferedInputStream sentenceIs = new BufferedInputStream(
        new FileInputStream(docFN));
    BufferedWriter bw = new BufferedWriter(new FileWriter(candsFN));
    for (Document sentence; 
        (sentence = Document.parseDelimitedFrom(sentenceIs)) != null;) {
      for (CompoundGroup cg : sentence.getCompoundList()) {
        if (cg.getType() == AnnotationType.PROPERTY) {
          if (cg.getCompoundCount() > 0) {
            Compound c = cg.getCompound(0);
            String relation = c.getText();
            String targetId = 
                DocumentExtractor.canonicalArg(sentence, 0, relation);
            String slot = DocumentExtractor.argumentText(sentence, relation, 1);
            int targetStart = 
                DocumentExtractor.getArgStart(sentence, relation, 0);
            int targetEnd =  DocumentExtractor.getArgEnd(sentence, relation, 0);
            int slotStart = 
                DocumentExtractor.getArgStart(sentence, relation, 1);
            int slotEnd =  DocumentExtractor.getArgEnd(sentence, relation, 1);
            
            bw.append(targetId).
            append("\t").append(relation).
            append("\t").append(slot).
            append("\t").append(sentence.getId()).
            append("\t").append(Integer.toString(targetStart)).
            append("\t").append(Integer.toString(targetEnd)).
            append("\t").append(Integer.toString(slotStart)).
            append("\t").append(Integer.toString(slotEnd)).
            append("\t").append(DocumentExtractor.textFromTokens(sentence)).
            append("\n");
          }
        }
      }
    }
    bw.close();
    sentenceIs.close();
  }
}