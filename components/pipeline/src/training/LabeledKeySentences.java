package training;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import rerac.protos.Corpus.Document;
import rerac.protos.Corpus.Document.AnnotationType;
import rerac.protos.Corpus.Document.Compound;
import rerac.protos.Corpus.Document.CompoundGroup;
import util.DocumentExtractor;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;


public class LabeledKeySentences {
  /**
   * This returns the triples according to a key file, with a certain label 
   * (positive, negative, inexact, ...).
   * By default the positive label ("1") is taken.
   * Document id is not considered, casing is considered.
   * 
   * LabeledKeySentences key candidates out_sentences [label]
   * 
   * @param args
   * @throws IOException 
   */
  public static void main(String[] args) throws IOException {
    String keyFn = args[0];
    String candidatesFn = args[1];
    String positiveOutFn = args[2];
    String goodLabel = "1";
    if (args.length == 4) {
      goodLabel = args[3];
    }
    System.out.println("Only sentences labeled with: " + goodLabel);
    
    // Positively judged triples of Target_id:relation:slot
    Set<String> positives = new HashSet<String>();
    
    System.out.println("reading judgments ...");
    BufferedReader br = new BufferedReader(new FileReader(keyFn));
    for (String line; (line = br.readLine()) != null;) {
      //51      SF590:per:member_of     eng-WL-11-174596-12951655       3       0       foreign service 0       0:0     0       0:0
      String[] parts = line.split("\\t");
      String judgement = parts[3];
      if (goodLabel.equals(judgement)) {
        String triple = parts[1] + ":" + parts[5];
        positives.add(triple);
      }
    }
    br.close();
    System.out.println("...done!");
    
    
    System.out.println("reading candidates ...");
    int numSents = 0;
    int numPos = 0;
    Multimap<String, Document> tripleToSentences = HashMultimap.create();
    BufferedInputStream sentenceIs = new BufferedInputStream(
        new FileInputStream(candidatesFn));
    for (Document sentence; 
        (sentence = Document.parseDelimitedFrom(sentenceIs)) != null;) {
      numSents++;
      for (CompoundGroup cg : sentence.getCompoundList()) {
        if (cg.getType() == AnnotationType.PROPERTY) {
          if (cg.getCompoundCount() > 0) {
            Compound c = cg.getCompound(0);
            String rel = c.getText();
            String targetId = DocumentExtractor.canonicalArg(sentence, 0, rel);
            String slot = DocumentExtractor.argumentText(sentence, rel, 1);
            String triple = targetId + ":" + rel + ":" + slot;
            if (positives.contains(triple)) {
              tripleToSentences.put(triple, sentence);
              numPos++;
            }
          }
        }
      }
      if (numSents % 10000 == 0) {
        System.out.println("Good: " + numPos + "/" + numSents);
      }
    }
    System.out.println("Good: " + numPos + "/" + numSents);
    sentenceIs.close();
    System.out.println("...done!");
    
    System.out.println("writing positive sentences...");
    DataOutputStream dataOut = new DataOutputStream(new BufferedOutputStream(
        new FileOutputStream(positiveOutFn)));
    for (String triple : tripleToSentences.keySet()) {
      for (Document sentence : tripleToSentences.get(triple)) {
        sentence.writeDelimitedTo(dataOut);
      }
    }
    dataOut.close();
    System.out.println("...done!");
  }
}
