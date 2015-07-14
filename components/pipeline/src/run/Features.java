package run;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import rerac.protos.Corpus.Document;
import rerac.protos.Corpus.Document.AnnotationType;
import rerac.protos.Corpus.Document.Compound;
import rerac.protos.Corpus.Document.CompoundGroup;
import rerac.protos.InstanceCollection.Instance;
import rerac.protos.InstanceCollection.Instance.Feature;
import util.DocumentExtractor;
import features.SentenceFeatureExtractor;

import org.apache.log4j.Logger;

public class Features {
  static Logger logger = Logger.getLogger(Features.class.getName());
  
  public static final String FGRP_NGRAM_DIRECTED = "ngram_directed";
  public static final String FGRP_NGRAM_LONG_DIRECTED = "ngram_long_directed";
  public static final String FGRP_BROWN_NGRAM_DIRECTED = "brown_ngram_directed";
  public static final String FGRP_BROWN_NGRAM_LONG_DIRECTED = "brown_ngram_long_directed";
  public static final String FGRP_SUB_BROWN_NGRAM_DIRECTED = "sub_brown_ngram_directed";
  public static final String FGRP_SUB_BROWN_NGRAM_LONG_DIRECTED = "sub_brown_ngram_long_directed";
  public static final String FGRP_SKIP_FUZZY = "skip_fuzzy";
  public static final String FGRP_SKIP_EXACT = "skip_exact";
  public static final String FGRP_TOPIC = "topic";
  public static final String FGRP_INTERTEXT = "intertext";
  public static final String FGRP_INTERTEXT_SHORT = "intertext_short";
  public static final String FGRP_MINTZ_NOARGS = "mintz_noargs";
  
  public static final String FGRP_ARG = "arg";
  
  // TODO: single experiment
  public static final boolean PRINT_NUM_SENTENCES = false;
  
  public static void writeFeatures(List<Document> matchingSentences, 
      SentenceFeatureExtractor sfe, String relation,  
      Map<String, String> classMap, BufferedWriter bw,  
      Set<String> featuretypes) throws IOException {
    Instance inst = instance(matchingSentences, sfe, relation, classMap, 
        featuretypes).build();
    //inst.writeDelimitedTo(outToClient);
    
    Document sentence = matchingSentences.get(0);
    
    String qid = 
        DocumentExtractor.canonicalArg(sentence, 0, relation);
    int qStart = DocumentExtractor.getArgStart(sentence, relation, 0);
    int qEnd = DocumentExtractor.getArgEnd(sentence, relation, 0);
    

    String slotId = 
        DocumentExtractor.canonicalArg(sentence, 1, relation);
    // TODO:
    //String slotText = 
    //    DocumentExtractor.argumentText(sentence, relation, 1);
    //if (!slotText.equals(slotId)) {
    //  System.out.println(slotText + " <-> " + slotId);
    //}
    
    int slStart = DocumentExtractor.getArgStart(sentence, relation, 1);
    int slEnd = DocumentExtractor.getArgEnd(sentence, relation, 1);
    
    if (PRINT_NUM_SENTENCES) {
      double mw = 0;
      for (Document s : matchingSentences) {
        if (DocumentExtractor.getWeight(s, relation) > mw) {
          mw = DocumentExtractor.getWeight(s, relation);
        }
      }
      System.out.println(matchingSentences.size() + " " + mw);
    }
    
    bw.append(qid)
      .append('\t')
      .append(relation)
      .append('\t')
      .append(slotId) // TODO
      .append('\t')
      .append(matchingSentences.get(0).getId())
      .append('\t')
      .append(Integer.toString(qStart))
      .append('\t')
      .append(Integer.toString(qEnd))
      .append('\t')
      .append(Integer.toString(slStart))
      .append('\t')
      .append(Integer.toString(slEnd))
      .append('\t');
    
    bw.append(inst.getPositive() ? "+1" : "-1");
    
    for (Feature f : inst.getFeatureList()) {
      bw.append(" ")
        .append(Integer.toString(f.getIndex()))
        .append(":")
        .append(Double.toString(f.getValue()));
    }
    bw.newLine();
  }
  
  static boolean firstMethodInvocationInstance = true;
  public static Instance.Builder instance(List<Document> matchingSentences, 
      SentenceFeatureExtractor sfe, String relation,  
      Map<String, String> classMap,  Set<String> featuretypes) {
    if (!DocumentExtractor.hasRelation(matchingSentences.get(0), relation)) {
      throw new IllegalStateException("Training sentences don't contain relation: " + relation);
    }
    

    
    Instance.Builder inst;
    boolean isPositive = 
        DocumentExtractor.hasPositiveRelation(matchingSentences.get(0), relation);

    inst = Instance.newBuilder();
    inst = sfe.setLabel(inst, relation, isPositive);
    
    if (firstMethodInvocationInstance) {
      logger.debug("Creating first instance.");
      logger.debug("Number of aggregate sentence: " + matchingSentences.size());
      logger.debug("Is positive: " + isPositive);
    }

    if (null == featuretypes || featuretypes.isEmpty()) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding standard features.");
      }
      // standard featureset
      inst = sfe.addBetweenNgram(inst, matchingSentences, relation, 1);
      inst = sfe.addBetweenNgram(inst, matchingSentences, relation, 2);
      inst = sfe.addBetweenNgram(inst, matchingSentences, relation, 3);
      inst = sfe.addBetweenNgram(inst, matchingSentences, relation, 4);
      
      inst = sfe.addOutsideNgram(inst, matchingSentences, relation, 1, 3);
      inst = sfe.addOutsideNgram(inst, matchingSentences, relation, 2, 3);
      inst = sfe.addOutsideNgram(inst, matchingSentences, relation, 3, 3);
      inst = sfe.addOutsideNgram(inst, matchingSentences, relation, 4, 3);
      
      // ngram-size, prefix-length
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 1, 2);
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 1, 6);
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 1, 10);
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 1, 20);
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 2, 2);
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 2, 6);
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 2, 10);
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 2, 20); 
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 3, 2);
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 3, 6);
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 3, 10);
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 3, 20);   
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 4, 2);
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 4, 6);
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 4, 10);
      inst = sfe.addBetweenBrownNgram(inst, matchingSentences, relation, classMap, 4, 20);   
      
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 1, 2, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 1, 6, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 1, 10, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 1, 20, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 2, 2, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 2, 6, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 2, 10, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 2, 20, 3); 
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 3, 2, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 3, 6, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 3, 10, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 3, 20, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 4, 2, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 4, 6, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 4, 10, 3);
      inst = sfe.addOutsideBrownNgram(inst, matchingSentences, relation, classMap, 4, 20, 3);
  
      // TODO: here there was a bug that reset the instance (until 11. 9. 2012).
      inst = sfe.extractBetweenClassConditionedBigram(inst, matchingSentences, 
          relation, classMap);
      inst = sfe.addOutsideStems(inst, matchingSentences, relation, false);
        
      inst = sfe.addDistanceBins(inst, matchingSentences, relation);
      
      inst = sfe.sortAndSumFeatures(inst);
      inst = sfe.normalizeFeaturesToMax(inst);
  
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 1, 2, 0);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 1, 6, 0);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 1, 10, 0);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 1, 20, 0);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 2, 2, 0);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 2, 6, 0);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 2, 10, 0);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 2, 20, 0); 
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 3, 2, 0);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 3, 6, 0);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 3, 10, 0);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 3, 20, 0);
      
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 1, 2, 1);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 1, 6, 1);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 1, 10, 1);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 1, 20, 1);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 2, 2, 1);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 2, 6, 1);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 2, 10, 1);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 2, 20, 1); 
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 3, 2, 1);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 3, 6, 1);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 3, 10, 1);
      inst = sfe.addArgBrownNgram(inst, matchingSentences, relation, classMap, 3, 20, 1);
  
      inst = sfe.tokenJaccardCoefficient(inst, matchingSentences, relation);
      inst = sfe.letterBigramJaccardCoefficient(inst, matchingSentences, relation);
      inst = sfe.addAcronymFeature(inst, matchingSentences, relation);
      inst = sfe.prefixOverlap(inst, matchingSentences, relation);
      inst = sfe.suffixOverlap(inst, matchingSentences, relation);
      
      //inst = sfe.addSlotLetterNgrams(inst, matchingSentences, relation);
      inst = sfe.addSlotCharacterNgrams(inst, matchingSentences, relation, 2);
      inst = sfe.addSlotCharacterNgrams(inst, matchingSentences, relation, 3);
      inst = sfe.addSlotCharacterNgrams(inst, matchingSentences, relation, 4);
      inst = sfe.addArgCapitalization(inst, matchingSentences, relation, 0);
      inst = sfe.addArgCapitalization(inst, matchingSentences, relation, 1);    
      inst = sfe.addSlotTokenCount(inst, matchingSentences, relation);
      inst = sfe.addSlotLogCharacterCount(inst, matchingSentences, relation);
      
      inst = sfe.sortAndSumFeatures(inst);
      inst = sfe.normalizeFeaturesToMax(inst);
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }
    if (featuretypes.contains(FGRP_NGRAM_DIRECTED)) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding feature set: " + FGRP_NGRAM_DIRECTED);
      }
      Instance.Builder fgrpInst = Instance.newBuilder();
      
      fgrpInst = sfe.addBetweenNgram(fgrpInst, matchingSentences, relation, 1, true);
      fgrpInst = sfe.addBetweenNgram(fgrpInst, matchingSentences, relation, 2, true);
      fgrpInst = sfe.addBetweenNgram(fgrpInst, matchingSentences, relation, 3, true);
      
      fgrpInst = sfe.addOutsideNgram(fgrpInst, matchingSentences, relation, 1, 3, true);
      fgrpInst = sfe.addOutsideNgram(fgrpInst, matchingSentences, relation, 2, 3, true);
      fgrpInst = sfe.addOutsideNgram(fgrpInst, matchingSentences, relation, 3, 3, true);
      
      fgrpInst = sfe.sortAndSumFeatures(fgrpInst);
      fgrpInst = sfe.normalizeFeaturesToMax(fgrpInst);
      
      inst = sfe.mergeFeatures(inst, fgrpInst.build());
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }
    if (featuretypes.contains(FGRP_NGRAM_LONG_DIRECTED)) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding feature set: " + FGRP_NGRAM_LONG_DIRECTED);
      }
      Instance.Builder fgrpInst = Instance.newBuilder();
      
      fgrpInst = sfe.addBetweenNgram(fgrpInst, matchingSentences, relation, 4, true);      
      fgrpInst = sfe.addOutsideNgram(fgrpInst, matchingSentences, relation, 4, 3, true);

      fgrpInst = sfe.sortAndSumFeatures(fgrpInst);
      fgrpInst = sfe.normalizeFeaturesToMax(fgrpInst);
      
      inst = sfe.mergeFeatures(inst, fgrpInst.build());
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }
    if (featuretypes.contains(FGRP_BROWN_NGRAM_DIRECTED)) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding feature set: " + FGRP_BROWN_NGRAM_DIRECTED);
      }
      Instance.Builder fgrpInst = Instance.newBuilder();
      
      // ngram-size, prefix-length
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 20, true);
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 20, true); 
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 20, true);   
      
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 20, 3, true);
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 20, 3, true); 
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 20, 3, true);

      fgrpInst = sfe.sortAndSumFeatures(fgrpInst);
      fgrpInst = sfe.normalizeFeaturesToMax(fgrpInst);
      
      inst = sfe.mergeFeatures(inst, fgrpInst.build());
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }
    if (featuretypes.contains(FGRP_SUB_BROWN_NGRAM_DIRECTED)) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding feature set: " + FGRP_SUB_BROWN_NGRAM_DIRECTED);
      }
      Instance.Builder fgrpInst = Instance.newBuilder();
      // ngram-size, prefix-length
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 2, true);
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 6, true);
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 10, true);
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 2, true);
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 6, true);
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 10, true);
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 2, true);
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 6, true);
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 10, true);
      
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 2, 3, true);
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 6, 3, true);
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 10, 3, true);
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 2, 3, true);
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 6, 3, true);
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 10, 3, true);
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 2, 3, true);
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 6, 3, true);
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 10, 3, true);

      fgrpInst = sfe.sortAndSumFeatures(fgrpInst);
      fgrpInst = sfe.normalizeFeaturesToMax(fgrpInst);
      
      inst = sfe.mergeFeatures(inst, fgrpInst.build());
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }
    if (featuretypes.contains(FGRP_BROWN_NGRAM_LONG_DIRECTED)) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding feature set: " + FGRP_BROWN_NGRAM_LONG_DIRECTED);
      }
      Instance.Builder fgrpInst = Instance.newBuilder();
      
      // ngram-size, prefix-length
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 4, 20, true);         
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 4, 20, 3, true);

      fgrpInst = sfe.sortAndSumFeatures(fgrpInst);
      fgrpInst = sfe.normalizeFeaturesToMax(fgrpInst);
      
      inst = sfe.mergeFeatures(inst, fgrpInst.build());
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }
    if (featuretypes.contains(FGRP_SUB_BROWN_NGRAM_LONG_DIRECTED)) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding feature set: " + FGRP_SUB_BROWN_NGRAM_LONG_DIRECTED);
      }
      Instance.Builder fgrpInst = Instance.newBuilder();
      // ngram-size, prefix-length
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 4, 2, true);
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 4, 6, true);
      fgrpInst = sfe.addBetweenBrownNgram(fgrpInst, matchingSentences, relation, classMap, 4, 10, true);
      
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 4, 2, 3, true);
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 4, 6, 3, true);
      fgrpInst = sfe.addOutsideBrownNgram(fgrpInst, matchingSentences, relation, classMap, 4, 10, 3, true);

      fgrpInst = sfe.sortAndSumFeatures(fgrpInst);
      fgrpInst = sfe.normalizeFeaturesToMax(fgrpInst);
      
      inst = sfe.mergeFeatures(inst, fgrpInst.build());
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }
    if (featuretypes.contains(FGRP_ARG)) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding feature set: " + FGRP_ARG);
      }
      Instance.Builder fgrpInst = Instance.newBuilder();
      
      fgrpInst = sfe.addDistanceBins(fgrpInst, matchingSentences, relation);
      
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 2, 0);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 6, 0);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 10, 0);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 20, 0);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 2, 0);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 6, 0);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 10, 0);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 20, 0); 
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 2, 0);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 6, 0);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 10, 0);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 20, 0);
      
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 2, 1);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 6, 1);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 10, 1);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 1, 20, 1);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 2, 1);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 6, 1);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 10, 1);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 2, 20, 1); 
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 2, 1);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 6, 1);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 10, 1);
      fgrpInst = sfe.addArgBrownNgram(fgrpInst, matchingSentences, relation, classMap, 3, 20, 1);
  
      fgrpInst = sfe.tokenJaccardCoefficient(fgrpInst, matchingSentences, relation);
      fgrpInst = sfe.letterBigramJaccardCoefficient(fgrpInst, matchingSentences, relation);
      fgrpInst = sfe.addAcronymFeature(fgrpInst, matchingSentences, relation);
      fgrpInst = sfe.prefixOverlap(fgrpInst, matchingSentences, relation);
      fgrpInst = sfe.suffixOverlap(fgrpInst, matchingSentences, relation);
      
      //fgrpInst = sfe.addSlotLetterNgrams(fgrpInst, matchingSentences, relation);
      fgrpInst = sfe.addSlotCharacterNgrams(fgrpInst, matchingSentences, relation, 2);
      fgrpInst = sfe.addSlotCharacterNgrams(fgrpInst, matchingSentences, relation, 3);
      fgrpInst = sfe.addSlotCharacterNgrams(fgrpInst, matchingSentences, relation, 4);
      fgrpInst = sfe.addArgCapitalization(fgrpInst, matchingSentences, relation, 0);
      fgrpInst = sfe.addArgCapitalization(fgrpInst, matchingSentences, relation, 1);    
      fgrpInst = sfe.addSlotTokenCount(fgrpInst, matchingSentences, relation);
      fgrpInst = sfe.addSlotLogCharacterCount(fgrpInst, matchingSentences, relation);

      fgrpInst = sfe.sortAndSumFeatures(fgrpInst);
      fgrpInst = sfe.normalizeFeaturesToMax(fgrpInst);
      
      inst = sfe.mergeFeatures(inst, fgrpInst.build());
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }
    
    if (featuretypes.contains(FGRP_SKIP_FUZZY)) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding feature set: " + FGRP_SKIP_FUZZY);
      }
      Instance.Builder fgrpInst = Instance.newBuilder();
      
      fgrpInst = sfe.addSkip(fgrpInst, matchingSentences, relation, 2, false);
      fgrpInst = sfe.addSkip(fgrpInst, matchingSentences, relation, 3, false);
      fgrpInst = sfe.addSkip(fgrpInst, matchingSentences, relation, 4, false);
      
      //fgrpInst = sfe.addOutsideSkipFuzzy(fgrpInst, matchingSentences, relation, 2, 4);
      //fgrpInst = sfe.addOutsideSkipFuzzy(fgrpInst, matchingSentences, relation, 3, 4);
      //fgrpInst = sfe.addOutsideSkipFuzzy(fgrpInst, matchingSentences, relation, 4, 4);
      
      fgrpInst = sfe.sortAndSumFeatures(fgrpInst);
      fgrpInst = sfe.normalizeFeaturesToMax(fgrpInst);
      
      inst = sfe.mergeFeatures(inst, fgrpInst.build());
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }
    
    if (featuretypes.contains(FGRP_INTERTEXT)) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding feature set: " + FGRP_INTERTEXT);
      }
      Instance.Builder fgrpInst = Instance.newBuilder();
      
      fgrpInst = sfe.addIntertext(fgrpInst, matchingSentences, relation);

      fgrpInst = sfe.sortAndSumFeatures(fgrpInst);
      fgrpInst = sfe.normalizeFeaturesToMax(fgrpInst);
      
      inst = sfe.mergeFeatures(inst, fgrpInst.build());
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }

    if (featuretypes.contains(FGRP_INTERTEXT_SHORT)) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding feature set: " + FGRP_INTERTEXT_SHORT);
      }
      Instance.Builder fgrpInst = Instance.newBuilder();

      fgrpInst = sfe.addIntertextShort(fgrpInst, matchingSentences, relation);

      fgrpInst = sfe.sortAndSumFeatures(fgrpInst);
      fgrpInst = sfe.normalizeFeaturesToMax(fgrpInst);

      inst = sfe.mergeFeatures(inst, fgrpInst.build());
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }
    
    if (featuretypes.contains(FGRP_SKIP_EXACT)) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding feature set: " + FGRP_SKIP_EXACT);
      }
      Instance.Builder fgrpInst = Instance.newBuilder();
      fgrpInst = sfe.addSkip(fgrpInst, matchingSentences, relation, 3, true);
      fgrpInst = sfe.addSkip(fgrpInst, matchingSentences, relation, 4, true);

      //fgrpInst = sfe.addOutsideSkipExact(fgrpInst, matchingSentences, relation, 3, 4, true);
      //fgrpInst = sfe.addOutsideSkipExact(fgrpInst, matchingSentences, relation, 4, 4, true);
      
      fgrpInst = sfe.sortAndSumFeatures(fgrpInst);
      fgrpInst = sfe.normalizeFeaturesToMax(fgrpInst);
      
      inst = sfe.mergeFeatures(inst, fgrpInst.build());
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }
    
    if (featuretypes.contains(FGRP_TOPIC)) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding feature set: " + FGRP_TOPIC);
      }
      Instance.Builder fgrpInst = Instance.newBuilder();
      fgrpInst = sfe.addTopics(fgrpInst, matchingSentences);    
      fgrpInst = sfe.sortAndSumFeatures(fgrpInst);
      fgrpInst = sfe.normalizeFeaturesToMax(fgrpInst);
      
      inst = sfe.mergeFeatures(inst, fgrpInst.build());
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }
    
    if (featuretypes.contains(FGRP_MINTZ_NOARGS)) {
      if (firstMethodInvocationInstance) {
        logger.debug("Adding feature set: " + FGRP_MINTZ_NOARGS);
      }
      Instance.Builder fgrpInst = Instance.newBuilder();
      fgrpInst = sfe.addMintzNoArgsLexical(fgrpInst, matchingSentences, relation, 0);
      fgrpInst = sfe.addMintzNoArgsLexical(fgrpInst, matchingSentences, relation, 1);
      fgrpInst = sfe.addMintzNoArgsLexical(fgrpInst, matchingSentences, relation, 2);
      fgrpInst = sfe.addMintzNoArgsSyntactic(fgrpInst, matchingSentences, relation);
      fgrpInst = sfe.sortAndSumFeatures(fgrpInst);
      fgrpInst = sfe.normalizeFeaturesToMax(fgrpInst);
      
      inst = sfe.mergeFeatures(inst, fgrpInst.build());
      if (firstMethodInvocationInstance) {
        logger.debug("Nr. of features for instance: " + inst.getFeatureList().size());
      }
    }
    
    inst = sfe.sortAndSumFeatures(inst);
    
    if (firstMethodInvocationInstance) {
      logger.debug("Finished creation of first instance. Final nr. of features: " + inst.getFeatureList().size());
    }

    firstMethodInvocationInstance = false;
    return inst;
  }

  public static void main(String[] args) throws IOException {
    if (args.length < 6) {
      throw new IllegalArgumentException("Features <feature_map> <brown_classes> <single=true|false> <update_map=true|false> <sentences_in> <features_out> <featuretypes>");
    }
    File featureMapFile = new File(args[0]);
    String classesFn = args[1];
    boolean singleSentenceMode = args[2].equals("true");
    boolean updateFeatureMap = args[3].equals("true");
    String sentenceFn = args[4];
    String featuresFn = args[5];
    Set<String> featuretypes = new HashSet<String>();
    if (args.length >= 7) {
      for (String type : args[6].split(",")) {
        featuretypes.add(type);
        if (!type.equals(FGRP_NGRAM_DIRECTED) &&
            !type.equals(FGRP_NGRAM_LONG_DIRECTED) &&
            !type.equals(FGRP_BROWN_NGRAM_DIRECTED) &&
            !type.equals(FGRP_BROWN_NGRAM_LONG_DIRECTED) &&
            !type.equals(FGRP_SUB_BROWN_NGRAM_DIRECTED) &&
            !type.equals(FGRP_SUB_BROWN_NGRAM_LONG_DIRECTED) &&
            !type.equals(FGRP_SKIP_FUZZY) &&
            !type.equals(FGRP_SKIP_EXACT) &&
            !type.equals(FGRP_INTERTEXT) &&
            !type.equals(FGRP_INTERTEXT_SHORT) &&
            !type.equals(FGRP_TOPIC) &&
            !type.equals(FGRP_MINTZ_NOARGS)) {
          throw new IllegalArgumentException("unknown feaure type: " + type);
        }
      }
    }
    logger.debug("Feature types: " + featuretypes);
    
    Map<String, String> vocabMap = null;
    vocabMap = new HashMap<String, String>();
    BufferedReader br = new BufferedReader(new FileReader(classesFn));
    for (String classWord; (classWord = br.readLine()) != null;) {
      String[] lineParts = classWord.split("\\t");
      vocabMap.put(lineParts[1], lineParts[0]);
    }
    vocabMap.remove(SentenceFeatureExtractor.UNKNOWN_TOKEN);
    br.close();
    
    SentenceFeatureExtractor sfe;
    // Sfe reads creates new feature map or reads in existing one.
    if (featureMapFile.exists()) {
      br = new BufferedReader(new FileReader(featureMapFile));
      sfe = new SentenceFeatureExtractor(br, true, updateFeatureMap);
      br.close(); 
    } else {
      sfe = new SentenceFeatureExtractor(true);
    }
    
    BufferedInputStream sentenceIs = new BufferedInputStream(
        new FileInputStream(sentenceFn));
    BufferedWriter featuresBw = new BufferedWriter(new FileWriter(featuresFn));

    String prevRelation = null;
    String prevArgs = null;
    List<Document> matchingSentences = new ArrayList<Document>();
    for (Document sentence; (sentence = Document.parseDelimitedFrom(sentenceIs)) != null;) {
      String relation = null;
      String arguments = null;
      for (CompoundGroup cg : sentence.getCompoundList()) {
        if (cg.getType() == AnnotationType.PROPERTY) {
          if (cg.getCompoundCount() > 0) {
            Compound c = cg.getCompound(0);
            relation = c.getText();
            arguments = DocumentExtractor.canonicalArg(sentence, 0, relation)
                + "\t" + DocumentExtractor.argumentText(sentence, relation, 1);
          }
        }
      }
      if (prevRelation != null
          && (!arguments.equals(prevArgs) || !relation.equals(prevRelation) || singleSentenceMode)) {
        // relation or arguments changed, so the previous group is finished
        writeFeatures(matchingSentences, sfe, prevRelation, vocabMap,
            featuresBw, featuretypes);
        matchingSentences.clear();
      }
      matchingSentences.add(sentence);
      prevRelation = relation;
      prevArgs = arguments;
    }
    // do not forget to process last group
    if (prevRelation != null) {
      writeFeatures(matchingSentences, sfe, prevRelation, vocabMap, featuresBw, 
          featuretypes);
      matchingSentences.clear();
    }
    featuresBw.close();
    sentenceIs.close();
    
    if (updateFeatureMap) {
      BufferedWriter bw = new BufferedWriter(new FileWriter(featureMapFile));
      sfe.writeFeatureMap(bw);
      bw.close();
    }
  }
}
