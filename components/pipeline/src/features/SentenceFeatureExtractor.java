package features;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.collections.bag.SynchronizedSortedBag;
import org.apache.lucene.analysis.WhitespaceAnalyzer;
import org.apache.lucene.index.Term;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.search.BooleanClause.Occur;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.PhraseQuery;
import org.apache.lucene.util.Version;

import parser.DependencyTree;

import rerac.protos.Corpus.Document;
import rerac.protos.Corpus.Document.Annotation;
import rerac.protos.Corpus.Document.AnnotationType;
import rerac.protos.Corpus.Document.Compound;
import rerac.protos.Corpus.Document.CompoundGroup;
import rerac.protos.Corpus.Document.Method;
import rerac.protos.Corpus.Document.Token;
import rerac.protos.InstanceCollection.FeatureMap;
import rerac.protos.InstanceCollection.Instance;
import rerac.protos.InstanceCollection.Instance.Builder;
import rerac.protos.InstanceCollection.Instance.Feature;
import util.DocumentExtractor;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;

/**
 * Class for extracting feature counts from documents.
 * Features are strings, but they can be stored as hashes as well in order to 
 * save memory. In this case it may happen (although with a small chance only) 
 * that two different features are indistinguishable as they have the same hash.
 * 
 * See extract() for a description of the features extracted.
 * 
 * @author Benjamin Roth
 *
 */
public class SentenceFeatureExtractor {
  // Maps feature strings to numbered ids.
  BiMap<String, Integer> features = HashBiMap.create();
  // Maps feature hashes to numbered ids.
  BiMap<Integer, Integer> featureHashes = HashBiMap.create();
  // Whether new features are addded on the fly (true), or only features 
  // already in the map are extracted (false);
  boolean updateMap = true;
  
  boolean useHash = false;
  double[] binRanges = new double[]{0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0};

  public static final String ARG_BROWN_NGRAM_FEATURE = "#ABNG#";
  public static final String ARG_CAPITALIZATION_FEATURE = "#ACP#";
  public static final String ACRONYM_FEATURE = "#ACRO#";
  public static final String ARG_LOG_CHAR_COUNT_FEATURE = "#ALCC#";
  public static final String ARG_FEATURE_PREFIX = "#ARG#";
  public static final String ARG_TOKEN_COUNT_FEATURE = "#ATC#";
  public static final String BETWEEN_BROWN_NGRAM_FEATURE = "#BBNG#";
  public static final String BIGRAM_FEATURE = "#BGR#";
  public static final String BETWEEN_NGRAM_FEATURE = "#BNG";
  public static final String DISTANCE_BIN_FEATURE = "#DB#";
  public static final String END_LETTERS_PREFIX = "#END_LTRS#";
  public static final String PERMUTATION_INVARIANT_OVERLAP_FEATURE = "#INV_OVLP#";
  public static final String INTERTEXT_FEATURE = "#INTERTEXT#";
  public static final String INTERTEXT_SHORT_FEATURE = "#INTERTEXT_SHORT#";
  public static final String LETTER_BIGRAM_JACCARD_FEATURE = "#LBJ#";
  public static final String LDA_FEATURE_PREFIX = "#LDA#";
  public static final String LOG_PAIR_DOCFREQ_FEATURE = "#LPDF#";
  public static final String MINTZ_LEXICAL_FEATURE = "#MLEX#";
  public static final String MINTZ_SYNTACTIC_FEATURE = "#MSYN#";
  public static final String NEAR_ACRONYM_FEATURE = "#NACRO#";
  public static final String NGD_FEATURE = "#NGD#";
  public static final String OUTSIDE_BROWN_NGRAM_FEATURE = "#OBNG#";
  public static final String OUTSIDE_NGRAM_FEATURE = "#ONG";
  public static final String OVERLAP_WORD_FEATURE = "#OVW#";
  public static final String PPMI_FEATURE = "#PPMI#";
  public static final String PREFIX_OVERLAP_FEATURE = "#PRF_OVLP#";
  public static final String SKIP_FEATURE = "#SKIP#";
  public static final String STEM_FEATURE_PREFIX = "#STEM#";
  public static final String START_LETTERS_PREFIX = "#STT_LTRS#";
  public static final String SUFFIX_OVERLAP_FEATURE = "#SUF_OVLP#";
  public static final String TOPIC_FEATURE = "#TOPIC#";
  public static final String UNKNOWN_TOKEN = "<UNK>";
  
  static QueryParser DEFAULT_PARSER = new QueryParser(Version.LUCENE_29, "contents", 
      new WhitespaceAnalyzer());

  /**
   * 
   * @param useFeatureHash whether features are represented as hashes (true) 
   * or full strings (false). 
   */
  public SentenceFeatureExtractor(boolean useFeatureHash) {
    useHash = useFeatureHash;
    updateMap = true;
  }
  
  /**
   * This adds features extending an initial feature map.
   * Features are stored as hashes if the initial feature map already has at 
   * least one feature stored as a has, otherwise features are stored as 
   * strings.
   * 
   * @param map
   */
  public SentenceFeatureExtractor(FeatureMap map, boolean update) {
    if (map.getFeatureHashCount() > 0) {
      useHash = true;
    } else {
      useHash = false;
    }
    if (useHash) {
      for (int i = 0; i < map.getFeatureHashCount(); ++i) {
        // Some classification toolkits need features start with 1.
        featureHashes.put(map.getFeatureHash(i), i + 1);
      }
    } else {
      for (int i = 0; i < map.getFeatureCount(); ++i) {
        // Some classification toolkits need features start with 1.
        features.put(map.getFeature(i), i + 1);
      }
    }
    updateMap = update;
    /*
    for (int i = 0; i < map.getCategoryCount(); ++i) {
      this.labels.put(map.getCategory(i), i);
    }
    */
  }
  
  
  public SentenceFeatureExtractor(List<String> featureList, 
      boolean interpreteAsHash, boolean update) {
    useHash = interpreteAsHash;
    if (useHash) {
      for (int i = 0; i < featureList.size(); ++i) {
          // Some classification toolkits need features start with 1.
        featureHashes.put(Integer.parseInt(featureList.get(i)), i + 1);
      }
    } else {
      for (int i = 0; i < featureList.size(); ++i) {
        // Some classification toolkits need features start with 1.
        features.put(featureList.get(i), i + 1);
      }      
    }
    updateMap = update;
  }

  public SentenceFeatureExtractor(BufferedReader featureListReader, 
      boolean interpreteAsHash, boolean update) throws IOException {
    useHash = interpreteAsHash;
    int i = 1;
    for (String line; (line = featureListReader.readLine()) != null;) {
      if (useHash) {
        featureHashes.put(Integer.parseInt(line), i);
      } else {
        features.put(line, i);
      }
      i += 1;
    }
    updateMap = update;
  }
  
  /**
   * This sets the class label of the instance for which features are to be 
   * extracted.
   * 
   * @param instance
   * @param label
   */
  public Instance.Builder setLabel(Builder instance, String label, boolean isPositive) {
    /*
    Integer labelIdx = labels.size();
    if (labels.containsKey(label)) {
      labelIdx = labels.get(label);
    } else {
      labels.put(label, labelIdx);
    }
    instance.setCategoryIndex(labelIdx);*/
    instance.setCategory(label);
    instance.setPositive(isPositive);
    return instance;
  }
  
  /**
   * Returns the index of the feature; a new index is added if the feature 
   * occurs the first time.
   * This methods is to be used if useHash == false.
   * 
   * @param fStr feature string
   * @return the index
   */
  private int getFeatureIdx(String fStr) {
    Integer featureIdx = -1;
    if (features.containsKey(fStr)) {
      featureIdx = features.get(fStr);
    } else if (updateMap) {
      featureIdx = features.size() + 1;
      features.put(fStr, featureIdx);
    }
    return featureIdx;
  }

  /**
   * Returns the index of hash of a feature; a new index is added if the 
   * corresponding hash occurs the first time.
   * This methods is to be used if useHash == true.
   * 
   * @param fStr feature string
   * @return the index
   */
  private int getFeatureHashIdx(String fStr) {
    int fHash = fStr.hashCode();
    Integer featureIdx = -1;
    if (featureHashes.containsKey(fHash)) {
      featureIdx = featureHashes.get(fHash);
    } else if (updateMap){
      featureIdx = featureHashes.size() + 1;
      featureHashes.put(fHash, featureIdx);
    }
    return featureIdx;
  }
  
  /**
   * This adds a feature with a given count to an instance. If the feature 
   * already exists, the counts are added.
   * 
   * @param instance the instance this feature is added to.
   * @param fStr the feature.
   * @param fCount the count of the feature.
   */
  public Instance.Builder addFeature(Instance.Builder instance, String fStr, 
        double fCount) {
    
    int featureIdx;
    if (useHash) {
      featureIdx = getFeatureHashIdx(fStr);
    } else {
      featureIdx = getFeatureIdx(fStr);
    }
    if (featureIdx != -1) {
      Feature.Builder feat = 
          Feature.newBuilder().setIndex(featureIdx).setValue(fCount);
      instance.addFeature(feat);
    }
    return instance;
  }

  /**
   * This adds features to an instance, each with a count of 1.0. If a feauture
   * exists already, counts are added.
   * 
   * @param instance the instance features are added to.
   * @param featureStrings the features.
   */
  private void addFeatures(Instance.Builder instance, String[] featureStrings, 
      double weight) {
    for (String fStr : featureStrings) {
      addFeature(instance, fStr, weight);
    }
  }
  
  private void addFeatures(Instance.Builder instance, List<String> featureStrings, 
      double weight) {
    for (String fStr : featureStrings) {
      addFeature(instance, fStr, weight);
    }
  }
  
  private void addFeatures(Instance.Builder instance, String[] featureStrings) {
    addFeatures(instance, featureStrings, 1.0);
  }

  public Instance.Builder emptyInstance(String relation, 
      boolean isPositive) {
    Instance.Builder instance = Instance.newBuilder();
    setLabel(instance, relation, isPositive);
    return instance;
  }
  
  public Instance.Builder extractCoarse(Instance.Builder instance, 
      List<Document> docList, String relation, 
      boolean isPositive) {
    for (Document doc : docList) {
      for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
        CompoundGroup cg = doc.getCompound(cgi);
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (int ci = 0; ci < cg.getCompoundCount(); ++ci) {
            Compound c = cg.getCompound(ci);
            if (c.getText().equals(relation)) {
              instance = extractCoarse(doc, cgi, ci, instance);
            }
          }
        }
      }
    }
    return instance;
  }
  
  public Instance.Builder addSlotClasses(Instance.Builder inst,
      List<Document> matchingSentences, String relation, 
      Map<String, String> classMap) {
    if (matchingSentences.size() == 0) {
      return inst;
    }
    String[] argTokens = DocumentExtractor.
        argumentText(matchingSentences.get(0), relation, 1).split(" ");
    // no sentence weight used, since features are sentence independent.
    for (String tok : argTokens) {
      String tcl = classMap.containsKey(tok) ? classMap.get(tok) : UNKNOWN_TOKEN;
      addFeature(inst, ARG_FEATURE_PREFIX+tcl, 1.0 / (double) argTokens.length );  
    }
    return inst;
  }
  
  /**
   * Token prefix overlap between arguments.
   * @param inst
   * @param matchingSentences
   * @param relation
   * @return
   */
  public Instance.Builder prefixOverlap(Instance.Builder inst,
      List<Document> matchingSentences, String relation) {
    if (matchingSentences.size() == 0) {
      return inst;
    }
    
    String arg1Str = DocumentExtractor.
        argumentText(matchingSentences.get(0), relation, 0).toLowerCase();
    String arg2Str = DocumentExtractor.
        argumentText(matchingSentences.get(0), relation, 1).toLowerCase();
    
    //if (arg1Str.equals(arg2Str)) {
    //  return inst;
    //}
    
    String[] arg1Tokens = arg1Str.split(" ");
    String[] arg2Tokens = arg2Str.split(" ");
    
    int overlap = 0;
    
    while (overlap < arg1Tokens.length && overlap < arg2Tokens.length &&
        arg1Tokens[overlap].equals(arg2Tokens[overlap])) {
      ++overlap;
    }
    // no sentence weight used -- feature sentence independent.
    if (overlap > 0) {
      double score = overlap * 2.0 / (arg1Tokens.length + arg2Tokens.length);
      if (useBins()) {
        addBinarizedFeature(inst, PREFIX_OVERLAP_FEATURE, score);
      } else {
        addFeature(inst, PREFIX_OVERLAP_FEATURE, score);
      }
    }

    return inst;
  }
  
  /**
   * Token suffix overlap between arguments.
   * @param inst
   * @param matchingSentences
   * @param relation
   * @return
   */
  public Instance.Builder suffixOverlap(Instance.Builder inst,
      List<Document> matchingSentences, String relation) {
    if (matchingSentences.size() == 0) {
      return inst;
    }
    
    String arg1Str = DocumentExtractor.
        argumentText(matchingSentences.get(0), relation, 0).toLowerCase();
    String arg2Str = DocumentExtractor.
        argumentText(matchingSentences.get(0), relation, 1).toLowerCase();
    
    //if (arg1Str.equals(arg2Str)) {
    //  return inst;
    //}
    
    String[] arg1Tokens = arg1Str.split(" ");
    String[] arg2Tokens = arg2Str.split(" ");
    
    int lastArg1Idx = arg1Tokens.length - 1;
    int lastArg2Idx = arg2Tokens.length - 1;
    
    int overlap = 0;
    
    while (overlap < arg1Tokens.length && overlap < arg2Tokens.length &&
        arg1Tokens[lastArg1Idx - overlap].equals(arg2Tokens[lastArg2Idx - overlap])) {
      ++overlap;
    }
    
    // no sentence weight used -- feature sentence independent.
    if (overlap > 0) {
      double score = overlap * 2.0 / (arg1Tokens.length + arg2Tokens.length);
      if (useBins()) {
        addBinarizedFeature(inst, SUFFIX_OVERLAP_FEATURE, score);        
      } else {
        addFeature(inst, SUFFIX_OVERLAP_FEATURE, score);
      }
    }

    return inst;
  }
  
  /**
   * Jaccard coefficient between tokens of relation arguments.
   * 
   * @param inst
   * @param matchingSentences
   * @param relation
   * @return
   */
  public Instance.Builder tokenJaccardCoefficient(Instance.Builder inst,
      List<Document> matchingSentences, String relation) {
    if (matchingSentences.size() == 0) {
      return inst;
    }
    String arg1Str = DocumentExtractor.
        argumentText(matchingSentences.get(0), relation, 0).toLowerCase();
    String arg2Str = DocumentExtractor.
        argumentText(matchingSentences.get(0), relation, 1).toLowerCase();

    Set<String> arg1Tokens = new HashSet<String>();
    Set<String> arg2Tokens = new HashSet<String>();
    for (String t : arg1Str.split(" ")) {
      arg1Tokens.add(t);
    }
    for (String t : arg2Str.split(" ")) {
      arg2Tokens.add(t);
    }
    Set<String> union = new HashSet<String>();
    union.addAll(arg1Tokens);
    union.addAll(arg2Tokens);
    Set<String> intersection = new HashSet<String>();
    intersection.addAll(arg1Tokens);
    intersection.retainAll(arg2Tokens);
    
    // no sentence weight used -- feature sentence independent.
    if (intersection.size() > 0) {
      double score = intersection.size() / (double) union.size();
      if (useBins()) {
        addBinarizedFeature(inst, PERMUTATION_INVARIANT_OVERLAP_FEATURE, score);        
      } else {
        addFeature(inst, PERMUTATION_INVARIANT_OVERLAP_FEATURE, score);
      }
    }
    return inst;
  }
  
  public Instance.Builder overlapWords(Instance.Builder inst,
      List<Document> matchingSentences, String relation) {
    if (matchingSentences.size() == 0) {
      return inst;
    }
    String arg1Str = DocumentExtractor.
        argumentText(matchingSentences.get(0), relation, 0).toLowerCase();
    String arg2Str = DocumentExtractor.
        argumentText(matchingSentences.get(0), relation, 1).toLowerCase();

    Set<String> arg1Tokens = new HashSet<String>();
    Set<String> arg2Tokens = new HashSet<String>();
    for (String t : arg1Str.split(" ")) {
      arg1Tokens.add(t);
    }
    for (String t : arg2Str.split(" ")) {
      arg2Tokens.add(t);
    }
    Set<String> intersection = new HashSet<String>();
    intersection.addAll(arg1Tokens);
    intersection.retainAll(arg2Tokens);
    
    // no sentence weight used -- feature sentence independent.
    if (intersection.size() > 0) {
      //double score = 1.0 / intersection.size();
      for (String token : intersection) {
        addFeature(inst, OVERLAP_WORD_FEATURE + token, 1.0);
      }
    }
    return inst;
  }
  
  /**
   * Jaccard coefficient between letter trigrams of relation arguments.
   * 
   * @param inst
   * @param matchingSentences
   * @param relation
   * @return
   */
  public Instance.Builder letterBigramJaccardCoefficient(Instance.Builder inst,
      List<Document> matchingSentences, String relation) {
    if (matchingSentences.size() == 0) {
      return inst;
    }
    String arg1Str = DocumentExtractor.
        argumentText(matchingSentences.get(0), relation, 0).toLowerCase().replaceAll("[^\\p{L}\\p{N}]", "");
    String arg2Str = DocumentExtractor.
        argumentText(matchingSentences.get(0), relation, 1).toLowerCase().replaceAll("[^\\p{L}\\p{N}]", "");

    if (arg1Str.length() < 2 || arg2Str.length() < 2) {
      return inst;
    }
    Set<String> arg1Bigrams = new HashSet<String>();
    Set<String> arg2Bigrams = new HashSet<String>();
    for (int i = 1; i < arg1Str.length(); ++i) {
      arg1Bigrams.add(arg1Str.substring(i-1, i));
    }
    for (int i = 1; i < arg2Str.length(); ++i) {
      arg2Bigrams.add(arg2Str.substring(i-1, i));
    }
    Set<String> union = new HashSet<String>();
    union.addAll(arg1Bigrams);
    union.addAll(arg2Bigrams);
    Set<String> intersection = new HashSet<String>();
    intersection.addAll(arg1Bigrams);
    intersection.retainAll(arg2Bigrams);
    
    // no sentence weight used -- feature sentence independent.
    if (intersection.size() > 0) {
      double score = intersection.size() / (double) union.size();
      if (useBins()) {
        addBinarizedFeature(inst, LETTER_BIGRAM_JACCARD_FEATURE, score);        
      } else {
        addFeature(inst, LETTER_BIGRAM_JACCARD_FEATURE, score);
      }
    }
    return inst;
  }
  
  /**
   * Boolean feature whether one argument is the acronym of the other.
   * @param inst
   * @param matchingSentences
   * @param relation
   * @return
   */
  public Instance.Builder addAcronymFeature(Instance.Builder inst,
      List<Document> matchingSentences, String relation) {
    if (matchingSentences.size() == 0) {
      return inst;
    }
    
    String arg1Str = DocumentExtractor.
        argumentText(matchingSentences.get(0), relation, 0);
    String arg2Str = DocumentExtractor.
        argumentText(matchingSentences.get(0), relation, 1);
    
    String[] arg1Tokens = arg1Str.split(" ");
    String[] arg2Tokens = arg2Str.split(" ");

    // no sentence weight used -- feature sentence independent.
    if (acronym(arg1Tokens).equals(arg2Str) || acronym(arg2Tokens).equals(arg1Str)) {
      //System.out.println(arg1Str + "\t" + arg2Str);
      addFeature(inst, ACRONYM_FEATURE, 1.0);
    }
    return inst;
  }
    
  private String acronym(String[] arg2Tokens) {
    StringBuffer sb = new StringBuffer();
    for (String t : arg2Tokens) {
      char[] c = t.toCharArray();
      if (c.length == 0) continue;
      if (Character.isUpperCase(c[0])) {
        sb.append(c[0]);
      }
    }
    return sb.toString();
  }

  /**
   * Letter n-gram of start and end of slot argument.
   * @param inst
   * @param matchingSentences
   * @param relation
   * @param n
   * @return
   */
  public Instance.Builder addSlotCharacterNgrams(Instance.Builder inst,
      List<Document> matchingSentences, String relation, int n) {
    if (matchingSentences.size() == 0) {
      return inst;
    }
    String arg2Str = DocumentExtractor.argumentText(matchingSentences.get(0), relation, 1);
    String startNgram = arg2Str.substring(0, Math.min(n, arg2Str.length()));
    String endNgram = arg2Str.substring(Math.max(0, arg2Str.length() - n), arg2Str.length());
    // no sentence weight used -- feature sentence independent.
    addFeature(inst, START_LETTERS_PREFIX + n + "#" + startNgram, 1.0);
    addFeature(inst, END_LETTERS_PREFIX + n + "#" + endNgram, 1.0);
    return inst;
  }
  
  public Instance.Builder addPPMI(Instance.Builder inst,
      List<Document> matchingSentences, String relation, IndexSearcher is) {
    if (matchingSentences.size() == 0) {
      return inst;
    }
    String arg1 = DocumentExtractor.argumentText(matchingSentences.get(0), relation, 0);
    String arg2 = DocumentExtractor.argumentText(matchingSentences.get(0), relation, 1);

    PhraseQuery arg1query = new PhraseQuery();
    for (String t : arg1.split(" ")) {
      arg1query.add(new Term("contents", t));      
    }
    PhraseQuery arg2query = new PhraseQuery();
    for (String t : arg2.split(" ")) {
      arg2query.add(new Term("contents", t));      
    }
    BooleanQuery pairQuery = new BooleanQuery();
    pairQuery.add(arg1query, Occur.MUST);
    pairQuery.add(arg2query, Occur.MUST);
    
    try {
      int pairCount = is.search(pairQuery, 1).totalHits;
      if (pairCount > 0) {
        int arg1Count = is.search(arg1query,1).totalHits;
        int arg2Count = is.search(arg2query,1).totalHits;        
        int n = is.getIndexReader().numDocs();
        double pmi = (Math.log(pairCount) - Math.log(arg1Count) - Math.log(arg2Count) + Math.log(n)) / Math.log(n);
        
        if (Double.isInfinite(pmi)) {
          System.out.println(pairCount);
          System.out.println(arg1Count);
          System.out.println(arg2Count);
          System.out.println(n);
          throw new IllegalStateException("Impossible score");
        }
        
        // no sentence weight used -- feature sentence independent.
        if (pmi > 0) {
          if (useBins()) {
            addBinarizedFeature(inst, PPMI_FEATURE, pmi);            
          } else {
            addFeature(inst, PPMI_FEATURE, pmi);
          }
        }
        if (pairCount > 1) {
          double logfreq = Math.log(pairCount) / Math.log(n);
          if (useBins()) {
            addBinarizedFeature(inst, LOG_PAIR_DOCFREQ_FEATURE, logfreq);
          } else {
            addFeature(inst, LOG_PAIR_DOCFREQ_FEATURE, logfreq);
          }
        }
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
    return inst;
  }
  
  
  private Instance.Builder addBinarizedFeature(Builder inst, String featureStr, double score) {
    if (null == binRanges) {
      return inst;
    }
    for (int i = 0; i < binRanges.length; ++i) {
      if (score <= binRanges[i]) {
        return addFeature(inst, featureStr + i, 1.0);
      }
    }
    return addFeature(inst, featureStr + binRanges.length, 1.0);
  }

  private boolean useBins() {
    return null != binRanges;
  }

  /**
   * This puts the distance between the two args in bins, that are doubling in
   * size relative to the distance.
   * 
   * @param inst
   * @param matchingSentences
   * @param relation
   * @return
   */
  public Instance.Builder addDistanceBins(Instance.Builder inst,
      List<Document> matchingSentences, String relation) {
    for (Document doc : matchingSentences) {
      for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
        CompoundGroup cg = doc.getCompound(cgi);
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (int ci = 0; ci < cg.getCompoundCount(); ++ci) {
            Compound c = cg.getCompound(ci);
            if (c.getText().equals(relation)) {
              int arg1Start = c.getSlot(0).getStartToken();
              int arg1End = c.getSlot(0).getEndToken();
              int arg2Start = c.getSlot(1).getStartToken();
              int arg2End = c.getSlot(1).getEndToken();
              int dist = 0;
              if (arg2Start > (arg1End - 1)) {
                dist = Math.min(arg2Start - (arg1End - 1), 50);
              } else if (arg1Start > (arg2End - 1)) {
                dist = Math.max((arg2End - 1) - arg1Start, -50);
              }
              // This lengthy term establishes the following mapping:
              // -15 .. -8 -> -4
              // -7 .. -4 -> -3
              // -3 .. -2 -> -2
              // -1 -> -1
              // 0 -> 0
              // 1 -> 1
              // ...
              int bin = dist == 0 ? 0 : 
                (int) Math.round( Math.signum(dist) * (1 + Math.floor(Math.log(Math.abs(dist))/Math.log(2))));
              double sentenceWeight = c.hasWeight() ? c.getWeight() : 1.0;
              addFeature(inst, DISTANCE_BIN_FEATURE + bin, sentenceWeight);
            }
          }
        }
      }
    }
    return inst;
  }
  
  /**
   * This adds bigrams of a brown class an the following token.
   * 
   * @param inst
   * @param docList
   * @param relation
   * @param isPositive
   * @param classMap
   * @return
   */
  public Instance.Builder extractBetweenClassConditionedBigram(Builder inst, 
      List<Document> docList, String relation, Map<String, String> classMap) {
    for (Document doc : docList) {
      for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
        CompoundGroup cg = doc.getCompound(cgi);
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (int ci = 0; ci < cg.getCompoundCount(); ++ci) {
            Compound c = cg.getCompound(ci);
            if (c.getText().equals(relation)) {
              inst = extractBetweenClassConditionedBigram(doc, cgi, ci, inst, classMap);
            }
          }
        }
      }
    }
    return inst;
  }
  
  /**
   * This adds all stems outside of the arguments.
   * 
   * @param instance
   * @param docList
   * @param relation
   * @param outside
   * @return
   */
  public Instance.Builder addOutsideStems(Instance.Builder instance, 
      List<Document> docList, String relation, boolean outside) {
    for (Document doc : docList) {
      for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
        CompoundGroup cg = doc.getCompound(cgi);
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (int ci = 0; ci < cg.getCompoundCount(); ++ci) {
            Compound c = cg.getCompound(ci);
            if (c.getText().equals(relation)) {
              instance = addOutsideStems(doc, cgi, ci, instance, outside);
            }
          }
        }
      }
    }
    return instance;
  }
  
  private Instance.Builder extractBetweenClassConditionedBigram(Document doc, int compoundGroupIndex, 
      int compoundIndex, Instance.Builder instance, Map<String, String> classMap) {
    List<String> tokenList = new ArrayList<String>();
    List<String> classList = new ArrayList<String>();
    Compound c = doc.getCompound(compoundGroupIndex).getCompound(compoundIndex);
    int argIdx1Start = c.getSlot(0).getStartToken();
    int argIdx1End = c.getSlot(0).getEndToken();
    int argIdx2Start = c.getSlot(1).getStartToken();
    int argIdx2End = c.getSlot(1).getEndToken();
    int leftEnd;
    int rightStart;
    boolean arg1first = argIdx1Start < argIdx2Start;
    if (arg1first) {
      leftEnd = argIdx1End;
      rightStart = argIdx2Start;
    } else {
      leftEnd = argIdx2End;
      rightStart = argIdx1Start;
    }
    tokenList.add(arg1first ? "ARG1" : "ARG2");
    classList.add(arg1first ? "ARG1" : "ARG2");
    for (int i = leftEnd; i < rightStart; ++i) {
      String tok = doc.getToken(i).getText();
      tokenList.add(tok);
      String tcl = classMap.containsKey(tok) ? classMap.get(tok) : UNKNOWN_TOKEN;
      classList.add(tcl);
    }
    tokenList.add(arg1first ? "ARG2" : "ARG1");
    classList.add(arg1first ? "ARG2" : "ARG1");
    List<String> bigramFeatures = new ArrayList<String>();
    for (int i = 1; i < tokenList.size(); ++i) {
      String bigram = BIGRAM_FEATURE + classList.get(i-1) + "#" + tokenList.get(i);
      bigramFeatures.add(bigram);
    }
    double sentenceWeight = c.hasWeight() ? c.getWeight() : 1.0;
    addFeatures(instance, bigramFeatures, sentenceWeight / bigramFeatures.size());
    return instance;
  }
  
  public Instance.Builder normalizeFeatures(Instance.Builder instance) {
    double featureSum = 0;
    for (Feature f : instance.getFeatureList()) {
      featureSum += f.getValue();
    }
    for (Feature.Builder f : instance.getFeatureBuilderList()) {
      f.setValue(f.getValue() / featureSum);
    }
    return instance;
  }
  
  /**
   * This normalizes an instance so that the maximal feature weight is 1.0 and
   * all the other feature weights are relative to that.
   * This can e.g. be used for not letting the number of sentences have an a
   * priory influence on prediction.
   * 
   * @param instance
   * @return
   */
  public Instance.Builder normalizeFeaturesToMax(Instance.Builder instance) {
    double featureMax = 0;
    for (Feature f : instance.getFeatureList()) {
      double v = Math.abs(f.getValue());
      if (v > featureMax) {
        featureMax = v;
      }
    }
    for (Feature.Builder f : instance.getFeatureBuilderList()) {
      f.setValue(f.getValue() / featureMax);
    }
    return instance;
  }
  
  public Instance.Builder binarizeFeatures(Instance.Builder instance) {
    for (Feature.Builder f : instance.getFeatureBuilderList()) {
      double val = f.getValue();
      if (val > 0) {
        f.setValue(1.0);
      } else if (val < 0) {
        f.setValue(-1.0);
      }
    }
    return instance;
  }
  
  
  private Instance.Builder addOutsideStems(Document doc, int compoundGroupIndex, 
      int compoundIndex, Instance.Builder instance, boolean outside) {
    int stemMethodInd = -1;
    for (int mi = 0; mi < doc.getMethodCount() && stemMethodInd == -1; ++mi) {
      Method m = doc.getMethod(mi);
      if (m.hasId() && m.getId().equals(DocumentExtractor.STEM)) {
        stemMethodInd = mi;
      }
    }
    
    if (-1 == stemMethodInd) {
      throw new IllegalStateException("No STEM annotation");
    }
    
    List<String> stemList = new ArrayList<String>(doc.getTokenCount());
    for (Token tok : doc.getTokenList()) {
      String stem = "";
      for (Annotation anno : tok.getAnnotationList()) {
        if (anno.getMethodIndex() == stemMethodInd) {
          stem = anno.getText();
          break;
        }
      }
      stemList.add(stem);
    }
        
    Compound c = doc.getCompound(compoundGroupIndex).getCompound(compoundIndex);
    if (c.getSlotCount() == 2) {
      int argIdx1Start = c.getSlot(0).getStartToken();
      int argIdx1End = c.getSlot(0).getEndToken();
  
      int argIdx2Start = c.getSlot(1).getStartToken();
      int argIdx2End = c.getSlot(1).getEndToken();
  
      int leftStart;
      int rightEnd;
      
      boolean arg1first = argIdx1Start < argIdx2Start;
      
      if (arg1first) {
        leftStart = argIdx1Start;
        rightEnd = argIdx2End;              
      } else {
        leftStart = argIdx2Start;
        rightEnd = argIdx1End;                  
      }
        
      List<String> stemFeatures = new ArrayList<String>(doc.getTokenCount());
      for (int i=0; i < doc.getTokenCount(); ++i) {
        if (outside && (i >= leftStart && i < rightEnd)) {
          continue;
        }
        if (!stemList.get(i).isEmpty()) {
          stemFeatures.add(STEM_FEATURE_PREFIX + stemList.get(i));
        }
      }
      if (stemFeatures.size() != 0) {
        double sentenceWeight = c.hasWeight() ? c.getWeight() : 1.0;
        addFeatures(instance, stemFeatures, sentenceWeight / stemFeatures.size());
      }
    }
    return instance;
  }


  /**
   * 
   * This extracts more coarse grained features, that are more likely to 
   * generalize well.
   * 
   * These features are:
   *  - The sequences of part-of-speech tags between the 
   *    arguments, if this sequence has a length of maximum 4.
   *    Otherwise, the sequence is represented by its length.
   *  - The sequences of part-of-speech tags one and two left (right) of
   *    the left (right) argument.
   *  - The stems of the tokens in the window starting two left
   *    of the left argument and ending two right of the right argument.
   *    Arguments are excluded.
   *  
   * @param doc
   * @param compoundGroupIndex
   * @param compoundIndex
   * @param instance
   * @return
   */
  // TODO: functionality moved entirely to Features (!?)
  private Instance.Builder extractCoarse(Document doc, int compoundGroupIndex, 
      int compoundIndex, Instance.Builder instance) {
    
    int posMethodInd = -1;
    for (int mi = 0; mi < doc.getMethodCount() && posMethodInd == -1; ++mi) {
      Method m = doc.getMethod(mi);
      if (m.hasId() && m.getId().equals(DocumentTagger.POS)) {
        posMethodInd = mi;
      }
    }
    
    if (-1 == posMethodInd) {
      throw new IllegalStateException("No POS annotation");
    }
    
    List<String> posList = new ArrayList<String>(doc.getTokenCount());
    for (Token tok : doc.getTokenList()) {
      String pos = "";
      for (Annotation anno : tok.getAnnotationList()) {
        if (anno.getMethodIndex() == posMethodInd) {
          pos = anno.getText();
          break;
        }
      }
      posList.add(pos);
    }
    
    int stemMethodInd = -1;
    for (int mi = 0; mi < doc.getMethodCount() && stemMethodInd == -1; ++mi) {
      Method m = doc.getMethod(mi);
      if (m.hasId() && m.getId().equals(DocumentExtractor.STEM)) {
        stemMethodInd = mi;
      }
    }
    
    if (-1 == stemMethodInd) {
      throw new IllegalStateException("No STEM annotation");
    }
    
    List<String> stemList = new ArrayList<String>(doc.getTokenCount());
    for (Token tok : doc.getTokenList()) {
      String stem = "";
      for (Annotation anno : tok.getAnnotationList()) {
        if (anno.getMethodIndex() == stemMethodInd) {
          stem = anno.getText();
          break;
        }
      }
      stemList.add(stem);
    }
        
    Compound c = doc.getCompound(compoundGroupIndex).getCompound(compoundIndex);
    if (c.getSlotCount() == 2) {
      int argIdx1Start = c.getSlot(0).getStartToken();
      int argIdx1End = c.getSlot(0).getEndToken();
  
      int argIdx2Start = c.getSlot(1).getStartToken();
      int argIdx2End = c.getSlot(1).getEndToken();
  
      int leftStart;
      int leftEnd;
      int rightStart;
      int rightEnd;
      
      boolean arg1first = argIdx1Start < argIdx2Start;
      
      if (arg1first) {
        leftStart = argIdx1Start;
        leftEnd = argIdx1End;
        rightStart = argIdx2Start;
        rightEnd = argIdx2End;              
      } else {
        leftStart = argIdx2Start;
        leftEnd = argIdx2End;
        rightStart = argIdx1Start;
        rightEnd = argIdx1End;                  
      }
      
      StringBuffer sb = new StringBuffer();
      for (int i = Math.max(0, leftStart - 2); i < leftStart; ++i) {
        if (sb.length() > 0) {
          sb.append(' ');
        }
        sb.append(posList.get(i));
      }
      String leftTwoPos = sb.toString();
      
      String leftPos = (leftStart > 0) ? 
          posList.get(leftStart - 1) : "";
      
      sb = new StringBuffer();
      for (int i = rightEnd; i < Math.min(rightEnd + 2, 
          doc.getTokenCount()); ++i) {
        if (sb.length() > 0) {
          sb.append(' ');
        }
        sb.append(posList.get(i));
      }
      String rightTwoPos = sb.toString();
      
      String rightPos = rightEnd < doc.getTokenCount() ? 
          posList.get(rightEnd) : "";

      int middleLength = rightStart - leftEnd;
      String middlePos;
      if (middleLength > 4) {
        middlePos = Integer.toString(middleLength);
      } else {
        sb = new StringBuffer();
        for (int i = leftEnd; i < rightStart; ++i) {
          if (sb.length() > 0) {
            sb.append(' ');
          }
          sb.append(doc.getToken(i).getText());
        }
        middlePos = sb.toString();
      }
      
      String leftArg = arg1first ? "ARG1" : "ARG2";
      String rightArg = arg1first ? "ARG2" : "ARG1";
  
      String[] featureStrings = new String[]{
          leftArg + " " + middlePos + " " + rightArg,
          leftPos + " " + leftArg,
          rightArg + " " + rightPos,
          leftTwoPos + " " + leftArg,
          rightArg + " " + rightTwoPos
      };
      addFeatures(instance, featureStrings);
  
      List<String> stemFeatures = new ArrayList<String>(doc.getTokenCount());
      for (int i=0; i < doc.getTokenCount(); ++i) {
        if ((i >= argIdx1Start && i < argIdx1End) || 
            (i >= argIdx2Start && i < argIdx2End)) {
          continue;
        }
        stemFeatures.add(stemList.get(i));
      }
      double sentenceWeight = c.hasWeight() ? c.getWeight() : 1.0;
      addFeatures(instance, stemFeatures, sentenceWeight / stemFeatures.size());
    }
    return instance;
  }

  
  /**
   * This returns the feature map, that maps features to indices.
   * 
   * @return the feature map.
   */
  public FeatureMap getFeatureMap() {
    FeatureMap.Builder fm = FeatureMap.newBuilder();
    if (useHash) {
      BiMap<Integer, Integer> idx2feat = featureHashes.inverse();
      for (int i = 1; i <= featureHashes.size(); ++i) {
        fm.addFeatureHash(idx2feat.get(i));
      }      
    } else {
      BiMap<Integer, String> idx2feat = features.inverse();
      for (int i = 1; i <= features.size(); ++i) {
        fm.addFeature(idx2feat.get(i));
      }
    }
    /*
    BiMap<Integer, String> idx2label = labels.inverse();
    for (int i = 0; i < labels.size(); ++i) {
      fm.addCategory(idx2label.get(i));
    }*/
    return fm.build();
  }
  
  public List<Integer> getFeatureHashes() {
    List<Integer> hashList = new ArrayList<Integer>();
    BiMap<Integer, Integer> idx2feat = featureHashes.inverse();
    for (int i = 1; i <= featureHashes.size(); ++i) {
      hashList.add(idx2feat.get(i));
    }
    return hashList;
  }
  
  public void writeFeatureMap(BufferedWriter bw) throws IOException {
    if (useHash) {
      BiMap<Integer, Integer> idx2feat = featureHashes.inverse();
      for (int i = 1; i <= featureHashes.size(); ++i) {
        bw.append(Integer.toString(idx2feat.get(i)));
        bw.newLine();
      }
    } else {
      BiMap<Integer, String> idx2feat = features.inverse();
      for (int i = 1; i <= features.size(); ++i) {
        bw.append(idx2feat.get(i));
        bw.newLine();
      }
    }
  }
/*
  public Instance.Builder addArgFeatures(Instance.Builder inst,
      List<Document> matchingSentences, String relation,
      int argNr) {
    String[] annotationIds = new String[]{DocumentExtractor.NE, DocumentExtractor.SS};
    return addArgFeatures(inst, matchingSentences, relation, argNr, 
        annotationIds);
  }
  
  public Instance.Builder addArgFeatures(Instance.Builder inst,
      List<Document> matchingSentences, String relation,
      int argNr, String[] annotationIds) {
    for (String id : annotationIds) {
      Map<String, Integer> feature2count = 
          DocumentExtractor.argumentAnnotationStatistics(id, 
              relation, argNr, matchingSentences);
      for (Entry<String, Integer> e : feature2count.entrySet()) {
        // TODO: make annotation id part of prefix.
        String fStr = ARG_FEATURE_PREFIX + " " + (argNr + 1) + " " + e.getKey(); 
        addFeature(inst, fStr, e.getValue());
      }      
    }
    return inst;
  }
  */
  /**
   * This sorts the features from small to big, and adds together features 
   * weights for repeated features. It should always be called before an 
   * feature instance is used.
   * 
   * @param instance
   * @return
   */
  public Builder sortAndSumFeatures(Builder instance) {
    SortedMap<Integer, Double> feat2val = new TreeMap<Integer, Double>();
    for (Feature f : instance.getFeatureList()) {
      double prevVal = feat2val.containsKey(f.getIndex()) ? 
          feat2val.get(f.getIndex()) : 0.0;
      feat2val.put(f.getIndex(), prevVal + f.getValue());
    }
    List<Feature> featureList = new ArrayList<Feature>(feat2val.size());
    for (Entry<Integer, Double> fe : feat2val.entrySet()) {
      featureList.add(
          Feature.newBuilder().setIndex(fe.getKey()).setValue(fe.getValue())
          .build());
    }
    instance.clearFeature();
    instance.addAllFeature(featureList);
    return instance;
  }
  
  public Builder mergeFeatures(Builder instance1, Instance instance2) {
    instance1.addAllFeature(instance2.getFeatureList());
    return sortAndSumFeatures(instance1);
  }

  /**
   * This adds, to an instance, n-grams of the tokens between the arguments for
   * all given sentences.
   * See also:
   * addBetweenNgram(Document doc, int cgi, int ci, Builder inst, int n)
   * @param inst the instance features are added to
   * @param matchingSentences
   * @param relation The relation for which arguments are considered.
   * @param n Size of n-grams.
   * @return the instance passed as an argument
   */
  public Builder addBetweenNgram(Builder inst,
      List<Document> matchingSentences, String relation, int n, 
      boolean markDirection) {
    for (Document doc : matchingSentences) {
      for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
        CompoundGroup cg = doc.getCompound(cgi);
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (int ci = 0; ci < cg.getCompoundCount(); ++ci) {
            Compound c = cg.getCompound(ci);
            if (c.getText().equals(relation)) {
              inst = addBetweenNgram(doc, cgi, ci, inst, n, markDirection);
            }
          }
        }
      }
    }
    return inst;
  }
  
  public Builder addBetweenNgram(Builder inst, List<Document> matchingSentences, 
      String relation, int n) {
    return addBetweenNgram(inst, matchingSentences, relation, n, false);
  }

  /**
   * This adds, to an instance, n-grams of the tokens between the arguments for
   * one sentence. The arguments are included in the ngrams, but wildcarded to
   * "ARG1" and "ARG2".
   * 
   * @param doc sentence with relation annotation.
   * @param cgi compound group index, referring to relation annotations group.
   * @param ci compound index referring to specific relation annotation.
   * @param inst feature instance to be modified.
   * @param n size of n-gram.
   * @return
   */
  private Builder addBetweenNgram(Document doc, int cgi, int ci, Builder inst,
      int n, boolean markDirection) {
    List<String> tokenList = new ArrayList<String>();
    
    Compound c = doc.getCompound(cgi).getCompound(ci);
    double sentenceWeight = c.hasWeight() ? c.getWeight() : 1.0;
    int argIdx1Start = c.getSlot(0).getStartToken();
    int argIdx1End = c.getSlot(0).getEndToken();
    int argIdx2Start = c.getSlot(1).getStartToken();
    int argIdx2End = c.getSlot(1).getEndToken();
    int leftEnd;
    int rightStart;
    boolean arg1first = argIdx1Start < argIdx2Start;
    if (arg1first) {
      leftEnd = argIdx1End;
      rightStart = argIdx2Start;
    } else {
      leftEnd = argIdx2End;
      rightStart = argIdx1Start;
    }
    if (n>1) tokenList.add(arg1first ? "ARG1" : "ARG2");
    for (int i = leftEnd; i < rightStart; ++i) {
      String tok = doc.getToken(i).getText();
      tokenList.add(tok);
    }
    if (n>1) tokenList.add(arg1first ? "ARG2" : "ARG1");
    List<String> ngramFeatures = new ArrayList<String>();
    for (int i = 0; i < tokenList.size() + 1 - n; ++i) {
      StringBuffer ngram = new StringBuffer();
      ngram.append(BETWEEN_NGRAM_FEATURE);
      // Marker for directionality
      if (markDirection) {
        ngram.append(arg1first ? ">" : "<");
      }
      for (int j = 0; j < n; ++j) {
        ngram.append("#").append(tokenList.get(i + j));
      }
      ngramFeatures.add(ngram.toString());
    }
    addFeatures(inst, ngramFeatures, sentenceWeight / ngramFeatures.size());
    return inst;
  }
  
  /*
  private Builder addBetweenNgram(Document doc, int cgi, int ci, Builder inst,
      int n) {
    return addBetweenNgram(doc, cgi, ci, inst, n, false);
  }*/
  
  
  /**
   * This adds ngrams outside of the argument spans. Arguments are included but
   * wildcarded.
   * 
   * @param inst
   * @param matchingSentences
   * @param relation
   * @param n
   * @param window
   * @return
   */
  public Builder addOutsideNgram(Builder inst,
      List<Document> matchingSentences, String relation, int n, int window, 
      boolean markDirection) {
    for (Document doc : matchingSentences) {
      for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
        CompoundGroup cg = doc.getCompound(cgi);
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (int ci = 0; ci < cg.getCompoundCount(); ++ci) {
            Compound c = cg.getCompound(ci);
            if (c.getText().equals(relation)) {
              inst = addOutsideNgram(doc, cgi, ci, inst, n, window, markDirection);
            }
          }
        }
      }
    }
    return inst;
  }
  
  public Builder addOutsideNgram(Builder inst,
      List<Document> matchingSentences, String relation, int n, int window) {
    return addOutsideNgram(inst, matchingSentences, relation, n, window, false);
  }

  private Builder addOutsideNgram(Document doc, int cgi, int ci, Builder inst,
      int n, int window, boolean markDirection) {
    List<String> leftTokenList = new ArrayList<String>();
    List<String> rightTokenList = new ArrayList<String>();
    Compound c = doc.getCompound(cgi).getCompound(ci);
    double sentenceWeight = c.hasWeight() ? c.getWeight() : 1.0;
    int argIdx1Start = c.getSlot(0).getStartToken();
    int argIdx1End = c.getSlot(0).getEndToken();
    int argIdx2Start = c.getSlot(1).getStartToken();
    int argIdx2End = c.getSlot(1).getEndToken();
    int leftStart;
    int rightEnd;
    boolean arg1first = argIdx1Start < argIdx2Start;
    if (arg1first) {
      leftStart = argIdx1Start;
    } else {
      leftStart = argIdx2End;
    }
    boolean arg1last = argIdx1End > argIdx2End;
    if (arg1last) {
      rightEnd = argIdx1End;
    } else {
      rightEnd = argIdx2End;
    }
    
    for (int i = Math.max(0, leftStart - window); i < leftStart - 1; ++i) {
      String tok = doc.getToken(i).getText();
      leftTokenList.add(tok);
    }
    
    if (n>1) leftTokenList.add(arg1first ? "ARG1" : "ARG2");
    
    if (n>1) rightTokenList.add(arg1last ? "ARG1" : "ARG2");
    
    for (int i = rightEnd; i < Math.min(doc.getTokenCount(), rightEnd + window); 
        ++i) {
      String tok = doc.getToken(i).getText();
      rightTokenList.add(tok);
    }
    
    List<String> ngramFeatures = new ArrayList<String>();
    
    for (int i = 0; i < leftTokenList.size() + 1 - n; ++i) {
      StringBuffer ngram = new StringBuffer();
      ngram.append(OUTSIDE_NGRAM_FEATURE);
      if (markDirection) {
        ngram.append(arg1first ? ">" : "<");
      }
      for (int j = 0; j < n; ++j) {
        ngram.append("#").append(leftTokenList.get(i + j));
      }
      ngramFeatures.add(ngram.toString());
    }
    
    for (int i = 0; i < rightTokenList.size() + 1 - n; ++i) {
      StringBuffer ngram = new StringBuffer();
      ngram.append(OUTSIDE_NGRAM_FEATURE);
      if (markDirection) {
        ngram.append(arg1first ? ">" : "<");
      }
      for (int j = 0; j < n; ++j) {
        ngram.append("#").append(rightTokenList.get(i + j));
      }
      ngramFeatures.add(ngram.toString());
    }
    
    addFeatures(inst, ngramFeatures, sentenceWeight / ngramFeatures.size());
    return inst;

  }

  /**
   * This adds ngrams of brown classes between the arguments (analogous to 
   * addBetweenNgram).
   * 
   * @param inst
   * @param matchingSentences
   * @param relation
   * @param classMap
   * @param n ngram size
   * @param prefixLength length of brown prefix
   * @return
   */
  public Builder addBetweenBrownNgram(Builder inst,
      List<Document> matchingSentences, String relation,
      Map<String, String> classMap, int n, int prefixLength, 
      boolean markDirection) {
    for (Document doc : matchingSentences) {
      for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
        CompoundGroup cg = doc.getCompound(cgi);
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (int ci = 0; ci < cg.getCompoundCount(); ++ci) {
            Compound c = cg.getCompound(ci);
            if (c.getText().equals(relation)) {
              inst = addBetweenBrownNgram(doc, cgi, ci, inst, classMap, n, 
                  prefixLength, markDirection);
            }
          }
        }
      }
    }
    return inst;
  }
  
  public Builder addBetweenBrownNgram(Builder inst,
      List<Document> matchingSentences, String relation,
      Map<String, String> classMap, int n, int prefixLength) {
    return addBetweenBrownNgram(inst, matchingSentences, relation,
        classMap, n, prefixLength, false);
  }

  private Builder addBetweenBrownNgram(Document doc, int cgi, int ci,
      Builder inst, Map<String, String> classMap, int n, int prefixLength, 
      boolean markDirection) {
    List<String> classList = new ArrayList<String>();
    Compound c = doc.getCompound(cgi).getCompound(ci);
    int argIdx1Start = c.getSlot(0).getStartToken();
    int argIdx1End = c.getSlot(0).getEndToken();
    int argIdx2Start = c.getSlot(1).getStartToken();
    int argIdx2End = c.getSlot(1).getEndToken();
    int leftEnd;
    int rightStart;
    boolean arg1first = argIdx1Start < argIdx2Start;
    if (arg1first) {
      leftEnd = argIdx1End;
      rightStart = argIdx2Start;
    } else {
      leftEnd = argIdx2End;
      rightStart = argIdx1Start;
    }
    if (n>1) classList.add(arg1first ? "ARG1" : "ARG2");
    for (int i = leftEnd; i < rightStart; ++i) {
      String tok = doc.getToken(i).getText();
      String tcl;
      if (classMap.containsKey(tok)) {
        tcl = classMap.get(tok);
        tcl = tcl.substring(0, Math.min(prefixLength, tcl.length()));
      } else {
        tcl = UNKNOWN_TOKEN;
      }
      classList.add(tcl);
    }
    if (n>1) classList.add(arg1first ? "ARG2" : "ARG1");
    List<String> ngramFeatures = new ArrayList<String>();
    for (int i = 0; i < classList.size() + 1 - n; ++i) {
      StringBuffer ngram = new StringBuffer();
      ngram.append(BETWEEN_BROWN_NGRAM_FEATURE).append(Integer.toString(prefixLength));
      if (markDirection) {
        ngram.append(arg1first ? ">" : "<");
      }
      for (int j = 0; j < n; ++j) {
        ngram.append("#").append(classList.get(i + j));
      }
      ngramFeatures.add(ngram.toString());
    }
    double sentenceWeight = c.hasWeight() ? c.getWeight() : 1.0;
    addFeatures(inst, ngramFeatures, sentenceWeight / ngramFeatures.size());
    return inst;
  }
  
  /**
   * This adds Brown ngrams outside of arguments (analogously to 
   * addOutsideNgram).
   * 
   * @param inst
   * @param matchingSentences
   * @param relation
   * @param classMap
   * @param n
   * @param prefixLength
   * @param window
   * @return
   */
  public Builder addOutsideBrownNgram(Builder inst,
      List<Document> matchingSentences, String relation,
      Map<String, String> classMap, int n, int prefixLength, int window, 
      boolean markDirection) {
    for (Document doc : matchingSentences) {
      for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
        CompoundGroup cg = doc.getCompound(cgi);
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (int ci = 0; ci < cg.getCompoundCount(); ++ci) {
            Compound c = cg.getCompound(ci);
            if (c.getText().equals(relation)) {
              inst = addOutsideBrownNgram(doc, cgi, ci, inst, classMap, n, 
                  prefixLength, window, markDirection);
            }
          }
        }
      }
    }
    return inst;
  }
  
  public Builder addOutsideBrownNgram(Builder inst,
      List<Document> matchingSentences, String relation,
      Map<String, String> classMap, int n, int prefixLength, int window) {
    return addOutsideBrownNgram(inst, matchingSentences, relation, classMap, n, 
        prefixLength, window, false);
  }
  
  private Builder addOutsideBrownNgram(Document doc, int cgi, int ci, Builder inst,
      Map<String, String> classMap, int n, int prefixLength, int window, 
      boolean markDirection) {
    List<String> leftBrownList = new ArrayList<String>();
    List<String> rightBrownList = new ArrayList<String>();
    Compound c = doc.getCompound(cgi).getCompound(ci);
    int argIdx1Start = c.getSlot(0).getStartToken();
    int argIdx1End = c.getSlot(0).getEndToken();
    int argIdx2Start = c.getSlot(1).getStartToken();
    int argIdx2End = c.getSlot(1).getEndToken();
    int leftStart;
    int rightEnd;
    boolean arg1first = argIdx1Start < argIdx2Start;
    if (arg1first) {
      leftStart = argIdx1Start;
    } else {
      leftStart = argIdx2End;
    }
    boolean arg1last = argIdx1End > argIdx2End;
    if (arg1last) {
      rightEnd = argIdx1End;
    } else {
      rightEnd = argIdx2End;
    }
    
    for (int i = Math.max(0, leftStart - window); i < leftStart - 1; ++i) {
      String tok = doc.getToken(i).getText();
      String tcl;
      if (classMap.containsKey(tok)) {
        tcl = classMap.get(tok);
        tcl = tcl.substring(0, Math.min(prefixLength, tcl.length()));
      } else {
        tcl = UNKNOWN_TOKEN;
      }
      leftBrownList.add(tcl);
    }
    if (n>1) leftBrownList.add(arg1first ? "ARG1" : "ARG2");
    
    if (n>1) rightBrownList.add(arg1last ? "ARG1" : "ARG2");
    for (int i = rightEnd; i < Math.min(doc.getTokenCount(), rightEnd + window); 
        ++i) {
      String tok = doc.getToken(i).getText();
      String tcl;
      if (classMap.containsKey(tok)) {
        tcl = classMap.get(tok);
        tcl = tcl.substring(0, Math.min(prefixLength, tcl.length()));
      } else {
        tcl = UNKNOWN_TOKEN;
      }
      rightBrownList.add(tcl);
    }
    
    List<String> ngramFeatures = new ArrayList<String>();
    
    for (int i = 0; i < leftBrownList.size() + 1 - n; ++i) {
      StringBuffer ngram = new StringBuffer();
      ngram.append(OUTSIDE_BROWN_NGRAM_FEATURE).append(Integer.toString(prefixLength));
      if (markDirection) {
        ngram.append(arg1first ? ">" : "<");
      }
      for (int j = 0; j < n; ++j) {
        ngram.append("#").append(leftBrownList.get(i + j));
      }
      ngramFeatures.add(ngram.toString());
    }
    
    for (int i = 0; i < rightBrownList.size() + 1 - n; ++i) {
      StringBuffer ngram = new StringBuffer();
      ngram.append(OUTSIDE_BROWN_NGRAM_FEATURE).append(Integer.toString(prefixLength));
      if (markDirection) {
        ngram.append(arg1first ? ">" : "<");
      }
      for (int j = 0; j < n; ++j) {
        ngram.append("#").append(rightBrownList.get(i + j));
      }
      ngramFeatures.add(ngram.toString());
    }
    double sentenceWeight = c.hasWeight() ? c.getWeight() : 1.0;
    addFeatures(inst, ngramFeatures, sentenceWeight / ngramFeatures.size());
    return inst;

  }

  /**
   * This adds brown-classes ngrams for the arguments.
   * @param inst
   * @param matchingSentences
   * @param relation
   * @param classMap
   * @param n
   * @param prefixLength
   * @param argNr
   * @return
   */
  public Builder addArgBrownNgram(Builder inst,
      List<Document> matchingSentences, String relation,
      Map<String, String> classMap, int n, int prefixLength, int argNr) {
    if (matchingSentences.size() == 0) {
      throw new IllegalArgumentException("Expected non-empty sentence list");
    }
    
    Document sentence = matchingSentences.get(0);
    
    List<String> brownList = new ArrayList<String>();
    for (String tok : 
      DocumentExtractor.argumentText(sentence, relation, argNr).split(" ")) {
      String tcl;
      if (classMap.containsKey(tok)) {
        tcl = classMap.get(tok);
        tcl = tcl.substring(0, Math.min(prefixLength, tcl.length()));
      } else {
        tcl = UNKNOWN_TOKEN;
      }
      brownList.add(tcl);
    }
    
    List<String> ngramFeatures = new ArrayList<String>();    
    for (int i = 0; i < brownList.size() + 1 - n; ++i) {
      StringBuffer ngram = new StringBuffer();
      ngram.append(ARG_BROWN_NGRAM_FEATURE).append(argNr).append("#")
      .append(Integer.toString(prefixLength));
      for (int j = 0; j < n; ++j) {
        ngram.append("#").append(brownList.get(i + j));
      }
      ngramFeatures.add(ngram.toString());
    }
    // here only one sentence is looked at, so it doesn't make sense to 
    // use different sentence weight in the current setting.
    double sentenceWeight = 1.0;
    addFeatures(inst, ngramFeatures, sentenceWeight / ngramFeatures.size());
    return inst;
  }
  
  private static final Pattern ALL_CAPS_PATTERN = Pattern.compile("[A-Z]+");
  public Builder addArgCapitalization(Builder inst,
      List<Document> matchingSentences, String relation, int argNr) {
    if (matchingSentences.size() == 0) return inst;
    Document sentence = matchingSentences.get(0);
    String argstr = DocumentExtractor.argumentText(sentence, relation, argNr);
    Matcher m = ALL_CAPS_PATTERN.matcher(argstr);
    if (m.matches()) {
      // no sentence weight used -- feature sentence independent.
      inst = addFeature(inst, ARG_CAPITALIZATION_FEATURE + argNr + "#ALL_CAPS", 1.0);
    }
    return inst;
  }

  public Builder addArgCapitalizationConjunction(Builder inst,
      List<Document> matchingSentences, String relation) {
    if (matchingSentences.size() == 0) return inst;
    Document sentence = matchingSentences.get(0);
    String argstr1 = DocumentExtractor.argumentText(sentence, relation, 0);
    Matcher m1 = ALL_CAPS_PATTERN.matcher(argstr1);
    String f = "";
    if (m1.matches()) {
      f+="C";
    } else {
      f+="l";
    }
    String argstr2 = DocumentExtractor.argumentText(sentence, relation, 0);
    Matcher m2 = ALL_CAPS_PATTERN.matcher(argstr2);
    if (m2.matches()) {
      f+="C";
    } else {
      f+="l";
    }
    // no sentence weight used -- feature sentence independent.
    inst = addFeature(inst, ARG_CAPITALIZATION_FEATURE + f, 1.0);
    return inst;
  }

  public Builder addSlotTokenCount(Builder inst,
      List<Document> matchingSentences, String relation) {
    if (matchingSentences.size() == 0) return inst;
    Document sentence = matchingSentences.get(0);
    String[] argstr = DocumentExtractor.argumentText(sentence, relation, 1).split(" ");
    int cappedLen = Math.min(argstr.length, 5);
    // no sentence weight used -- feature sentence independent.
    inst = addFeature(inst, ARG_TOKEN_COUNT_FEATURE + cappedLen, 1.0);
    return inst;
  }

  public Builder addSlotLogCharacterCount(Builder inst,
      List<Document> matchingSentences, String relation) {
    if (matchingSentences.size() == 0) return inst;
    Document sentence = matchingSentences.get(0);
    String argstr = DocumentExtractor.argumentText(sentence, relation, 1);
    if (argstr.length() > 1) {
      int len = (int) Math.floor(Math.log(argstr.length()) / Math.log(2.0)); 
      // no sentence weight used -- feature sentence independent.
      inst = addFeature(inst, ARG_LOG_CHAR_COUNT_FEATURE + len, 1.0);      
    }

    return inst;
  }
  
  public Builder addSkip(Builder inst,
      List<Document> matchingSentences, String relation, int n, boolean exact) {
    for (Document doc : matchingSentences) {
      for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
        CompoundGroup cg = doc.getCompound(cgi);
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (int ci = 0; ci < cg.getCompoundCount(); ++ci) {
            Compound c = cg.getCompound(ci);
            if (c.getText().equals(relation)) {
              inst = addSkipFuzzy(doc, cgi, ci, inst, n, exact);
            }
          }
        }
      }
    }
    return inst;
  }

  private Builder addSkipFuzzy(Document doc, int cgi, int ci, Builder inst,
      int n, boolean exact) {
    List<String> tokenList = new ArrayList<String>();
    Compound c = doc.getCompound(cgi).getCompound(ci);
    int argIdx1Start = c.getSlot(0).getStartToken();
    int argIdx1End = c.getSlot(0).getEndToken();
    int argIdx2Start = c.getSlot(1).getStartToken();
    int argIdx2End = c.getSlot(1).getEndToken();
    int leftEnd;
    int rightStart;
    boolean arg1first = argIdx1Start < argIdx2Start;
    if (arg1first) {
      leftEnd = argIdx1End;
      rightStart = argIdx2Start;
    } else {
      leftEnd = argIdx2End;
      rightStart = argIdx1Start;
    }
    if (n>1) tokenList.add(arg1first ? "ARG1" : "ARG2");
    for (int i = leftEnd; i < rightStart; ++i) {
      String tok = doc.getToken(i).getText();
      tokenList.add(tok);
    }
    if (n>1) tokenList.add(arg1first ? "ARG2" : "ARG1");
    List<String> ngramFeatures = new ArrayList<String>();
    for (int i = 0; i < tokenList.size() + 1 - n; ++i) {
      StringBuffer ngram = new StringBuffer();
      ngram.append(SKIP_FEATURE);
      ngram.append("#").append(exact ? "EXACT" : "FUZZY");
      ngram.append(arg1first ? ">" : "<");
      for (int j = 0; j < n; ++j) {
        if (j == 0 || j == n-1) {
          ngram.append("#").append(tokenList.get(i + j));
        } else if (exact) {
          // For the exact version, count how many are skipped.
          ngram.append("#");
        }
      }
      ngramFeatures.add(ngram.toString());
    }
    double sentenceWeight = c.hasWeight() ? c.getWeight() : 1.0;
    addFeatures(inst, ngramFeatures, sentenceWeight / ngramFeatures.size());
    return inst;
  }

  // TODO: experimental code
  public static final String TOPIC_METHOD_ID = "TOPIC_METHOD_ID";
  public Builder addTopics(Builder fgrpInst, Document sentence) {
    int methodIdx = -1;
    for (int i = 0; i < sentence.getMethodCount(); ++i) {
      Method m = sentence.getMethod(i);
      if (m.getId().equals(TOPIC_METHOD_ID)) {
        methodIdx = i;
        break;
      }
    }
    if (methodIdx == -1) {
      throw new IllegalArgumentException("no topic annotation");
    }
    
    CompoundGroup cg = null;

    for (CompoundGroup cgCandidate : sentence.getCompoundList()) {
      if (cgCandidate.getMethodIndex() == methodIdx) {
        cg = cgCandidate;
        break;
      }
    }
    if (cg == null) {
      throw new IllegalArgumentException("no topic annotation");  
    }
    for (Compound c : cg.getCompoundList()) {
      if (c.getWeight() > 0) {
        String fStr = TOPIC_FEATURE + c.getVal();
        addFeature(fgrpInst, fStr, c.getWeight());
      }
    }
    return fgrpInst;
  }
  
  public Builder addTopics(Builder fgrpInst, List<Document> matchingSentences) {
    for (Document sentence : matchingSentences) {
      addTopics(fgrpInst, sentence);
    }
    return fgrpInst;
  }

  public Builder addIntertext(Builder inst, List<Document> matchingSentences, String relation) {
    for (Document doc : matchingSentences) {
      for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
        CompoundGroup cg = doc.getCompound(cgi);
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (int ci = 0; ci < cg.getCompoundCount(); ++ci) {
            Compound c = cg.getCompound(ci);
            if (c.getText().equals(relation)) {
              inst = addIntertext(doc, cgi, ci, inst);
            }
          }
        }
      }
    }
    return inst;
  }

  private Builder addIntertext(Document doc, int cgi, int ci, Builder inst) {
    StringBuilder sb = new StringBuilder();
    
    Compound c = doc.getCompound(cgi).getCompound(ci);
    double sentenceWeight = c.hasWeight() ? c.getWeight() : 1.0;
    int argIdx1Start = c.getSlot(0).getStartToken();
    int argIdx1End = c.getSlot(0).getEndToken();
    int argIdx2Start = c.getSlot(1).getStartToken();
    int argIdx2End = c.getSlot(1).getEndToken();
    int leftEnd;
    int rightStart;
    boolean arg1first = argIdx1Start < argIdx2Start;
    if (arg1first) {
      leftEnd = argIdx1End;
      rightStart = argIdx2Start;
    } else {
      leftEnd = argIdx2End;
      rightStart = argIdx1Start;
    }
    sb.append(arg1first ? "ARG1" : "ARG2").append(" ");
    for (int i = leftEnd; i < rightStart; ++i) {
      String tok = doc.getToken(i).getText();
      sb.append(tok).append(" ");
    }
    sb.append(arg1first ? "ARG2" : "ARG1");

    String interText = INTERTEXT_FEATURE + sb.toString(); // TODO
    addFeature(inst, interText, sentenceWeight);
    return inst;
  }


  public Builder addIntertextShort(Builder inst, List<Document> matchingSentences, String relation) {
    for (Document doc : matchingSentences) {
      for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
        CompoundGroup cg = doc.getCompound(cgi);
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (int ci = 0; ci < cg.getCompoundCount(); ++ci) {
            Compound c = cg.getCompound(ci);
            if (c.getText().equals(relation)) {
              inst = addIntertextShort(doc, cgi, ci, inst, 2);
            }
          }
        }
      }
    }
    return inst;
  }

  private Builder addIntertextShort(Document doc, int cgi, int ci, Builder inst, int maxTokensLeftRight) {
    StringBuilder sb = new StringBuilder();

    Compound c = doc.getCompound(cgi).getCompound(ci);
    double sentenceWeight = c.hasWeight() ? c.getWeight() : 1.0;
    int argIdx1Start = c.getSlot(0).getStartToken();
    int argIdx1End = c.getSlot(0).getEndToken();
    int argIdx2Start = c.getSlot(1).getStartToken();
    int argIdx2End = c.getSlot(1).getEndToken();
    int leftEnd;
    int rightStart;
    boolean arg1first = argIdx1Start < argIdx2Start;
    if (arg1first) {
      leftEnd = argIdx1End;
      rightStart = argIdx2Start;
    } else {
      leftEnd = argIdx2End;
      rightStart = argIdx1Start;
    }

    String logbin = "";
    Boolean writeLogbin = false;
    if  (rightStart - leftEnd > 2 * maxTokensLeftRight) {
      logbin = "[" + (int) (Math.log(rightStart - leftEnd - 2 * maxTokensLeftRight) / Math.log(2)) + "]";
      writeLogbin = true;
    }

    sb.append(arg1first ? "ARG1" : "ARG2").append(" ");
    for (int i = leftEnd; i < rightStart; ++i) {

      if (i >= leftEnd + maxTokensLeftRight && i < rightStart - maxTokensLeftRight) {
        if (writeLogbin) {
          sb.append(logbin).append(" ");
          writeLogbin = false;
        }
      } else {
        String tok = doc.getToken(i).getText();
        sb.append(tok).append(" ");
      }
    }
    sb.append(arg1first ? "ARG2" : "ARG1");

    String interText = INTERTEXT_SHORT_FEATURE + sb.toString(); // TODO

    // TODO: debugging
    /*
    System.out.println("===");
    System.out.println(DocumentExtractor.textFromTokens(doc));
    System.out.println(DocumentExtractor.textFromTokens(doc, leftEnd, rightStart));
    System.out.println(interText);
*/
    addFeature(inst, interText, sentenceWeight);
    return inst;
  }
  
  private Builder addMintzNoArgsLexical(Document doc, int cgi, int ci, 
      Builder inst, int windowK) {
    StringBuilder sb = new StringBuilder();
    
    Compound c = doc.getCompound(cgi).getCompound(ci);
    List<String> posTagList = 
        DocumentExtractor.annotationFromTokens(doc, DocumentTagger.POS);
    if (posTagList.size() != doc.getTokenCount()) {
      throw new IllegalArgumentException(
          "Sentence does not have correct pos-tag annotation." +
          "Number of tokens: " + doc.getTokenCount() + " Number of pos tags: " + posTagList.size());
    }
        //getTokenAnnotation(doc, DocumentTagger.POS);
    
    double sentenceWeight = c.hasWeight() ? c.getWeight() : 1.0;
    int argIdx1Start = c.getSlot(0).getStartToken();
    int argIdx1End = c.getSlot(0).getEndToken();
    int argIdx2Start = c.getSlot(1).getStartToken();
    int argIdx2End = c.getSlot(1).getEndToken();
    int leftArgStart;
    int leftArgEnd;
    int rightArgStart;
    int rightArgEnd;
    boolean arg1first = argIdx1Start < argIdx2Start;
    if (arg1first) {
      leftArgStart = argIdx1Start;
      leftArgEnd = argIdx1End;
      rightArgStart = argIdx2Start;
      rightArgEnd = argIdx2End;
    } else {
      leftArgStart = argIdx2Start;
      leftArgEnd = argIdx2End;
      rightArgStart = argIdx1Start;
      rightArgEnd = argIdx1End;
    }
    
    int leftWindowStart = Math.max(0, leftArgStart - windowK);
    int rightWindowEnd = Math.min(rightArgEnd + windowK, doc.getTokenCount());

    for (int i = leftWindowStart; i < leftArgStart; ++i) {
      String tok = doc.getToken(i).getText();
      sb.append(" ").append(tok).append("/").append(posTagList.get(i));
    }
    sb.append(" ").append(arg1first ? "ARG1" : "ARG2");
    for (int i = leftArgEnd; i < rightArgStart; ++i) {
      String tok = doc.getToken(i).getText();
      sb.append(" ").append(tok).append("/").append(posTagList.get(i));
    }
    sb.append(" ").append(arg1first ? "ARG2" : "ARG1");
    for (int i = rightArgEnd; i < rightWindowEnd; ++i) {
      String tok = doc.getToken(i).getText();
      sb.append(" ").append(tok).append("/").append(posTagList.get(i));
    }
    
    String interText = MINTZ_LEXICAL_FEATURE + sb.toString(); // TODO
    addFeature(inst, interText, sentenceWeight);
    return inst;
  }

  public Builder addMintzNoArgsLexical(Builder inst,
      List<Document> matchingSentences, String relation, int windowK) {
    for (Document doc : matchingSentences) {
      for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
        CompoundGroup cg = doc.getCompound(cgi);
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (int ci = 0; ci < cg.getCompoundCount(); ++ci) {
            Compound c = cg.getCompound(ci);
            if (c.getText().equals(relation)) {
              inst = addMintzNoArgsLexical(doc, cgi, ci, inst, windowK);
            }
          }
        }
      }
    }
    return inst;
  }

  public Builder addMintzNoArgsSyntactic(Builder inst,
      List<Document> matchingSentences, String relation) {
    for (Document doc : matchingSentences) {
      for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
        CompoundGroup cg = doc.getCompound(cgi);
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (int ci = 0; ci < cg.getCompoundCount(); ++ci) {
            Compound c = cg.getCompound(ci);
            if (c.getText().equals(relation)) {
              inst = addMintzNoArgsSyntactic(doc, cgi, ci, inst);
            }
          }
        }
      }
    }
    return inst;
  }

  private Builder addMintzNoArgsSyntactic(Document doc, int cgi, int ci,
      Builder inst) {
    if (!DependencyTree.hasDependencyAnnotation(doc)) {
      throw new IllegalArgumentException("Unparsed sentence: " + doc);
    }
    Compound c = doc.getCompound(cgi).getCompound(ci);
    double sentenceWeight = c.hasWeight() ? c.getWeight() : 1.0;
    int argIdx1Start = c.getSlot(0).getStartToken();
    int argIdx1End = c.getSlot(0).getEndToken();
    int argIdx2Start = c.getSlot(1).getStartToken();
    int argIdx2End = c.getSlot(1).getEndToken();
    
    // Reversal not necessary, since we look at relational arg1 arg2, not 
    // positional ones.
    //boolean reveresed = argIdx1Start > argIdx2Start;
    
    DependencyTree tree = new DependencyTree(doc);
    int arg1Head = tree.getHead(argIdx1Start, argIdx1End);
    int arg2Head = tree.getHead(argIdx2Start, argIdx2End);
    String middlePath = tree.pathStringLexicalized(arg1Head, arg2Head, doc);
    String arg1window = tree.MintzWindowFeature(arg1Head, arg2Head, arg1Head, doc);
    String arg2window = tree.MintzWindowFeature(arg1Head, arg2Head, arg2Head, doc);
    // combination of features:
    // yes yes
    if (arg1window != null && arg2window != null) {
      
      String feature = MINTZ_SYNTACTIC_FEATURE + arg1window + "#" + middlePath + "#" + arg2window;
      addFeature(inst, feature, sentenceWeight);
    }
    // yes no
    if (arg1window != null) {
      String feature = MINTZ_SYNTACTIC_FEATURE + arg1window + "#" + middlePath + "#";
      addFeature(inst, feature, sentenceWeight);
    }
    // no yes
    if (arg2window != null) {
      String feature = MINTZ_SYNTACTIC_FEATURE + "#" + middlePath + "#" + arg2window;
      addFeature(inst, feature, sentenceWeight);
    }
    // no no
    String feature = MINTZ_SYNTACTIC_FEATURE + "#" + middlePath + "#";
    addFeature(inst, feature, sentenceWeight);
    return inst;
  }
}
