package util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.tartarus.snowball.SnowballStemmer;

import features.BreakLevel;

import rerac.protos.Corpus.Document;
import rerac.protos.Corpus.Document.Annotation;
import rerac.protos.Corpus.Document.AnnotationType;
import rerac.protos.Corpus.Document.Compound;
import rerac.protos.Corpus.Document.Compound.CompoundSlot;
import rerac.protos.Corpus.Document.CompoundGroup;
import rerac.protos.Corpus.Document.Method;
import rerac.protos.Corpus.Document.Token;

/**
 * TODO: Rename and move to util.
 * 
 * This extracts information from a document. Currently it is used to extract 
 * sub-document and to get a single String from the token texts.
 * 
 * @author Benjamin Roth
 *
 */
public class DocumentExtractor {
  static final int DEFAULT_TARGET = -1;
  static final String RELATION = "RELATION";
  
  public static final String BREAK = "BREAK";
  public static final String STEM = "STEM";
  public static final String SS = "SS";
  public static final String NE = "NE";
  /** annotation-type of an annotation that assigns an id to arguments. These ids map 1:N to arguments and 1:1 to entity-equivalence classes such as {"barack obama", "obama", "XXth president of the USA"}. */
  public static final String ARG_ID = "ARG_ID";
  /**
   * This extracts a subdocument containing the tokens from start index 
   * (inclusive) to end index (exclusive) with all Annotations.
   * The following values are changed so that they refer to indices in the new 
   * document:
   * Annotation.target_token
   * Annotation.target_compound
   * Token.start
   * Token.end
   * Compound annotations are copied over partially if one of the arguments is 
   * in the new document.
   * The document id is set to 'ORIGINAL_ID:start:end'.
   * 
   * @param doc the document
   * @param start start index of token, inclusive.
   * @param end end index of token, exclusive.
   */
  public static Document extractSubDocument(Document doc, int start, int end,
      boolean useDefaultTarget) {
    if (start < 0 || end > doc.getTokenCount()) {
      throw new IllegalArgumentException("Start or end index out of range.");
    }
    Document.Builder docBuilder = Document.newBuilder();
    docBuilder.addAllMethod(doc.getMethodList());
    docBuilder.setConsistentMethod(doc.getConsistentMethod());
    docBuilder.setId(doc.getId() + ":" + start + ":" + end);
    if (doc.getTokenCount() == 0) {
      return docBuilder.build();
    }
    
    if (doc.hasText()) {
      Token firstTok = doc.getToken(0);
      Token lastTok = doc.getToken(doc.getTokenCount() - 1);
      docBuilder.setText(doc.getText().substring(firstTok.getStart(), 
          lastTok.getEnd()));
    }
    
    for (int i = start; i < end; ++i) {
      Token.Builder tb = Token.newBuilder(doc.getToken(i));
      for (int ai = 0; ai < tb.getAnnotationCount(); ++ai) {
        Annotation a = tb.getAnnotation(ai);
        if (a.hasTargetToken()) {
          int newTarget = a.getTargetToken() - start;
          if (newTarget >= 0 && newTarget < (end - start)) {
            tb.setAnnotation(ai, 
                Annotation.newBuilder(a).setTargetToken(newTarget));
          } else if (useDefaultTarget) {
            tb.setAnnotation(ai, 
                Annotation.newBuilder(a).setTargetToken(DEFAULT_TARGET));            
          }
        }
      }
      docBuilder.addToken(tb);
    }
    
    for (int cgi = 0; cgi < doc.getCompoundCount(); ++cgi) {
      CompoundGroup cg = doc.getCompound(cgi);
      CompoundGroup.Builder cgb = CompoundGroup.newBuilder(cg);
      cgb.clearCompound();
      for (int origCompInd = 0; origCompInd < cg.getCompoundCount(); 
            ++origCompInd) {
        Compound c = cg.getCompound(origCompInd);
        Compound.Builder cb = Compound.newBuilder(c);
        cb.clearSlot();
        for (CompoundSlot cs : c.getSlotList()) {
          if (cs.getStartToken() >= start && cs.getEndToken() <= end) {
            int newStartToken = cs.getStartToken() - start;
            int newEndToken = cs.getEndToken() - start;
            
            // Make slot point to position in new document.
            cb.addSlot(
                CompoundSlot.newBuilder(cs).setStartToken(newStartToken).
                setEndToken(newEndToken));
            for (int ti = cs.getStartToken(); ti < cs.getEndToken(); ++ti) {
              // Update index to CompoundGroup.compound in token.Annotation.
              Token t = doc.getToken(ti);
              Token.Builder tb = Token.newBuilder(docBuilder.getToken(ti - start));
              for (int ai = 0; ai < t.getAnnotationCount(); ++ai) {
                Annotation anno = t.getAnnotation(ai);
                if (anno.hasTargetCompound() &&
                    anno.getCompoundGroup() == cgi && 
                    anno.getTargetCompound() == origCompInd) {
                  tb.setAnnotation(ai, 
                      Annotation.newBuilder(anno).setTargetCompound(
                          cgb.getCompoundCount()));
                }
              }
              docBuilder.setToken(ti - start, tb);
            }
          }
        }
        if (cb.getSlotCount() > 0) {
          cgb.addCompound(cb);
        }
      }
      docBuilder.addCompound(cgb);
    }
    return docBuilder.build();
  }
  
  public static List<Document> sentences(Document doc) {
    int sentenceStart = 0;
    List<Document> retList = new ArrayList<Document>();
    // Start with second token, since sentences breaks are indicated wrt the 
    // previous token.
    for (int currentInd = 1; currentInd < doc.getTokenCount(); ++currentInd) {
      Token tok = doc.getToken(currentInd);
      for (Annotation a : tok.getAnnotationList()) {
        if (a.hasType() && 
            a.getType() == AnnotationType.BOUNDARY &&
            a.getVal() >= BreakLevel.SENTENCE) {
          retList.add(extractSubDocument(doc, sentenceStart, currentInd, true));
          sentenceStart = currentInd;
          break;
        } 
      }
    }
    retList.add(extractSubDocument(doc, sentenceStart, doc.getTokenCount(), true));
    return retList;
  }
  
  /**
   * This returns the concatenation of all token texts.
   * 
   * @param doc the document
   * @param separator the separator inserted between succeeding tokens.
   * @return
   */
  public static String textFromTokens(Document doc, String separator) {
    // TODO: make separation dependent on BreakLevel.
    StringBuffer sb = new StringBuffer();
    for (Token tok : doc.getTokenList()) {
      if (sb.length() > 0) {
        sb.append(separator);
      }
      sb.append(tok.getText());
    }
    return sb.toString();
  }
  
  
  public static String textFromTokens(Document doc) {
    return textFromTokens(doc, 0, doc.getTokenCount());
  }
  
  public static String textFromTokensNoArgs(Document doc) {
    List<String> tokens = new ArrayList<String>();
    for (Token t : doc.getTokenList()) {
      tokens.add(t.getText());
    }
    for (CompoundGroup cg : doc.getCompoundList()) {
      if (cg.getType() == AnnotationType.PROPERTY) {
        if (cg.getCompoundCount() > 0) {
          for (Compound c : cg.getCompoundList()) {
            for (CompoundSlot cs : c.getSlotList()) {
              for (int i = cs.getStartToken(); i < cs.getEndToken(); ++i) {
                tokens.set(i, null);
              }
            }
          }
        }
      }
    }
    StringBuffer sb = new StringBuffer();
    String sep = "";
    for (String t : tokens) {
      if (null != t) {
        sb.append(sep);
        sb.append(t);
        sep = " ";
      }
    }
    return sb.toString();
  }
  
  public static String textFromTokens(Document doc, int start, int end) {
    String[] separators = new String[]{"", " ", " ", "\n"};
    StringBuffer sb = new StringBuffer();
    for (int i = start; i < end; ++i) {
      Token tok = doc.getToken(i);
      int bl = BreakLevel.SPACE;
      for (Annotation a : tok.getAnnotationList()) {
        if (a.getType() == AnnotationType.BOUNDARY && bl >=0 && 
            bl < separators.length) {
          bl = a.getVal();
          break;
        }
      }
      if (sb.length() > 0) {
        sb.append(separators[bl]);
      }
      sb.append(tok.getText());
    }
    return sb.toString();
  }

  /**
   * This returns the concatenation of a specific token annotation.
   * 
   * @param doc the document
   * @param methodName the annotation
   * @param separator the separator to separate the annotations
   * @return the concatenation
   */
  public static String annotationFromTokens(Document doc, String methodName, 
      String separator, int start, int end) {
    StringBuilder sb = new StringBuilder();
    boolean isFirst = true;
    for (String anno : annotationFromTokens(doc, methodName, start, end)) {
      if (isFirst) {
        isFirst = false;
      } else {
        sb.append(separator);
      }
      sb.append(anno);
    }
    return sb.toString();
  }
  
  public static List<String> annotationFromTokens(Document doc, 
      String methodName, int start, int end) {
    List<String> retList = new ArrayList<String>(doc.getTokenCount());
    int methodInd = -1;
    for (int mi = 0; mi < doc.getMethodCount() && methodInd == -1; ++mi) {
      Method m = doc.getMethod(mi);
      if (m.hasId() && m.getId().equals(methodName)) {
        methodInd = mi;
      }
    }
    for (int i = start; i < end; ++i) {
      Token tok = doc.getToken(i);
      for (Annotation a : tok.getAnnotationList()) {
        if (a.getMethodIndex() == methodInd && a.hasText()) {
          retList.add(a.getText());
          break;
        }
      }      
    }
    return retList;
  }
  
  public static String annotationFromTokens(Document doc, String methodName, 
      String separator) {
    return annotationFromTokens(doc, methodName, separator, 0, 
        doc.getTokenCount());
  }
  
  public static List<String> annotationFromTokens(Document doc, String methodName) {
    return annotationFromTokens(doc, methodName, 0, doc.getTokenCount());
  }
  
  /**
   * 
   * This returns the counts of a particular annotation field of an argument,
   * concatenated together (white-space separated).
   * 
   * @param methodName the name of the annotation
   * @param relation the name of the relation to consider
   * @param argNr the argument nr for which the statistics is gatheres
   * @param docs the document to take as a basis
   * @return a map from tags to counts of occurrences in the docs.
   */
  public static Map<String, Integer> argumentAnnotationStatistics(
      String methodName, String relation, int argNr, List<Document> docs) {
    Map<String, Integer> tags2count = new HashMap<String, Integer>();
    for (Document doc : docs) {
      int methodInd = -1;
      for (int mi = 0; mi < doc.getMethodCount() && methodInd == -1; ++mi) {
        Method m = doc.getMethod(mi);
        if (m.hasId() && m.getId().equals(methodName)) {
          methodInd = mi;
        }
      }
      if (methodInd == -1) {
        continue;
      }
      for (CompoundGroup cg : doc.getCompoundList()) {
        if (cg.getType() == AnnotationType.PROPERTY) {
          for (Compound c : cg.getCompoundList()) {
            if (c.getText().equals(relation)) {
              StringBuffer sb = new StringBuffer();
              for (int ti = c.getSlot(argNr).getStartToken(); 
                  ti < c.getSlot(argNr).getEndToken(); ++ti) {
                for (Annotation a : doc.getToken(ti).getAnnotationList()) {
                  if (a.getMethodIndex() == methodInd) {
                    if (sb.length() > 0) { sb.append(" "); }
                    sb.append(a.getText());
                  }
                }
              }
              String tags = sb.toString();
              int count = tags2count.containsKey(tags) ? 
                  tags2count.get(tags) : 0;
              tags2count.put(tags, count + 1);
            }
          }
        }
      }
    }
    return tags2count;
  }
  
  public static String topNeTagSequence(String relation, int argNr, 
      List<Document> docs) {
    Map<String,Integer> tags2count = 
        argumentAnnotationStatistics(NE, relation, argNr, docs);
    String maxTags = null;
    int maxCount = 0;
    for (Entry<String, Integer> e : tags2count.entrySet()) {
      if (e.getValue() >= maxCount) {
        maxCount = e.getValue();
        maxTags = e.getKey();
      }
    }
    return maxTags;
  }
  
  public static Document annotateFromTo(Document doc, int fromInd, int toInd, String methodName, String annotationText) {
    int methodInd = -1;
    AnnotationType type = AnnotationType.OTHER;
    for (int mi = 0; mi < doc.getMethodCount() && methodInd == -1; ++mi) {
      Method m = doc.getMethod(mi);
      if (m.hasId() && m.getId().equals(methodName)) {
        methodInd = mi;
        type = m.getType();
      }
    }
    Document.Builder docBuilder = Document.newBuilder(doc);
    for (int i = fromInd; i < toInd; ++i) {
      Token.Builder tb = docBuilder.getTokenBuilder(i);
      Annotation.Builder ab = Annotation.newBuilder();
      ab.setMethodIndex(methodInd);
      ab.setText(annotationText);
      ab.setType(type);
      tb.addAnnotation(ab);
    }
    return docBuilder.build();
  }
  
  public static Document annotateArg(Document doc, String relation, 
      int argNr, String methodName, String annotationText) {
    int methodInd = -1;
    for (int mi = 0; mi < doc.getMethodCount() && methodInd == -1; ++mi) {
      Method m = doc.getMethod(mi);
      if (m.hasId() && m.getId().equals(methodName)) {
        methodInd = mi;
      }
    }
    for (CompoundGroup cg : doc.getCompoundList()) {
      if (cg.getType() == AnnotationType.PROPERTY) {
        for (Compound c : cg.getCompoundList()) {
          if (c.getText().equals(relation)) {
            int start = c.getSlot(argNr).getStartToken();
            int end = c.getSlot(argNr).getEndToken();
            return annotateFromTo(doc, start, end, methodName, annotationText);
          }
        }
      }
    }
    return null;
  }
  
  public static String getArgAnnotation(Document doc, String relation, 
      int argNr, String methodName) {
    int methodInd = -1;
    for (int mi = 0; mi < doc.getMethodCount() && methodInd == -1; ++mi) {
      Method m = doc.getMethod(mi);
      if (m.hasId() && m.getId().equals(methodName)) {
        methodInd = mi;
      }
    }
    for (CompoundGroup cg : doc.getCompoundList()) {
      if (cg.getType() == AnnotationType.PROPERTY) {
        for (Compound c : cg.getCompoundList()) {
          if (c.getText().equals(relation)) {
            int start = c.getSlot(argNr).getStartToken();
            Token tok = doc.getToken(start);
            for (Annotation anno : tok.getAnnotationList()) {
              if (anno.getMethodIndex() == methodInd) {
                return anno.getText();
              }
            }
          }
        }
      }
    }
    // TODO: return argument String for the case that there is no arg annotation. 
    return null;
  }
  
  /**
   * @return the ARG_ID annotation of the argument or - if no such annotation is present, the surfacetext representation of the argument
   */
  public static String canonicalArg(Document sentence, int argNr,
      String relation) {
    int methodIndex = -1;
    for (int i = 0; i < sentence.getMethodCount() && methodIndex == -1; ++i) {
      if (sentence.getMethod(i).getId().equals(ARG_ID)) {
        methodIndex = i;
      }
    }
    for (CompoundGroup cg : sentence.getCompoundList()) {
      if (cg.getType() == AnnotationType.PROPERTY) {
        if (cg.getCompoundCount() > 0) {
          for (Compound c : cg.getCompoundList()) {
            if (c.getText().equals(relation)) {
              int argStart = c.getSlot(argNr).getStartToken();
              Token tok = sentence.getToken(argStart);
              for (Annotation anno : tok.getAnnotationList()) {
                if (anno.getMethodIndex() == methodIndex) {
                  return anno.getText();
                }
              }
              int argEnd = c.getSlot(argNr).getEndToken();
              String argStr = DocumentExtractor.textFromTokens(
                  DocumentExtractor.extractSubDocument(sentence, argStart,
                      argEnd, true), " ");
              return relation + "." + argStr;
            }
          }
        }
      }
    }
    return null;
  }
  
  public static String argumentText(Document sentence,
      String relation, int argNr) {
    for (CompoundGroup cg : sentence.getCompoundList()) {
      if (cg.getType() == AnnotationType.PROPERTY) {
        if (cg.getCompoundCount() > 0) {
          for (Compound c : cg.getCompoundList()) {
            if (c.getText().equals(relation)) {
              int argStart = c.getSlot(argNr).getStartToken();
              int argEnd = c.getSlot(argNr).getEndToken();
              return DocumentExtractor.textFromTokens(sentence, argStart, argEnd);
            }
          }
        }
      }
    }
    return null;
  }

  public static int firstMatchingNePattern(Document doc, String nePattern, int size) {
    int methodInd = -1;
    for (int mi = 0; mi < doc.getMethodCount() && methodInd == -1; ++mi) {
      Method m = doc.getMethod(mi);
      if (m.hasId() && m.getId().equals(NE)) {
        methodInd = mi;
      }
    }
    StringBuffer sb = new StringBuffer();
    for (int ti = 0; ti < doc.getTokenCount(); ++ti) {
      for (Annotation a : doc.getToken(ti).getAnnotationList()) {
        if (a.getMethodIndex() == methodInd) {
          if (sb.length() > 0) { sb.append(" "); }
          sb.append(a.getText());
          if (ti + 1 >= size && sb.toString().endsWith(nePattern)) {
            return ti + 1 - size;
          }
        }
      }
    }
    return -1;
  }
  
  public static Document addRel(Document doc, String relation, int arg1Start, 
      int arg1End, int arg2Start, int arg2End, double weight) {
    Document.Builder relDoc = Document.newBuilder(doc);
    int methodIdx = doc.getMethodCount();
    Method m = Method.newBuilder().setId(RELATION).setType(AnnotationType.PROPERTY).build();
    relDoc.addMethod(m);
    
    int compoundGroupIdx = doc.getCompoundCount();
    CompoundGroup.Builder relationCG = CompoundGroup.newBuilder();
    relationCG.setMethodIndex(methodIdx);
    relationCG.setType(AnnotationType.PROPERTY);

    Compound.Builder relationCompound = Compound.newBuilder();
    relationCompound.addSlot(
        CompoundSlot.newBuilder().setStartToken(arg1Start).
        setEndToken(arg1End));
    relationCompound.addSlot(
        CompoundSlot.newBuilder().setStartToken(arg2Start).
        setEndToken(arg2End));
    relationCompound.setText(relation);
    // TODO: include weight?
    relationCompound.setWeight(weight);
    int compoundIdx = relationCG.getCompoundCount();
    relationCG.addCompound(relationCompound);
    
    for (int tokIdx = arg1Start; tokIdx < arg1End; ++tokIdx) {
      Token.Builder tokB = Token.newBuilder(relDoc.getToken(tokIdx));
      tokB.addAnnotation(Annotation.newBuilder()
          .setMethodIndex(methodIdx)
          .setType(AnnotationType.PROPERTY)
          .setCompoundGroup(compoundGroupIdx)
          .setTargetCompound(compoundIdx)
          .build());
      relDoc.setToken(tokIdx, tokB);
    }
    
    for (int tokIdx = arg2Start; tokIdx < arg2End; ++tokIdx) {
      Token.Builder tokB = Token.newBuilder(relDoc.getToken(tokIdx));
      tokB.addAnnotation(Annotation.newBuilder()
          .setMethodIndex(methodIdx)
          .setType(AnnotationType.PROPERTY)
          .setCompoundGroup(compoundGroupIdx)
          .setTargetCompound(compoundIdx)
          .build());
      relDoc.setToken(tokIdx, tokB);
    }

    relDoc.addCompound(relationCG);
    return relDoc.build();
  }
  
  public static Document stem(Document doc, SnowballStemmer stemmer) {
    Document.Builder stemmedDoc = Document.newBuilder(doc);
    Method stemMethod = Method.newBuilder().setId(STEM).
        setType(AnnotationType.MORPHOLOGY).build();
    int stemInd = stemmedDoc.getMethodCount();
    stemmedDoc.addMethod(stemMethod);
    for (Token.Builder tb : stemmedDoc.getTokenBuilderList()) {
      String word = tb.getText();
      stemmer.setCurrent(word.toLowerCase().replaceAll("[^\\p{L}0-9]", ""));
      stemmer.stem();
      String stem = stemmer.getCurrent();
      tb.addAnnotation(Annotation.newBuilder().setText(stem).
          setMethodIndex(stemInd).setType(stemMethod.getType()));
    }
    return stemmedDoc.build();
  }
  
  public static Document makeSentenceDoc(String[] tokens, String id) {
    Document.Builder sentence = Document.newBuilder();
    Method breakMethod = Method.newBuilder().setId(BREAK).
        setType(AnnotationType.BOUNDARY).build();
    int breakInd = 0;
    
    sentence.setId(id);
    sentence.addMethod(breakMethod);
    int breakLevel = BreakLevel.PARAGRAPH;
    for (String token : tokens) {
      Token.Builder t = Token.newBuilder().setText(token);
      t.addAnnotation(Annotation.newBuilder().setVal(breakLevel).
          setMethodIndex(breakInd).setType(breakMethod.getType()));
      sentence.addToken(t);
      breakLevel = BreakLevel.SPACE;
    }
    return sentence.build();
  }

  public static List<String> relations(Document sentence) {
    List<String> retList = new ArrayList<String>();
    for (CompoundGroup cg : sentence.getCompoundList()) {
      if (cg.getType() == AnnotationType.PROPERTY) {
        if (cg.getCompoundCount() > 0) {
          for (Compound c : cg.getCompoundList()) {
            retList.add(c.getText());
          }
        }
      }
    }
    return retList;
  }

  public static boolean hasPositiveRelation(Document sentence, String relation) {
    for (CompoundGroup cg : sentence.getCompoundList()) {
      if (cg.getType() == AnnotationType.PROPERTY) {
        if (cg.getCompoundCount() > 0) {
          for (Compound c : cg.getCompoundList()) {
            if(c.getText().equals(relation) && 
                (!c.hasWeight() || c.getWeight() >= 0.0)) {
              return true;
            }
          }
        }
      }
    }    
    return false;
  }
  
  
  public static double getWeight(Document sentence, String relation) {
    for (CompoundGroup cg : sentence.getCompoundList()) {
      if (cg.getType() == AnnotationType.PROPERTY) {
        if (cg.getCompoundCount() > 0) {
          for (Compound c : cg.getCompoundList()) {
            if(c.getText().equals(relation) && c.hasWeight()) {
              return c.getWeight();
            }
          }
        }
      }
    }
    return 0;
  }
  
  public static boolean hasRelation(Document sentence, String relation) {
    for (CompoundGroup cg : sentence.getCompoundList()) {
      if (cg.getType() == AnnotationType.PROPERTY) {
        if (cg.getCompoundCount() > 0) {
          for (Compound c : cg.getCompoundList()) {
            if(c.getText().equals(relation)) {
              return true;
            }
          }
        }
      }
    }    
    return false;
  }

  public static int getArgStart(Document sentence, String rel, int i) {
    for (CompoundGroup cg : sentence.getCompoundList()) {
      if (cg.getType() == AnnotationType.PROPERTY) {
        if (cg.getCompoundCount() > 0) {
          for (Compound c : cg.getCompoundList()) {
            if(c.getText().equals(rel)) {
              return c.getSlot(i).getStartToken();
            }
          }
        }
      }
    }    
    return -1;
  }
  
  public static int getArgEnd(Document sentence, String rel, int i) {
    for (CompoundGroup cg : sentence.getCompoundList()) {
      if (cg.getType() == AnnotationType.PROPERTY) {
        if (cg.getCompoundCount() > 0) {
          for (Compound c : cg.getCompoundList()) {
            if(c.getText().equals(rel)) {
              return c.getSlot(i).getEndToken();
            }
          }
        }
      }
    }    
    return -1;
  }
  
}
