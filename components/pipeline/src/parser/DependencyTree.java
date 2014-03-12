package parser;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;

import rerac.protos.Corpus.Document;
import rerac.protos.Corpus.Document.Annotation;
import rerac.protos.Corpus.Document.AnnotationType;
import rerac.protos.Corpus.Document.Method;
import rerac.protos.Corpus.Document.Token;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;

/**
 * This holds a dependency tree, typically constructed from a parsed document.
 * At the moment used to get a string feature for pairs of tokens from their
 * shortest path in the tree.
 * 
 * Copied from RERAC (tbarth), added pattern string output method.
 * 
 * @author Benjamin Roth
 *
 */
public class DependencyTree {
  public static final String METHOD_NAME = "DependencyParser";
  /**
   * An edge pointing to another node in the tree.
   * 
   * @author Benjamin Roth
   *
   */
  class Edge {
    // The node index pointed to.
    private int targetIndex;
    // The edge label.
    private String label;
   
    /**
     * 
     * @param l a String, the Edge label.
     * @param target the int index of the node pointed to.
     */
    Edge(String l, int target) {
      label = l;
      targetIndex = target;
    }
    
    /**
     * 
     * @return an int, the index og the node pointed to.
     */
    int getTarget() {
      return targetIndex;
    }
    /**
     * 
     * @return a String, the label of the edge.
     */
    String getLabel() {
      return label;
    }
    
    @Override
    public String toString() {
      return "Edge [targetIndex=" + targetIndex + ", label=" + label + "]";
    }
  }

  // This provides a mapping from any node index that has outgoing edges to the 
  // edges going out from that node.
  private final Multimap<Integer, Edge> index2edges = HashMultimap.create();
  // The number of tokens = potential nodes.
  private int numTokens;
  
  /**
   * This builds up a parse tree from a parsed document.
   * 
   * @param doc a Document in protobuf format.
   */
  public DependencyTree(Document doc) {
    for (int i = 0; i < doc.getTokenCount(); ++i) {
      Token t = doc.getToken(i);
      for (Annotation a : t.getAnnotationList()) {
        if (a.getType() == AnnotationType.SYNTAX_HEAD && 
            a.hasTargetToken()) {
          String label = a.hasText() ? a.getText() : "";
          Edge e = new Edge(label, a.getTargetToken());
          index2edges.put(i, e);
        }
      }
    }
    numTokens = doc.getTokenCount();
  }
  
  DependencyTree() {
  }
  
  /**
   * Adds dependency annotations to given document. Essentially the inverse
   * operation of the constructor above. 
   * @param doc Document to annotate
   * @return Annotated document
   */
  public Document annotate(Document doc) {
    Document.Builder docParse = Document.newBuilder(doc);

    int parserIdx = docParse.getMethodCount();
    // We're loosing info here.
    String parserName = METHOD_NAME;//StanfordParser.class.getName();
    AnnotationType parserType = AnnotationType.SYNTAX_HEAD;
    
    Method.Builder m = 
      Method.newBuilder().setId(parserName).setType(parserType);
    // In general, the parser can add 0, 1 or more annotations to a token. 
    docParse.setConsistentMethod(false);
    docParse.addMethod(m);
    
    for(Entry<Integer, Edge> entry : index2edges.entries()) {
      int depIdx = entry.getKey();
      int targetIdx = entry.getValue().getTarget();
      if (depIdx < 0 || depIdx >= docParse.getTokenCount() || 
          targetIdx >= docParse.getTokenCount()) {
        throw new IllegalStateException("Illegal dep and/or targetIdx: " + 
          depIdx + ", " + targetIdx + ". For document: " + docParse.getId() + " " 
            + docParse.getTokenList() + " " + index2edges);
      }
      Token.Builder tok = Token.newBuilder(docParse.getToken(depIdx));
      Annotation.Builder anno = Annotation.newBuilder();
      anno.setType(parserType);
      anno.setTargetToken(targetIdx);
      anno.setMethodIndex(parserIdx);
      // toString() includes the word form of collapsed tokens (e.g., prep_for)   
      anno.setText(entry.getValue().getLabel());  //getShortName());
      tok.addAnnotation(anno);
      docParse.setToken(depIdx, tok);
    }
    
    return docParse.build();
  }
  
  public static boolean hasDependencyAnnotation(Document doc) {
    // Check whether document has dependency annotation.
    int methodInd = -1;
    for (int mi = 0; mi < doc.getMethodCount() && methodInd == -1; ++mi) {
      Method m = doc.getMethod(mi);
      if (m.hasId() && m.getId().equals(METHOD_NAME)) {
        methodInd = mi;
      }
    }
    return methodInd != -1;  
  }
  

  
  void addEdge(int startIdx, String label, int targetToken) {
    Edge edge = new Edge(label, targetToken);
    index2edges.put(startIdx, edge);
  }
  
  /**
   * 
   * This returns a String representation of the shortest path between two 
   * nodes, suitable for matching with PRIS patterns.
   * The dependency labels are directly integrated into the string, while the 
   * suffix -1 is used to indicate inverse directions (e.g., from subject to 
   * verb). 
   * 
   * For example if the shortest path from token_2 to token_7 goes through 
   * token_5 and the following dependencies exist 
   * 
   * child edge_label parent
   * -----------------------
   * token_2 SUBJ token_5
   * token_7 OBJ token_5
   * 
   * Then the String pathString(2,7) returns
   * 
   * token_2 SUBJ-1 token_5 OBJ token_7
   * 
   * Note, that pathString(7,2) would return
   * 
   * token_7 OBJ-1 token_5 SUBJ token_2
   * 
   * @param indexA index of a token in the sentence
   * @param indexB index of another token in the sentence
   * @return The string representation of the shortest path connecting indexA
   * with indexB, the empty String if there is no such path.
   */
  public String patternPathString(int indexA, int indexB, List<Token> tokens) {
    if (tokens.size() <= indexB || tokens.size() <= indexA || indexA < 0 || 
        indexB < 0) {
      throw new IllegalArgumentException("0 < index{A,B} < tokens.size(), was " 
          + indexA + ", " + indexB);
    }
    
    List<Integer> path = shortestPath(indexA, indexB);
    StringBuffer sb = new StringBuffer();
    
    int lastIndex = indexA;
    for (int i = 1; i < path.size(); ++i) {
      int currIndex = path.get(i);
      if (i > 1) {
        sb.append(" ");
      }
      String tokenText =  lastIndex == indexA ? "$ARG1" : 
                          lastIndex == indexB ? "$ARG2" : 
                            tokens.get(lastIndex).getText();
      sb.append(tokenText);
      sb.append(" ");
      boolean upEdge = false;
      for (Edge e : index2edges.get(lastIndex)) {
        if (e.getTarget() == currIndex) {
          sb.append(e.getLabel() + "-1");
          upEdge=true;
          break;
        }
      }
      if (!upEdge) {
        for (Edge e : index2edges.get(currIndex)) {
          if (e.getTarget() == lastIndex) {
            sb.append(e.getLabel());
            break;
          }
        }        
      }
      lastIndex = currIndex;
    }
    if (lastIndex != indexA) {
      sb.append(" ");
    }
    String tokenText =  lastIndex == indexA ? "$ARG1" : 
                        lastIndex == indexB ? "$ARG2" : 
                          tokens.get(lastIndex).getText();
    sb.append(tokenText);
    
    return sb.toString();
  }
  
  
  /**
   * 
   * This returns a String representation of the shortest path between two 
   * nodes. The string representation is made up from the labels of the edges
   * and indicates the directionality of the individual edges.
   * 
   * For example if the shortes path from token_2 to token_7 goes through 
   * token_5 and the following dependencies exist 
   * 
   * child edge_label parent
   * -----------------------
   * token_2 SUBJ token_5
   * token_7 OBJ token_5
   * 
   * Then the String pathString(2,7) returns
   * 
   * SUBJ-><-OBJ
   * 
   * Note, that pathString(7,2) would return
   * 
   * OBJ-><-SUBJ
   * 
   * @param indexA index of a token in the sentence
   * @param indexB index of another token in the sentence
   * @return The string representation of the shortest path connecting indexA
   * with indexB, the empty String if the is no such path.
   */
  public String pathString(int indexA, int indexB) {
    List<Integer> path = shortestPath(indexA, indexB);
    StringBuffer sb = new StringBuffer();
    
    int lastIndex = indexA;
    for (int i = 1; i < path.size(); ++i) {
      int currIndex = path.get(i);
      if (i > 1) {
        sb.append(" ");
      }
      boolean upEdge = false;
      for (Edge e : index2edges.get(lastIndex)) {
        if (e.getTarget() == currIndex) {
          sb.append(e.getLabel());
          sb.append("->");
          upEdge=true;
          break;
        }
      }
      if (!upEdge) {
        for (Edge e : index2edges.get(currIndex)) {
          if (e.getTarget() == lastIndex) {
            sb.append("<-");
            sb.append(e.getLabel());
            break;
          }
        }        
      }
      lastIndex = currIndex;
    }
    
    return sb.toString();
  }
  
  /**
   * 
   * Lexicalized shortest path.
   * 
   * For example if the shortes path from token_2 to token_7 goes through 
   * token_5 and the following dependencies exist 
   * 
   * child edge_label parent
   * -----------------------
   * token_2 SUBJ token_5
   * token_7 OBJ token_5
   * 
   * Then the String pathString(2,7) returns
   * 
   * SUBJ-> token_5 <-OBJ
   * 
   * Note, that pathString(7,2) would return
   * 
   * OBJ-> token_5 <-SUBJ
   * 
   * @param indexA index of a token in the sentence
   * @param indexB index of another token in the sentence
   * @return The string representation of the shortest path connecting indexA
   * with indexB, the empty String if the is no such path.
   */
  public String pathStringLexicalized(int indexA, int indexB, Document doc) {
    List<Integer> path = shortestPath(indexA, indexB);
    StringBuffer sb = new StringBuffer();
    
    int lastIndex = indexA;
    for (int i = 1; i < path.size(); ++i) {
      int currIndex = path.get(i);
      if (i > 1) {
        sb.append(" ");
      }
      boolean upEdge = false;
      for (Edge e : index2edges.get(lastIndex)) {
        if (e.getTarget() == currIndex) {
          sb.append(e.getLabel());
          sb.append("->");
          upEdge=true;
          break;
        }
      }
      if (!upEdge) {
        for (Edge e : index2edges.get(currIndex)) {
          if (e.getTarget() == lastIndex) {
            sb.append("<-");
            sb.append(e.getLabel());
            break;
          }
        }        
      }
      
      if (currIndex != indexB) {
        sb.append(" " + doc.getToken(currIndex).getText() + " ");
      }
      
      lastIndex = currIndex;
    }
    
    return sb.toString();
  }
  
  public String MintzWindowFeature(int indexA, int indexB, int indexForWindow, Document doc) {
    if (indexForWindow != indexA && indexForWindow != indexB) {
      throw new IllegalArgumentException("Window node must be for start or end");
    }
    
    List<Integer> path = shortestPath(indexA, indexB);
    int windowIdx = -1;

    // Is there an outgoing edge, that is not on the path?
    for (Edge e : index2edges.get(indexForWindow)) {
      if (!path.contains(e.targetIndex)) {
        windowIdx = e.targetIndex;
        break;
      }
    }
    // No outcoming edge. Maybe an incoming edge?
    for (int i = 0; i < doc.getTokenCount() && windowIdx == -1; ++i) {
      if (path.contains(i)) {
        continue;
      }
      for (Edge e : index2edges.get(i)) {
        if (e.targetIndex == indexForWindow) {
          windowIdx = e.targetIndex;
          break;
        }
      }
    }
    if (windowIdx == -1) {
      return null;
    }
    return doc.getToken(windowIdx) + pathString(windowIdx, indexForWindow);
  }
  
  /**
   * This finds the shortest path from A to B. This is done by breadth first 
   * search on the transition matrix of the undirected graph of the dependency
   * tree (i.e. egdes are followed in both directions).
   * 
   * @param indexA index of a token in the sentence.
   * @param indexB index of another token in the sentence.
   * @return The indices of the nodes the shortest path traverses.
   */
  private List<Integer> shortestPath(int indexA, int indexB) {
    Multimap<Integer, Integer> transitions = 
      HashMultimap.create(numTokens,index2edges.values().size() * 2);
    boolean[] blocked = new boolean[numTokens];
    int[] comingFrom = new int[numTokens];
    
    for (int i1 : index2edges.keySet()) {
      for (Edge e : index2edges.get(i1)) {
        int i2 = e.getTarget();
        transitions.put(i1, i2);
        transitions.put(i2, i1);
      }
    }
    
    LinkedList<Integer> toVisit = new LinkedList<Integer>();
    toVisit.add(indexA);
    
    boolean foundPath = false;
    while (toVisit.size() > 0 && !foundPath) {
      int currNode = toVisit.poll();
      for (int nextNode : transitions.get(currNode)) {
        if (-1 == nextNode) {
          continue;
        }
        if (!blocked[nextNode]) {
          comingFrom[nextNode] = currNode;
          blocked[nextNode] = true;
          toVisit.add(nextNode);
          if (nextNode == indexB) {
            foundPath = true;
            break;
          }
        }
      }
    }
    
    List<Integer> path = new LinkedList<Integer>();
    
    if (foundPath) {
      for (int nextNode = indexB; nextNode != indexA; 
          nextNode = comingFrom[nextNode]) {
        path.add(0, nextNode);
      }
      path.add(0, indexA);
    }
    return new ArrayList<Integer>(path);
  }

  /**
   * This returns the head of a given span (start inclusive, end exclusive).
   * The head is the element that governs, through paths within the span, most 
   * tokens in the span. In case of ties, the rightmost token is selected.
   * 
   * @param start token index
   * @param end token index
   * @return the index of the head token.
   */
  public int getHead(int start, int end) {
    if (end > numTokens + 1000 || start > end || start < 0) {
      throw new IllegalArgumentException("Illegal indices: " + start + 
          ", " + end + ". Max index allowed: " + (numTokens));
    }
    int[] headOf = new int[end - start];
    for (int i = start; i < end; i ++) {
      // increment count for all tokens that govern token i.
      boolean[] blocked = new boolean[end - start];
      LinkedList<Integer> toVisit = new LinkedList<Integer>();
      toVisit.add(i);
      while (toVisit.size() > 0) {
        int currNode = toVisit.poll();
        for (Edge nextNode : index2edges.get(currNode)) {
          int head = nextNode.getTarget();
          if (head < start || head >= end || blocked[head - start]) {
            continue;
          }
          if (!blocked[head - start]) {
            headOf[head - start] += 1;
            blocked[head - start] = true;
            toVisit.add(head);
          }
        }
      }
    }
    int head = 0;
    for (int cand = 1; cand < end - start; ++cand) {
      if (headOf[cand] >= headOf[head]) {
        head = cand;
      }
    }
    return start + head;
  }
}
