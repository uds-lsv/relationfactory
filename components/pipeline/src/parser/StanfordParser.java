package parser;

import java.util.ArrayList;
import java.util.List;

import rerac.protos.Corpus.Document;
import edu.stanford.nlp.ling.Word;
import edu.stanford.nlp.parser.lexparser.LexicalizedParser;
import edu.stanford.nlp.trees.GrammaticalStructure;
import edu.stanford.nlp.trees.GrammaticalStructureFactory;
import edu.stanford.nlp.trees.PennTreebankLanguagePack;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.trees.TreebankLanguagePack;
import edu.stanford.nlp.trees.TypedDependency;

/**
 * 
 * This is a wrapper class for using the Stanford parser.
 * 
 * @author Benjamin Roth
 *
 */
public class StanfordParser implements SentenceDependencyParser {
  
  /**
   * Wrapper for information about words.
   * 
   * @author Benjamin Roth
   *
   */
  // Parser used.
  LexicalizedParser lp;
  // Used to create the dependency strucure.
  GrammaticalStructureFactory gsf;
    
  /**
   * This creates a parser with a given grammar and standard option settings.
   * 
   * @param grammar the file name of the grammar model.
   */
  public StanfordParser(String grammar) {
    this(grammar, 
        new String[] { "-maxLength", "80", "-retainTmpSubcategories",
            "-outputFormat", "typedDependenciesCollapsed" });
  }
  
  /**
   * 
   * This creates a parser with a given grammar and given option settings.
   * 
   * @param grammar the file name of the grammar model.
   * @param options the array of option strings.
   */
  StanfordParser(String model, String[] options) {
    lp = LexicalizedParser.loadModel(model);
    lp.setOptionFlags(options);
    TreebankLanguagePack tlp = new PennTreebankLanguagePack();
    gsf = tlp.grammaticalStructureFactory();
  }
  
  /**
   * This takes a document parses it and creates a new document with added 
   * parse information which is returned.
   * 
   * @param doc the document to be parsed.
   * @return the document with added parse information.
   */
  public Document parse(Document doc) {
    ArrayList<String> tokens = new ArrayList<String>(doc.getTokenList().size());
    for (int i = 0; i < doc.getTokenList().size(); i++) {
      tokens.add(doc.getTokenList().get(i).getText());
    }
    return parseToDepTree(tokens).annotate(doc);
  }
  
  public DependencyTree parseToDepTree(List<String> tokens) {
    List<Word> wordList = new ArrayList<Word>();
    for (String token : tokens) {
      wordList.add(new Word(token));
    }
    Tree parse = lp.apply(wordList);
    GrammaticalStructure gs = gsf.newGrammaticalStructure(parse);
    DependencyTree tree = new DependencyTree();
    for(TypedDependency tdp : gs.typedDependenciesCollapsed()) {
      int depIdx = tdp.dep().index() - 1;
      int govIdx = tdp.gov().index() - 1;
      tree.addEdge(depIdx, tdp.reln().toString(), govIdx);
    }
    return tree;
  }

}
