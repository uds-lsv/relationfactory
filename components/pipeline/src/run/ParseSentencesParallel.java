package run;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import parser.DependencyTree;
import parser.SentenceDependencyParser;
import parser.StanfordParser;
import rerac.protos.Corpus.Document;

/**
 * This parses single sentence documents using the stanford nlp tagger.
 * It can parse multiple sentences in parallel. Note: the ouput
 * sentences are not neccessarily ordered the same way as the input sentences!
 * 
 * Partly copied from RERAC, May 2013
 * 
 * @author Tassilo Barth (tbarth)
 *
 */
public class ParseSentencesParallel {

  public static final String REFERRED_NAME = "parse_sentences";
  // Sentences with more tokens than this value will not be parsed.
  public static int SENTENCE_MAXLEN = 50;

  // Assumption: Thread-safe
  private final SentenceDependencyParser parser;
  private ConcurrentHashMap<String, DependencyTree> parses;

  /**
   * This parses single sentence documents using the given parser and puts the
   * result into a given queue.
   * 
   * @author Tassilo Barth
   *
   */
  private class ParallelParseProducer implements Runnable {
    private final List<String> tokens;
    private final String sid;

    public ParallelParseProducer(List<String> tokens, String sid) {
      this.tokens = tokens;
      this.sid = sid;
    }

    @Override
    public void run() {
      DependencyTree tree = parser.parseToDepTree(tokens);
      if (tree == null)  {
        return;
      }
      // ConcurrentHashMap handles locking.
      parses.put(sid, tree);
    }
  }

  public ParseSentencesParallel(SentenceDependencyParser parser) {
    this.parser = parser;
  }

  public int parseSentences(int cores, InputStream is) throws IOException {
    ExecutorService producerExec = Executors.newFixedThreadPool(cores);
    Map<String, List<String>> sentencesToTokens = 
        new HashMap<String, List<String>>();

    int numEntries = 0;
    int maxEntries = Integer.MAX_VALUE;
    //int maxEntries = 10;
    int numSkippedEntries = 0;
    
    for (Document doc;
        sentencesToTokens.size() < maxEntries && 
        (doc = Document.parseDelimitedFrom(is)) != null;) {
      if (doc.getTokenCount() > SENTENCE_MAXLEN) {
        numSkippedEntries++;
        continue;
      }
      if (!sentencesToTokens.containsKey(doc.getId())) {
        List<String> tokens = new ArrayList<String>(doc.getTokenList().size());
        for (int i = 0; i < doc.getTokenList().size(); i++) {
          tokens.add(doc.getTokenList().get(i).getText());
        }
        sentencesToTokens.put(doc.getId(), tokens);
      }
    }

    parses = new ConcurrentHashMap<String, DependencyTree>(
        sentencesToTokens.size());

    for (Entry<String, List<String>> entry : sentencesToTokens.entrySet()) {
      producerExec.execute(new ParallelParseProducer(entry.getValue(),
          entry.getKey()));
      ++numEntries;
    }
    
    Timer timer = new Timer();
    timer.schedule( new TimerTask() {
      @Override
      public void run() {
        System.err.print("\r ... Parsed " + getNumParsed() + 
            " sentences.");
      }
    }, 5 * 1000, 5 * 1000 );  // start in 5 seconds, repeat every 5 seconds

    System.err.println("Sent " + numEntries + " sentences to parser. " +
        numSkippedEntries + " entries were skipped. " +
        "Now waiting for parses to end.");

    producerExec.shutdown();
    try {
      // Reasonable guess at how long one parse should take on average (max).
      if (!producerExec.awaitTermination(numEntries * 5 / cores, 
          TimeUnit.SECONDS)) {
        System.err.println("Remaining parses are taking too long, will stop them now.");
        producerExec.shutdownNow();
      }
    } catch (InterruptedException e) {
      producerExec.shutdownNow();
    }
    timer.cancel();
    System.err.println("");
    return numEntries;
  }
  
  public int annotateDocuments(InputStream is, DataOutputStream out) 
      throws IOException {
    if (parses == null) {
      throw new IllegalStateException("No parses to write out (parses was not" +
          "initaizliaed");
    }
    int annotated = 0;
    for (Document doc;
        (doc = Document.parseDelimitedFrom(is)) != null;) {
      DependencyTree tree = parses.get(doc.getId());
      if (tree == null) {
        //throw new IllegalStateException("Unparsed sentence: " + doc.getId());
        continue;
      }
      tree.annotate(doc).writeDelimitedTo(out);
      annotated++;
    }
    return annotated; 
  }
  
  public int getNumParsed() {
    return parses.size();
  }

  public static void main(String[] args) throws IOException, InterruptedException {
    if (args.length < 2 || args.length > 4) {
      System.err.println("ParseSentencesParallel " +
          "<candidates.pb> <path to serialized stanford parser grammar> " +
          "[<number of threads>] [<max sentence length>]" +
          " > output.pb");
      return;
    }   

    // Sentences on stdin, parsed sentences on stdout.
    String parserGrammar = args[1];
    String candidatesFn = args[0];

    int cores = Math.max(Runtime.getRuntime().availableProcessors() - 1, 1); 
    if (args.length >= 3) {
      try {
        cores = Math.min(cores, Integer.parseInt(args[2]));
        if (cores < 0) {
          throw new NumberFormatException();
        }
      } catch(NumberFormatException e) {
        System.err.println("Wrong format for number of threads, must be an" +
            " integer > 0! Ignoring the given value (" + args[2] + ").");
      }
    }
    System.err.println("Using " + cores + " threads.");
    
    if (args.length >= 4) {
      SENTENCE_MAXLEN = Integer.parseInt(args[3]);
    }
    System.err.println("Maximum sentence length: " + SENTENCE_MAXLEN);
    
    SentenceDependencyParser parser = new StanfordParser(parserGrammar);
    final ParseSentencesParallel driver = new ParseSentencesParallel(parser);
    
    File candidatesFile = new File(candidatesFn);
    long lastmodtime = candidatesFile.lastModified();
    
    BufferedInputStream is = new BufferedInputStream(
        new FileInputStream(candidatesFile));
    long startTime = System.currentTimeMillis();
    // this blocks
    int numEntries = driver.parseSentences(cores, is);
    long currentTime = System.currentTimeMillis();
    double speed = numEntries * 1000.0 / (currentTime - startTime);
    is.close();
        
    System.err.println(speed + " sentences / second (likely misleading when parsing " +
    		"was aborted)");
    System.err.println("All parses done. Now waiting for writing out " +
        "to end.");
    
    if (!candidatesFile.exists() || 
        candidatesFile.lastModified() != lastmodtime) {
      throw new IllegalStateException("The candidates input file " + 
        candidatesFn + " was deleted or modified during the parsing. " +
        "Consistency can no longer be guaranteed.");
    }
    
    DataOutputStream output = new DataOutputStream(new BufferedOutputStream(
        System.out));
    is = new BufferedInputStream(new FileInputStream(candidatesFile));
    int annotatedDocs = driver.annotateDocuments(is, output);
    output.flush();
    is.close();
    
    System.err.println("Annotated " + annotatedDocs + " documents.");
    System.err.println("Done.");
  }

}
