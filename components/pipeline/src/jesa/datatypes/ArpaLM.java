package jesa.datatypes;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;

public class ArpaLM {
	  private static Pattern sep = Pattern.compile("\\t+");
	  private HashMap<String, Double> ngram2logProb;
	  private HashMap<String, Double> ngram2logBackoff;
	  //private HashMap<String, DoublePair> ngram2logProbBackoff;
	  
	  
	  public ArpaLM( String filename ) throws IOException {
		  boolean modelStart = false;
		  ngram2logProb = new HashMap<String, Double>();
		  ngram2logBackoff = new HashMap<String, Double>();
		  //ngram2logProbBackoff = new HashMap<String, DoublePair>();
		  
			BufferedReader br = new BufferedReader(new FileReader(filename));
			
			int lineNr = 0;
			
			for (String line; ( line = br.readLine()) != null;){
				
				lineNr++;
				
				if ( lineNr % 10000 == 0)
					System.out.println( lineNr / 1000 + "k" );
				
			    if (modelStart && !line.startsWith("\\")){
			      String[] lineParts = sep.split(line);
			      // line format, separated by tabs: 
			      //prob ngram backoff
			      // ngram is seperated by whitespaces
			      if (lineParts.length > 1){
			        
			    	String keyStr = new String( lineParts[1].trim() );
			    	
			    	// TODO: hack to kepp memory consumptio low: remove again
			    	//String[] keytokens = keyStr.split(" ");
			    	//if (keyStr.length() > 80 || (keytokens.length == 3 && !keytokens[2].startsWith("#")))
			    	//	continue;
			    	
			    	//double logProb = Double.valueOf( lineParts[0] );
			    	  
			    	ngram2logProb.put( keyStr , Double.valueOf( lineParts[0] ) );
			    	
			    	//double backOff = 0.0;
			        
				    if (lineParts.length > 2){
					   ngram2logBackoff.put( keyStr, Double.valueOf( lineParts[2] ) );
				    // backOff = Double.valueOf( lineParts[2] );
					}
				    
				    //ngram2logProbBackoff.put( keyStr, new DoublePair(logProb, backOff) );
			      }
			      
			    }
			    
			    // model starts after the unigram indicator line
			    if (!modelStart && line.startsWith("\\1-grams:")){ 
			      modelStart = true;
			    }	        	
			}
			br.close();
	  }
		  
	  private String mkString( List<String> ngram, char sep ) {
		  StringBuffer sb = new StringBuffer();
		  boolean first = true;
		  for ( String tok : ngram) {
			  if ( first )
				  first = false;
			  else
				  sb.append( sep );
			  
			  sb.append(tok);
		  }
		  return sb.toString();
	  }


	  // TODO: "remember" queried words, by adding them to the map (maybe only for sentence)
	  public Double getLogProbRec( List<String> ngram ) {
	    
	    // this should never be true for an open vocabulary language model
	    if (ngram.size() == 0)
	      return 0.0;
	    
	    String ngString = mkString(ngram, ' ');
	    
	    if ( ngram2logProb.containsKey( ngString )  ){
	      return ngram2logProb.get( ngString ); // TODO: sentence boundaries..
	    } else {
	            
	      String history = mkString( ngram.subList( 0, ngram.size() - 1 ) , ' ' );
	      List<String> shortNGram  = ngram.subList( 1, ngram.size() );
	      
	      double backoff = 0.0;
	      
	      if ( ngram2logBackoff.containsKey(history) )
	    	  backoff = ngram2logBackoff.get( history );

	      return getLogProbRec(shortNGram) + backoff;
	    }
	  }
	  
	  /*
	  public Double getLogProbRec( List<String> ngram ) {
		    
		    // this should never be true for an open vocabulary language model
		    if (ngram.size() == 0)
		      return 0.0;
		    
		    String ngString = mkString(ngram, ' ');
		    
		    if ( ngram2logProbBackoff.containsKey( ngString )  ){
		      return ngram2logProbBackoff.get( ngString ).getFirst(); // TODO: sentence boundaries..
		    } else {
		            
		      String history = mkString( ngram.subList( 0, ngram.size() - 1 ) , ' ' );
		      List<String> shortNGram  = ngram.subList( 1, ngram.size() );
		      
		      double backoff = 0.0;
		      
		      if ( ngram2logProbBackoff.containsKey(history) )
		    	  backoff = ngram2logProbBackoff.get( history ).getSecond();

		      return getLogProbRec(shortNGram) + backoff;
		    }
		  }
	  */
	  /** 
	   * This returns a log-probability for the queried n-gram according to the recursive equation and the lm/backoff scores.
	   * Unknown words are assigned the value for <unk> tokens in an open vocabulary model, or 0.0 (i.e. ignored) in a 
	   * closed vocabulary model.
	   * 
	   * The last String of the Sequence indicates the word for which the probability is queried, conditioned on the sequence 
	   * of the others.
	   */
	  public Double getLogProb( String[] ngram ) {
		List<String> ngramList = new LinkedList<String>();  
		
		for ( String word : ngram ){
			//if ( ngram2logProbBackoff.containsKey(word) )
			if ( ngram2logProb.containsKey(word) )
				ngramList.add(word);
			else
				ngramList.add("<unk>");
		}
		
		return getLogProbRec(ngramList);
	  }
	  
	  public Double getLogProb( List<String> ngram ) {
			List<String> ngramList = new LinkedList<String>();  
			
			for ( String word : ngram ){
				//if ( ngram2logProbBackoff.containsKey(word) )
				if ( ngram2logProb.containsKey(word) )
					ngramList.add(word);
				else
					ngramList.add("<unk>");
			}
			
			return getLogProbRec(ngramList);
		  }
	  
	  /**
	   * This returns the probability of an ngram, using getLogProb and assuming the lm is stored in the log to the base 10.
	   * 
	   * @param ngram
	   * @return
	   */
	  public Double getProb( String[] ngram ) {
		  return Math.pow(10, getLogProb(ngram));
	  }
	  
	  public Double getProb( List<String> ngram ) {
		  return Math.pow(10, getLogProb(ngram));
	  }
}
