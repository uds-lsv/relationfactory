package jesa.datatypes;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.List;

public class TermDocTransposer {
	// document frequency of terms
	private int[] df;
	// term counts (tfidf, int-scaled) of words in docs
	private int[] termDocCounts = null;
	// index after last written doc-count pair for a particular term
	private int[] blockPairInds = null;

	// start term index of lexicon block processed
	private int firstTermInIteration = 0;
	private int nTermsInIteration = 0;

	private static final int INTEGERBYTES = ( Integer.SIZE / 8 );
	
	public TermDocTransposer (int[] dfArr){
		this.df = dfArr;
	}
		
	public void transposeNextBlock(BufferedReader br, int memory) throws IOException{
		this.initDataStructures(memory);
		this.fillDataStructures(br);
	}

	public void transposeNextBlock(BufferedReader br) throws IOException{
		long maxMemory = Runtime.getRuntime().maxMemory();  
		long allocatedMemory = Runtime.getRuntime().totalMemory();  
		long freeMemory = Runtime.getRuntime().freeMemory();

		// use half of whats there
		// 				 				free on heap | what still can be made free
		// TODO: give more than 1/2? beware of int overflows, when multipying?
		int useMemory = (int) Math.min((freeMemory  + (maxMemory - allocatedMemory) )/2 , Integer.MAX_VALUE);
		
		transposeNextBlock(br, useMemory);
	}
		
	public void writeBlock(BufferedWriter bw, List<String> terms) throws IOException{		
		int i = 0;
		for (int termNr = 0; termNr < nTermsInIteration; termNr++){
			bw.append( terms.get(firstTermInIteration + termNr) );

			while ( i < blockPairInds[termNr] ){
				
				bw.append(' ');
				bw.append( Integer.toString( termDocCounts[i] ) );
				bw.append(' ');
				bw.append( Integer.toString( termDocCounts[i+1] ) );				
				i+=2;
			}
			bw.newLine();
		}
		
		termDocCounts = null;// GC can remove it
	}
		
	public boolean hasNextBlock(){
		return (firstTermInIteration + blockPairInds.length) != df.length;
	}
	
	private void initDataStructures(int memory){
		int maxEntries = memory / INTEGERBYTES;
				
		// the block starts where it ended last time
		if ( this.blockPairInds != null )
			firstTermInIteration += nTermsInIteration; // nTerms of last iteration
		
		// which lexicon terms are processed
		int blockEnd = firstTermInIteration;
		// how much space needed (document frequency of terms * 2)
		int accumSum = 0;
				
		// add up document frequencies of terms to see how much memory is used
		while ( blockEnd < df.length && 
				// complicated memory formula (smaller constants ignored)
				// entries of data array (=two values) + index array < memory
				(accumSum + 2 * df[blockEnd]) + (blockEnd - firstTermInIteration) < maxEntries ){
			accumSum += 2 * df[blockEnd];
			blockEnd++;
		}
		
		// has to store all document ocurrences 
		// -> size = sum of document frequencies of the words
		termDocCounts = new int[accumSum];
		
		// block of terms in lexicon for which inverted index is biult
		nTermsInIteration = blockEnd - firstTermInIteration; // nTerms newly set
		blockPairInds = new int[nTermsInIteration];
		
		// TODO
		System.out.println("blockstart: " + firstTermInIteration);
		System.out.println("blocksize: " + nTermsInIteration);
		
		if ( 0 == nTermsInIteration && df.length != 0)
			throw new IllegalStateException("Too little memory assigned to process entry.");
		
		// for every term get starting location
		for (int i = 0; i < nTermsInIteration; i++ ){
			// its the last position + space for every doc-count pair
			blockPairInds[i] = (0==i)? 
					0 : blockPairInds[i-1] + 2 * df[firstTermInIteration + i - 1];
		}		
	}
	
	private void fillDataStructures(BufferedReader br) throws IOException {		
		int docId = 0;
		
        for (String line; (line = br.readLine()) != null;){        	
	        // ugly line parsing...
	        int from = 0;
	        int to = 0;
	
	        // entry name delimited by tab
	        while(line.charAt(to) != ' ')
	            to++;
	        // set to index after tab
	        from = ++to;
	
	        int termIdInBlock=0;
	        
	        boolean isTerm = true;
	        for(;to < line.length(); to++){
	            char ch = line.charAt(to);
	
	            if (ch == ' '){
		            if (isTerm){
		            	// can be negative!
		                termIdInBlock = Integer.parseInt( line.substring(from, to) ) - firstTermInIteration;
		                from = ++to;
		            } else {	            	
		            	if (termIdInBlock >= 0 && termIdInBlock < nTermsInIteration){
		            		
		            		// if this is a term we consider, write doc Id and count
		            		// to the position in the array its index is poiting to
		            		int count = Integer.parseInt(line.substring(from, to));
		            		this.termDocCounts[ blockPairInds[termIdInBlock] ] = docId;
		            		this.termDocCounts[ blockPairInds[termIdInBlock] + 1 ] = count;
		            		
		            		// move the index pointer two fields
		            		blockPairInds[termIdInBlock] += 2;
		            		
		            	}
		                from = ++to;
		            }
		            isTerm = !isTerm;
	            }
	        }
	        // no whitespace after last cpunt
        	if (termIdInBlock >= 0 && termIdInBlock < nTermsInIteration){
        		int count = Integer.parseInt(line.substring(from, to));
        		this.termDocCounts[ blockPairInds[termIdInBlock] ] = docId;
        		this.termDocCounts[ blockPairInds[termIdInBlock] + 1 ] = count;
        		blockPairInds[termIdInBlock] += 2;	
        	}
        	
	        docId++;
        }        
	}
}
