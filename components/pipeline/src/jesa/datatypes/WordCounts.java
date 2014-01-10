package jesa.datatypes;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.Map.Entry;

public class WordCounts {

	private String name;
	private Map<String, Integer> term2count = new HashMap<String, Integer>();
	int allCounts = 0;
	
	public WordCounts(){
		name = null;
	}
	
    public WordCounts(String line) {
        // ugly line parsing...
        int from = 0;
        int to = 0;

        // entry name delimited by tab
        while(line.charAt(to) != ' ')
            to++;
        // from beginning to index at white space (excludes ws)
        // TODO: possible memory leak with sbstring - but fast!
		this.name = line.substring(from, to);
        // set to index after tab
        from = ++to;

        // term:count,term:count
        String term = null;
        int count;
        
        boolean isTerm = true;
        
        for(;to < line.length(); to++){
            char ch = line.charAt(to);

            if (ch==' '){
	            if (isTerm){
	                term=line.substring(from, to);
	                from = ++to;
	            } else {
	                count = Integer.parseInt(line.substring(from, to));
	                from = ++to;
	                this.term2count.put( term, count );
	                this.allCounts += count;
	            }
	            isTerm = !isTerm;
            }
        }
        // no white space after last count
        count = Integer.parseInt(line.substring(from, to) );
        this.term2count.put( term, count );
        this.allCounts += count;
	}
    
    public WordCounts(String name, List<String> terms) {
    	this.name = name;
    	
    	for (String t : terms){
    		this.incCount(t);
    	}
	}

	public void setName(String n) {
		this.name = n;
	}
    
    public void incCount(String term) {
    	if ( 0 != term.length() ){
			allCounts++;
			int count = term2count.containsKey(term)? term2count.get(term) : 0;
			term2count.put(term, count + 1);
    	}
	}
    
    public DocTfIdfVector getTfIdfVector(Map<String,Integer> term2df, Map<String, Integer> term2id,
    		int nDocs, double minTfIdf){
    	
		// added up counts
		double entryTypeCounts = (double) this.getTypeCount();

		// Sorted Map is handy later on, nut maybe slower?
		SortedMap<Integer, Double> term2tfIdf = new TreeMap<Integer,Double>(  );
		//Map<Integer, Double> term2tfIdf = new HashMap<Integer,Double>( this.getTerm2Count().size() );

		for ( String term : this.getTerm2Count().keySet() )
		{
			if ( term2df.containsKey( term ) ){
				
				double tf = ( (double) this.getTerm2Count().get(term) ) / entryTypeCounts;
				double idf = Math.log( nDocs / (double) term2df.get(term) );
					
				// set min threshhold
				double tfIdf = tf * idf;
				if (tfIdf >= minTfIdf){					
					term2tfIdf.put( term2id.get(term),  tfIdf );
				} 
					
			}
		}
		
		return new DocTfIdfVector(this.getName(), term2tfIdf);
    }

	
	public int getTypeCount(){
		return this.allCounts;
	}
	
	public Map<String, Integer> getTerm2Count(){
		return this.term2count;
	}

	public String getName() {
		return this.name;
	}

	public void writeTo(BufferedWriter bw) throws IOException {
		bw.append(name);
		for (String term : term2count.keySet()){
			bw.append(' ');			
			bw.append(term);
			bw.append(' ');
			int count = term2count.get(term);
			bw.append(Integer.toString( count ));			
		}
		bw.newLine();
	}
	
	public List<String> getExpandedTermList(){
		List<String> tl = new ArrayList<String>();
		
		for (Entry<String, Integer> e : term2count.entrySet()){
			for (int i = 0; i <  e.getValue(); i++){
				tl.add(e.getKey());
			}
		}
		
		return tl;
	}
	
	public void writeTo(BufferedWriter bw, Set<String> vocab) throws IOException {
		bw.append(name);
		for (String term : term2count.keySet()){
			if (vocab.contains(term)){
				bw.append(' ');			
				bw.append(term);
				bw.append(' ');
				int count = term2count.get(term);
				bw.append(Integer.toString( count ));
			}
		}
		bw.newLine();
	}
}
