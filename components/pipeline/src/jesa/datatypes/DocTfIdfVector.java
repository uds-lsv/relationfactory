package jesa.datatypes;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.SortedMap;


public class DocTfIdfVector {
	
	private SortedMap<Integer,Double> term2idf;
	String name;

	public DocTfIdfVector(String nameString, SortedMap<Integer, Double> term2tfIdfMap) {
		this.name = nameString;
		this.term2idf = term2tfIdfMap;
	}
	
	/**
	 * 
	 * Parses a line and fills the List alternating with term ids and counts.
	 * 
	 * @param line
	 * @param idCountPair
	 * @return
	 * @throws an IllegaArgumentException if idCountPair is not empty
	 */
	/*
	public static String parseRaw(String line, List<Integer> idCountPair){
		// TODO implement
		if ( !idCountPair.isEmpty() ){
			throw new IllegalArgumentException("id-count List has to be empty in Order to be filled");
		}
		return null;
	}
*/
    public void writeTo(BufferedWriter bw, double minTfIdf) throws IOException{
        bw.append(name);
        bw.append(' ');

        boolean first = true;

        for (int termId : term2idf.keySet() ){

            if (first){
                first = false;
            } else {
                bw.append(' ');
            }

			bw.append(Integer.toString(termId));
			bw.append(' ');
			
			// scale to int s.t. smallest value becomes 1
			bw.append(Integer.toString( (int) (term2idf.get(termId) / minTfIdf) ) );

            // TODO: here is the bottleneck of the attribute vector part
            // TODO: why write to ascii files anyway???
            // this gives smallest files and is most readable
            //bw.append( nf.format( term2idf.get(termId)) );
            // this performes fastest but gives big files
            //bw.append(Float.toString( term2idf.get(termId).floatValue()));
            // this also perfomes fast,but the files are not very readable
            //bw.append(Double.toHexString(term2idf.get(termId)));
        }
    }
    
	public void increaseDf( int[] filteredDf ) {
		for (int termId : this.term2idf.keySet() ){
			filteredDf[ termId ]++;
		}
	}
}
