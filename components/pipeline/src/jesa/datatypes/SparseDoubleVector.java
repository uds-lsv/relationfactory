package jesa.datatypes;

import java.util.Arrays;

public class SparseDoubleVector {
	private int[] inds;
	private double[] vals;
	private double storedL2norm;
	
	// TODO: should be less a problem for doubles
	public static final double MAX_DBL_BY_2 = Double.MAX_VALUE / 2;
	public static final double SQRT_MDB2 = Math.sqrt(Double.MAX_VALUE / 2);
	
	public static SparseDoubleVector sumOf(SparseDoubleVector v1, SparseDoubleVector v2 ){
		int[] nInds = new int[v1.inds.length + v2.inds.length];
		double[] nVals = new double[v1.inds.length + v2.inds.length];
		
		int nPos = 0;
		int i1 = 0;
		int i2 = 0;
		while ( i1 != v1.inds.length || i2 != v2.inds.length ){
			
			if ( i1 == v1.inds.length ) {
				nInds[nPos] = v2.inds[i2];
				nVals[nPos] = v2.vals[i2];				
				
				nPos++;
				i2++;
			} else if ( i2 == v2.inds.length ) {
				nInds[nPos] = v1.inds[i1];
				nVals[nPos] = v1.vals[i1];
				
				nPos++;
				i1++;
			} else if ( v1.inds[i1] > v2.inds[i2] ) {
				nInds[nPos] = v2.inds[i2];
				nVals[nPos] = v2.vals[i2];				
				
				nPos++;
				i2++;				
			} else if ( v1.inds[i1] < v2.inds[i2] ) {
				nInds[nPos] = v1.inds[i1];
				nVals[nPos] = v1.vals[i1];
				
				nPos++;
				i1++;				
			} else {
				nInds[nPos] = v1.inds[i1];
				nVals[nPos] = v1.vals[i1] + v2.vals[i2];
				
				nPos++;
				i1++;
				i2++;
			} 
		}
		
		return new SparseDoubleVector( Arrays.copyOf(nInds, nPos), Arrays.copyOf(nVals, nPos) );
	}
	
	public SparseDoubleVector clone(){
		return new SparseDoubleVector(this.inds.clone(),this.vals.clone());
	}
	
	/**
	 * This constructs a sparse vector from a String representation of the
	 * form:
	 * 
	 * indexA valueA indexB valueB (...) indexN valueN
	 * 
	 * NOTE: the indices must be in ascending order!
	 * 
	 * @param strRepr the representation
	 */
	public SparseDoubleVector(String strRepr) {
		// TODO: check ascending order!
		
		String[] vecStr = strRepr.split(" "); // TODO: slow!

		inds = new int[ vecStr.length / 2 ];
		vals = new double[ vecStr.length / 2 ];
		
		for ( int i = 0 ; i < inds.length; i++ ){
			inds[i] = Integer.parseInt( vecStr[ 2*i ] );
			vals[i] = Double.parseDouble( vecStr[ 2*i + 1 ] );
		}
		
		storedL2norm = calculateL2norm();
	}
	
	public static SparseDoubleVector fromFull(String strRepr){
		String[] vecStr = strRepr.split(" "); // TODO: slow!
		
		int[] inds = new int[ vecStr.length ];
		double[] vals = new double[ vecStr.length ];
		
		int pos = 0;
		for ( int i = 0 ; i < vecStr.length; i++ ){
			double val = Double.parseDouble( vecStr[ i ] );
			
			if ( val != 0 ){
				vals[pos] = val;
				inds[pos] = i;
				pos++;
			}
		}
		
		return new SparseDoubleVector(Arrays.copyOf(inds,pos), Arrays.copyOf(vals, pos) );
	}
	
	//TODO: untested
	public static SparseDoubleVector fromFull(double[] fullVals){
		
		int[] inds = new int[ fullVals.length ];
		double[] vals = new double[ fullVals.length ];
		
		int pos = 0;
		for ( int i = 0 ; i < fullVals.length; i++ ){
			double val = fullVals[i];
			
			if ( val != 0 ){
				vals[pos] = val;
				inds[pos] = i;
				pos++;
			}
		}
		
		return new SparseDoubleVector(Arrays.copyOf(inds,pos), Arrays.copyOf(vals, pos) );
	}
	
	/*
    public SparseDoubleVector(String line) {
        // line parsing...
        int from = 0;
        int to = 0;
        
        boolean isIndex = true;
        int i = 0;
        
        // index value index value ...
        for(;to < line.length(); to++){
            char ch = line.charAt(to);

            if (ch==' '){
	            if (isIndex){
	                int ind = Integer.parseInt( line.substring(from, to) );
	                this.inds[i] = ind; // TODO: how to initialize? first loop to count white spaces? List to Array?
	                from = ++to;
	            } else {
	                double val = Double.parseDouble(line.substring(from, to));
	                this.vals[i] = val;
	                from = ++to;
	                i++;
	            }
	            isIndex = !isIndex;
            }
        }
        // no white space after last count
        vals[i] = Double.parseDouble(line.substring(from, to) );
	}*/
	
	public SparseDoubleVector(int[] is, double[] ds) {
		this.inds = is;
		this.vals = ds;
		storedL2norm = calculateL2norm();
	}

	private double calculateL2norm(){
		double l2norm = 0;
		
		for ( int i = 0 ; i < inds.length; i++ ){
			if ( l2norm > MAX_DBL_BY_2 || vals[i] > SQRT_MDB2 ){
				throw new IllegalStateException("Vector contains too big component. Dim = " + inds.length);
			}
			l2norm += vals[i] * vals[i];
		}
		
		return Math.sqrt(l2norm);
	}
	
	/**
	 * cosine between two vectors:
	 * complexity: O( max( this.nnz(), other.nnz() ) )
	 * 
	 * @param other
	 * @return
	 */
	public double cosSim(SparseDoubleVector other){
		/*int thisI = 0;
		int otherI = 0;
		double numerator = 0;
		while (thisI != this.inds.length && otherI != other.inds.length ){
			if ( this.inds[thisI] == other.inds[otherI] ){
				
				if ( numerator > MAX_DBL_BY_2 || this.vals[thisI] > SQRT_MDB2 || other.vals[otherI] > SQRT_MDB2){
					throw new IllegalStateException("Values too big for cosine similarity.");
				}
				
				numerator += this.vals[ thisI ] * other.vals[ otherI ];
				thisI++;
				otherI++;
			} else if ( this.inds[thisI] > other.inds[otherI] ){
				otherI++;
			} else {
				thisI++;
			}
		}*/
		
		// log 
		// System.out.println(numerator + " / (" + this.storedL2norm + " * " + other.storedL2norm + ")");
		
		return this.dotProduct(other) / (this.storedL2norm * other.storedL2norm);
	}
	
	public double dotProduct(SparseDoubleVector other){
		int thisI = 0;
		int otherI = 0;
		double dotP = 0;
		while (thisI != this.inds.length && otherI != other.inds.length ){
			if ( this.inds[thisI] == other.inds[otherI] ){
				
				if ( dotP > MAX_DBL_BY_2 || this.vals[thisI] > SQRT_MDB2 || other.vals[otherI] > SQRT_MDB2){
					throw new IllegalStateException("Values too big for cosine similarity.");
				}
				
				dotP += this.vals[ thisI ] * other.vals[ otherI ];
				thisI++;
				otherI++;
			} else if ( this.inds[thisI] > other.inds[otherI] ){
				otherI++;
			} else {
				thisI++;
			}
		}
		
		return dotP;
	}
	
	/**
	 * number of non-zero entries
	 * @return
	 */
	public int nnz(){
		return this.vals.length;
	}
	
	/**
	 * Retain biggest n components.
	 * Complexity: O( nnz() log nnz() )
	 * 
	 * @param n
	 */
	public void retainMaxNnz( int n ){
		
		if ( inds.length <= n )
			return;
		
		double[] sorted = vals.clone();
		Arrays.sort( sorted );
		
		// must be bigger than the first value outside
		double tooSmall = sorted[ sorted.length - (n + 1) ];
		
		double[] nVals = new double[n];
		int[] nInds = new int[n];
		
		int npos = 0;

		for (int i = 0; i < inds.length; i++){
			if (vals[i] > tooSmall){
				nVals[npos] = vals[i];
				nInds[npos] = inds[i];
				npos++;
			}
		}
		
		if (npos == nVals.length){
			this.vals = nVals;
			this.inds = nInds;
		} else {
			this.vals = Arrays.copyOf(nVals, npos);
			this.inds = Arrays.copyOf(nInds, npos);
		}
		
		storedL2norm = calculateL2norm();
	}
	
	/**
	 * This normalizes by l2-norm. The entries are devided by the norm, 
	 * afterwards the norm is set to 1
	 *
	 *//*
	public void normalizeL2(){
		this.normalizeL2(1.0);
	}*/
	
	/**
	 * 
	 * normalize s.t. the vector has new norm
	 * 
	 * @param newNorm
	 *//*
	public void normalizeL2(double newNorm){
		
		double normFactor = newNorm / storedL2norm;
		
		for (int i = 0 ; i < vals.length; i++){
			vals[i] *= normFactor;
		}
		storedL2norm = newNorm;
	}*/
	
	/*
	public void normalizeL1(){
		normalizeL1(1.0);
	}*/
	
	/*
	public void normalizeL1(double newNorm){
		double normFactor = newNorm / calculateL1norm();
		for (int i = 0 ; i < vals.length; i++){
			vals[i] *= normFactor;
		}
	}*/
	
	
	/*
	private double calculateL1norm(){
		double l1norm = 0;
		
		for ( int i = 0 ; i < inds.length; i++ ){
			l1norm += Math.abs( vals[i] );
		}
		
		return l1norm;
	}*/
	
	
	public void scaleBy(double scale){
		for (int i = 0 ; i < vals.length; i++){
			vals[i] *= scale;
		}
		storedL2norm = scale * storedL2norm;
	}

	public String toIntString() {
		StringBuffer sb = new StringBuffer();
		
		for ( int i = 0 ; i < inds.length; i++){
			if ( i > 0 )
				sb.append(' ');
			
			sb.append( inds[i] );
			sb.append(' ');
			
			if ( vals[i] > Integer.MAX_VALUE )
				throw new IllegalStateException("Component value too big to be written out as Integer.");
			
			sb.append( Integer.toString( (int) vals[i] ) );
		}
		
		return sb.toString();
	}
	
	public String toString() {
		StringBuffer sb = new StringBuffer();
		
		for ( int i = 0 ; i < inds.length; i++){
			if ( i > 0 )
				sb.append(' ');
			
			sb.append( inds[i] );
			sb.append(' ');
						
			sb.append( Double.toString( vals[i] ) );
		}
		
		return sb.toString();
	}
	
	public int[] getInds(){
		return this.inds;
	}

	public double calculateL1Norm() {
		double l1norm = 0;
		
		for ( int i = 0 ; i < inds.length; i++ ){
			l1norm += Math.abs(vals[i]);
		}
		
		return Math.sqrt(l1norm);
	}
}
