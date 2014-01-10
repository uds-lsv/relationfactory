package jesa.datatypes;

/**
 * Class to store pairs of integers to be used as a hash-key. Lesson learnde:
 * Never try to use String[] to store such keys - leads to memory problems.
 * 
 * @author beroth
 *
 */
public class StringPair {
	private String s1;
	private String s2;
	private int hashCode;
	
	public StringPair(String first, String second) {

		this.s1 = first;
		this.s2 = second;

        if (null == s1 || null == s2){
            throw new IllegalArgumentException("cannot instantiate with null strings");
        }

        //this.hashCode = this.s1.hashCode() ^ this.s2.hashCode() ^ (this.s1.hashCode() << 3) ^ (this.s2.hashCode() >> 1);
        //this.hashCode = (this.s1.hashCode() << 7) ^ (this.s2.hashCode() >> 3);
        this.hashCode = (this.s1.hashCode() << 3) ^ (this.s2.hashCode() >> 1);

    }
	
	public int hashCode() {
		return this.hashCode;
	}
	
	public boolean equals(Object obj) {
		if(this == obj)
			return true;
		
		if((obj == null) || (obj.getClass() != this.getClass()))
			return false;
			
		StringPair other = (StringPair)obj;
		
		return s1.equals( other.s1 ) && s2.equals( other.s2 ) ;
	}
	
	public String getFirst(){
		return this.s1;
	}
	
	public String getSecond(){
		return this.s2;
	}

}
