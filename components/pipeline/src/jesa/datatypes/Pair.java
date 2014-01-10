package jesa.datatypes;

public class Pair<Type> {
	private Type el1;
	private Type el2;
	private int hashCode;
	
	public Pair( Type first, Type second) {
		this.el1 = first;
		this.el2 = second;
        this.hashCode = (el1.hashCode() << 3) ^ ( el2.hashCode() >> 1);
    }
	
	public int hashCode() {
		return this.hashCode;
	}
	
	public boolean equals(Object obj) {
		if(this == obj)
			return true;
		
		if((obj == null) || (obj.getClass() != this.getClass()))
			return false;
			
		Pair other = (Pair)obj;
		
		return el1.equals( other.el1 ) && el2.equals( other.el2 );
	}
	
	public Type getFirst(){
		return this.el1;
	}
	
	public Type getSecond(){
		return this.el2;
	}

}
