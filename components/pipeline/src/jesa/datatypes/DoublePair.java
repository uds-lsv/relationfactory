package jesa.datatypes;

public class DoublePair {// TODO: generalize to typed pair
		private double s1;
		private double s2;
		private int hashCode;
		
		public DoublePair(double first, double second) {

			this.s1 = first;
			this.s2 = second;

	        this.hashCode = (new Double(s1).hashCode() << 3) ^ ( new Double(s2).hashCode() >> 1);

	    }
		
		public int hashCode() {
			return this.hashCode;
		}
		
		public boolean equals(Object obj) {
			if(this == obj)
				return true;
			
			if((obj == null) || (obj.getClass() != this.getClass()))
				return false;
				
			DoublePair other = (DoublePair)obj;
			
			return s1==other.s1 && s2 == other.s2  ;
		}
		
		public Double getFirst(){
			return this.s1;
		}
		
		public Double getSecond(){
			return this.s2;
		}

}
