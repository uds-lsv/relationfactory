package util;

public class OffsetPair {
  public final int start;
  public final int end;

  public OffsetPair(int start, int end) {
    if (start < 0 || start > end) {
      throw new IllegalArgumentException("0 < start <= end, was " + 
          start + ", " + end);
    }
    this.start = start;
    this.end = end;
  }
  
  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + end;
    result = prime * result + start;
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    OffsetPair other = (OffsetPair) obj;
    if (end != other.end)
      return false;
    if (start != other.start)
      return false;
    return true;
  }

  @Override
  public String toString() {
    return "OffsetPair [start=" + start + ", end=" + end + "]";
  }
}