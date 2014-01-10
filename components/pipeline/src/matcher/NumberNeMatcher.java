package matcher;

public class NumberNeMatcher extends NePatternMatcher {  
  public NumberNeMatcher() {
    super("B-N:CARDINAL( I-N:CARDINAL)*");
  }
}
