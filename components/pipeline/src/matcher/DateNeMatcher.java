package matcher;

public class DateNeMatcher extends NePatternMatcher {  

  private static DateNeMatcher instance = null;
  public static DateNeMatcher getInstance() {
    if(instance == null) {
       instance = new DateNeMatcher();
    }
    return instance;
  }
  
  public DateNeMatcher() {
    super("B-T:DATE:DATE( I-T:DATE:DATE)*");
  }
}
