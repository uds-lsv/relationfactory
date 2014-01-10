package matcher;

public class LongTitleNeMatcher extends NePatternMatcher {  
  public LongTitleNeMatcher() {
    super("B-E:PER_DESC( I-E:PER_DESC)+");
  }
}
