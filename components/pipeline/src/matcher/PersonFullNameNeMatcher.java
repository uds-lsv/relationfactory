package matcher;

public class PersonFullNameNeMatcher  extends NePatternMatcher {  
  public PersonFullNameNeMatcher() {
    super("B-E:PERSON( I-E:PERSON)+");
  }
}
