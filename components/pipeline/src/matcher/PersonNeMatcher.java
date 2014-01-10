package matcher;

public class PersonNeMatcher extends NePatternMatcher {

  private static PersonNeMatcher instance = null;
  public static PersonNeMatcher getInstance() {
    if(instance == null) {
       instance = new PersonNeMatcher();
    }
    return instance;
  }
  public PersonNeMatcher() {
    super("B-E:PERSON( I-E:PERSON)*");
  }
}
