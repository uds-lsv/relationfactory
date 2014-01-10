package matcher;

public class NorpPoliticalNeMatcher extends NePatternMatcher {
  private static NorpPoliticalNeMatcher instance = null;
  public static NorpPoliticalNeMatcher getInstance() {
    if(instance == null) {
       instance = new NorpPoliticalNeMatcher();
    }
    return instance;
  }
  public NorpPoliticalNeMatcher() {
    super("B-E:NORP:POLITICAL( I-E:NORP:POLITICAL)*");
  }
}
