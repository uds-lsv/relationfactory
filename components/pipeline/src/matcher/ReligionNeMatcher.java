package matcher;

public class ReligionNeMatcher extends NePatternMatcher {
  private static ReligionNeMatcher instance = null;
  public static ReligionNeMatcher getInstance() {
    if(instance == null) {
       instance = new ReligionNeMatcher();
    }
    return instance;
  }
  public ReligionNeMatcher() {
    super("B-E:NORP:RELIGION( I-E:NORP:RELIGION)*");
  }
}
