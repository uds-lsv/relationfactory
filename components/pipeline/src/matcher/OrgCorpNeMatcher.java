package matcher;

public class OrgCorpNeMatcher extends NePatternMatcher {  
  private static OrgCorpNeMatcher instance = null;
  public static OrgCorpNeMatcher getInstance() {
    if(instance == null) {
       instance = new OrgCorpNeMatcher();
    }
    return instance;
  }
    public OrgCorpNeMatcher() {
    super("B-E:ORGANIZATION:CORPORATION( I-E:ORGANIZATION:CORPORATION)*");
  }
}
