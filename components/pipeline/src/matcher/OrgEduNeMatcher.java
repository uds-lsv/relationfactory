package matcher;

public class OrgEduNeMatcher extends NePatternMatcher {  
  private static OrgEduNeMatcher instance = null;
  public static OrgEduNeMatcher getInstance() {
    if(instance == null) {
       instance = new OrgEduNeMatcher();
    }
    return instance;
  }
  public OrgEduNeMatcher() {
    super("B-E:ORGANIZATION:EDUCATIONAL( I-E:ORGANIZATION:EDUCATIONAL)*");
  }
}
