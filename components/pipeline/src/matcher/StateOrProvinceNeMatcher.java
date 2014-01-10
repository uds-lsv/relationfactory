package matcher;

public class StateOrProvinceNeMatcher extends NePatternMatcher {
  
  private static StateOrProvinceNeMatcher instance = null;
  public static StateOrProvinceNeMatcher getInstance() {
    if(instance == null) {
       instance = new StateOrProvinceNeMatcher();
    }
    return instance;
  }
  
  public StateOrProvinceNeMatcher() {
    super("(B-E:GPE_DESC:STATE_PROVINCE( I-E:GPE_DESC:STATE_PROVINCE)*|B-E:GPE:STATE_PROVINCE( I-E:GPE:STATE_PROVINCE)*)");
  }
}
