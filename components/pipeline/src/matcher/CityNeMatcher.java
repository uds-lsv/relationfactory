package matcher;

public class CityNeMatcher extends NePatternMatcher {
  private static CityNeMatcher instance = null;
  public static CityNeMatcher getInstance() {
    if(instance == null) {
       instance = new CityNeMatcher();
    }
    return instance;
  }
  public CityNeMatcher() {
    super("(B-E:GPE_DESC:CITY( I-E:GPE_DESC:CITY)*|B-E:GPE:CITY( I-E:GPE:CITY)*)");
  }
}
