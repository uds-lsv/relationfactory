package matcher;

public class CountryNeMatcher extends NePatternMatcher {

  private static CountryNeMatcher instance = null;
  public static CountryNeMatcher getInstance() {
    if(instance == null) {
       instance = new CountryNeMatcher();
    }
    return instance;
  }
  public CountryNeMatcher() {
    super("(B-E:GPE:COUNTRY( I-E:GPE:COUNTRY)*|B-E:NORP:NATIONALITY( I-E:NORP:NATIONALITY)*|B-E:GPE_DESC:COUNTRY( I-E:GPE_DESC:COUNTRY)*)");
  }
}
