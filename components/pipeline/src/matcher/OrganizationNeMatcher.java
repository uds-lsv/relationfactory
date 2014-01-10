package matcher;

public class OrganizationNeMatcher extends NePatternMatcher {

  private static OrganizationNeMatcher instance = null;
  public static OrganizationNeMatcher getInstance() {
    if(instance == null) {
       instance = new OrganizationNeMatcher();
    }
    return instance;
  }
  public OrganizationNeMatcher() {
    super("B-E:ORGANIZATION:[A-Z_]+( I-E:ORGANIZATION:[A-Z_]+)*");
  }
}
