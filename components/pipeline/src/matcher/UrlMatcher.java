package matcher;

import java.util.Set;
import java.util.HashSet;
import java.util.regex.Pattern;

import util.DocumentExtractor;

public class UrlMatcher extends TextPatternMatcher {
  // TODO: normalization, expansion.
  static String urlRegex = 
      "(https? ?: ?//)?" +   // http://|https : //
      "([\\w\\-]+\\.)?" + // www.|translate.|myshop.
      "([\\w\\-]+\\.[a-zA-Z]{2,4})" + // google.com|mvv-muenchen.de|benjaminroth.net
      "(/[\\w\\-]+)?/?";  // /|/help|/help/
  
  public UrlMatcher() {
    super(urlRegex);
  }
  
  @Override
  public String normalize(rerac.protos.Corpus.Document doc, int start, int end) {
    String text = DocumentExtractor.textFromTokens(doc, start, end);
    return normalize(text);
  };
  
  private String normalize(String text) {
    return super.normalize(new int[]{2,3}, text);
  }
  
  public Set<String> alternatives(String text) {
    Set<String> result = new HashSet<String>();
    String normalized = normalize(text);
    result.add(normalized);
    result.add(normalized + "/");
    result.add("http : //" + normalized);
    result.add("http : //" + normalized + "/");
    return result;
  }

  public static void main(String[] args) {
    UrlMatcher um = new UrlMatcher();
    System.out.println(um.normalize("translate.google.com"));
    System.out.println(um.normalize("https : //www.google.de/"));
    System.out.println(um.normalize("http://www.google.de/"));
    System.out.println(um.normalize("www.google.de/help"));
    System.out.println(um.normalize("mvv-muenchen.de"));
    System.out.println(um.normalize("mvv-muenchen.help"));
    System.out.println(um.normalize("7.32"));
    System.out.println(um.normalize("127.0.0.1"));
    System.out.println(um.normalize("www.translate.google.com"));
    System.out.println(um.normalize("//www.google.de/"));
    
    Pattern p = Pattern.compile(urlRegex);
    java.util.regex.Matcher m = p.matcher("translate.google.com");
    System.out.println(m.matches());
    m = p.matcher("https : //www.google.de/");
    System.out.println(m.matches());  
    m = p.matcher("http://www.google.de/");
    System.out.println(m.matches());  
    m = p.matcher("www.google.de/help");
    System.out.println(m.matches());
    m = p.matcher("mvv-muenchen.de");
    System.out.println(m.matches()); 
    m = p.matcher("mvv-muenchen.help");
    System.out.println(m.matches()); 
    m = p.matcher("7.32");
    System.out.println(m.matches()); 
    m = p.matcher("127.0.0.1");
    System.out.println(m.matches());
    m = p.matcher("translate.google");
    System.out.println(m.matches());
    m = p.matcher("www.translate.google.com");
    System.out.println(m.matches());
  }
}