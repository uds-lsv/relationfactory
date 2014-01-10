package matcher;

import java.util.regex.Pattern;

import rerac.protos.Corpus.Document;

public class TextPatternMatcher extends Matcher {

  Pattern pattern;
  
  public TextPatternMatcher(String neRegex) {
    pattern = Pattern.compile(neRegex);
  }
  
  public String normalize(int[] groups, String text) {
    java.util.regex.Matcher m = pattern.matcher(text);
    if (!m.matches()) {
      return text;
    }
    StringBuffer sb = new StringBuffer();
    for (int g : groups) {
      if (m.group(g) != null) {
        sb.append(m.group(g));
      }
    }
    return sb.toString();
  }
  
  @Override
  public boolean isMatch(Document doc, int start, int end) {
    String neString = util.DocumentExtractor.textFromTokens(doc, start, end);
    java.util.regex.Matcher m = pattern.matcher(neString);
    return m.matches();
  }
}


