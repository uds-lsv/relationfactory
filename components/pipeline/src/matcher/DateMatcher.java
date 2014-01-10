package matcher;

import java.util.Set;
import java.util.HashSet;
import java.util.regex.Pattern;

import rerac.protos.Corpus.Document;
import util.DocumentExtractor;

public class DateMatcher extends Matcher {
  // TODO: abbreviations
  // "(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|June?|July?|Aug(ust)?|" + 
  // "Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?) "
  static Pattern monthDayYearPattern = Pattern.compile(
      "([Oo]n )?(January|February|March|April|May|June|July|August|" + 
      "September|October|November|December) " + 
      "([0123]?\\d)(st|nd|th)? ?, (\\d\\d\\d\\d)");
  static Pattern dayMonthYearPattern = Pattern.compile(
      "([Oo]n )?([0123]?\\d)(st|nd|th)? " + 
      "(January|February|March|April|May|June|July|August|" + 
      "September|October|November|December)" +
      " (\\d\\d\\d\\d)");
  static Pattern yearPattern = Pattern.compile(
      "([Ii]n |[Ii]n the year)?((17|18|19|20)\\d\\d)");
  static Pattern monthYearPattern = Pattern.compile(
      "([Ii]n )?(January|February|March|April|May|June|July|August|" + 
      "September|October|November|December)" +
      " (\\d\\d\\d\\d)");
  static Pattern monthDayPattern = Pattern.compile(
      "([Oo]n )?(January|February|March|April|May|June|July|August|" + 
      "September|October|November|December) " + 
      "([0123]?\\d)(st|nd|th)?");
  static Pattern dayMonthPattern = Pattern.compile(
      "([Oo]n )?([0123]?\\d)(st|nd|th)? " + 
      "(January|February|March|April|May|June|July|August|" + 
      "September|October|November|December)");
  static Pattern strictCanonicalPattern = Pattern.compile(
      "(([1]?\\d)/(([123]?\\d)/)?)?((17|18|19|20)\\d\\d)|([1]?\\d)/([123]?\\d)");
  static Pattern canonicalPattern = Pattern.compile(
      "(([1]?\\d)/(([123]?\\d)/)?)?((\\d\\d)\\d\\d)|([1]?\\d)/([123]?\\d)");
  static Pattern canonicalFreebasePattern = Pattern.compile(
      "(\\d\\d\\d\\d)(-(\\d\\d)(-(\\d\\d))?)?");
  static String[] months = new String[]{"January","February","March","April",
    "May","June","July","August","September","October","November","December"};

  public boolean patternMatches(String candidate) {    
    return monthDayYearPattern.matcher(candidate).matches() ||
        dayMonthYearPattern.matcher(candidate).matches() ||
        yearPattern.matcher(candidate).matches() ||
        monthYearPattern.matcher(candidate).matches() ||
        monthDayPattern.matcher(candidate).matches() ||
        dayMonthPattern.matcher(candidate).matches() ||
        strictCanonicalPattern.matcher(candidate).matches();    
  }
  
  @Override
  public boolean isMatch(Document doc, int start, int end) {
    String candidate = DocumentExtractor.textFromTokens(doc, start, end);
    return patternMatches(candidate);
  }
  
  private String normalize(String candidate) {
    String day = null;
    String year = null;
    String month = null;
    java.util.regex.Matcher m;
    m = canonicalFreebasePattern.matcher(candidate);
    if (m.matches()) {
      year = m.group(1);
      month = m.group(3);
      day = m.group(5);
    }
    m = monthDayYearPattern.matcher(candidate);
    if (m.matches()) {
      month = m.group(2);
      day = m.group(3);
      year = m.group(5);
    }
    m = dayMonthYearPattern.matcher(candidate);
    if (m.matches()) {
      day = m.group(2);
      month = m.group(4);
      year = m.group(5);
    }
    m = yearPattern.matcher(candidate);
    if (m.matches()) {
      year = m.group(2);
    }
    m = monthYearPattern.matcher(candidate);
    if (m.matches()) {
      month = m.group(2);
      year = m.group(3);
    }
    m = monthDayPattern.matcher(candidate);
    if (m.matches()) {
      month = m.group(2);
      day = m.group(3);
    }
    m = dayMonthPattern.matcher(candidate);   
    if (m.matches()) {
      day = m.group(2);
      month = m.group(4);
    }
    if (month != null) {
      for (int i = 0; i < months.length; ++i) {
        if (month.equals(months[i])) {
          month = Integer.toString(i + 1);
          break;
        }
      }
    }
    // Trimming leading 0's.
    if (day != null) {
      day = Integer.toString(Integer.parseInt(day));
    }
    if (month != null && day != null && year != null) {
      return month + "/" + day + "/" + year;
    }
    if (month != null && year != null) {
      return month + "/" + year;
    }
    if (month != null && day != null) {
      return month + "/" + day;
    }
    if (year != null) {
      return year;
    }
    return candidate;
  }
  
  @Override
  public String normalize(Document doc, int start, int end) {
    String candidate = DocumentExtractor.textFromTokens(doc, start, end);
    return normalize(candidate);
  }
  
  @Override
  public Set<String> alternatives(String matchString) {
    Set<String> retSet = new HashSet<String>();
    retSet.add(matchString);
    matchString = normalize(matchString);
    java.util.regex.Matcher m = canonicalPattern.matcher(matchString);
    if (m.matches()) {
      String month = m.group(2);
      String day = m.group(4);
      String year = m.group(5);
      // Use second part of pattern if first did not match.
      if (null == month) {
        month = m.group(7);
      }
      if (null == day) {
        day = m.group(8);
      }
      // Change month number in name.
      if (month != null) {
        int monthIdx = Integer.parseInt(month) - 1;
        month = months[monthIdx] ;
      }
      // Add expanded strings.
      // TODO: "on/in" as part of expansions necessary? inconsistent with matching?
      if (month != null && day != null && year != null) {
        retSet.add(month + " " + day + ", " + year);
        retSet.add(day + " " + month + " " + year);
        //retSet.add(month + " " + day + " , " + year);
        
        // TAC system expects dates without on/in.
        //retSet.add("on " + month + " " + day + ", " + year);
        //retSet.add("on " + month + " " + day + " , " + year);
      }
      if (month != null && year != null) {
        retSet.add(month + " " + year);
        // TAC system expects dates without on/in.
        // retSet.add("in " + month + " " + year);
      }
      // TODO: Left out to avoid substrings as expansions.
      /*
      if (month != null && day != null) {
        retSet.add("on " + month + " " + day);
      }
      */
      if (year != null) {
        retSet.add(year);
        // TAC system expects dates without on/in.
        //retSet.add("in " + year);
      }
    }
    return retSet;
  }
  
  public static void main(String[] args) {
    DateMatcher dm = new DateMatcher();
    System.out.println(dm.normalize("May 12 , 1727"));
    System.out.println(dm.normalize("on May 12, 1727"));
    System.out.println(dm.normalize("Sept. 17"));
    System.out.println(dm.normalize("September 17"));
    System.out.println(dm.normalize("September 2001"));
    System.out.println(dm.normalize("in October 2001"));
    System.out.println(dm.normalize("in 1871"));
    System.out.println(dm.normalize("1871"));
    System.out.println(dm.normalize("0012"));
    System.out.println(dm.normalize("0012-12-25"));
    System.out.println(dm.alternatives("May 12 , 1727"));
    System.out.println(dm.alternatives("on May 12, 1727"));
    System.out.println(dm.alternatives("Sept. 17"));
    System.out.println(dm.alternatives("September 17"));
    System.out.println(dm.alternatives("September 2001"));
    System.out.println(dm.alternatives("in October 2001"));
    System.out.println(dm.alternatives("in 1871"));
    System.out.println(dm.alternatives("1871"));
    System.out.println(dm.alternatives("0012"));
    System.out.println(dm.alternatives("0012-12-25"));
  }

}
