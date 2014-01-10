package entity_expansion;


import java.util.Set;
import java.util.HashSet;
import java.util.regex.Pattern;


public class DateMatcher {
  // TODO: abbreviations
  // "(Jan(uary)?|Feb(ruary)?|Mar(ch)?|Apr(il)?|May|June?|July?|Aug(ust)?|" + 
  // "Sep(tember)?|Oct(ober)?|Nov(ember)?|Dec(ember)?) "
  
  /*
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
//  static Pattern strictCanonicalPattern_OLD = Pattern.compile(
//      "(([1]?\\d)/(([123]?\\d)/)?)?((17|18|19|20)\\d\\d)|([1]?\\d)/([123]?\\d)");
  static Pattern canonicalPattern_OLD = Pattern.compile(
      "(([1]?\\d)/(([123]?\\d)/)?)?((\\d\\d)\\d\\d)|([1]?\\d)/([123]?\\d)");
  static Pattern canonicalPattern_TIMEX2 = Pattern.compile(
      "(\\d\\d|XX)(\\d\\d|XX)-(\\d\\d|XX)-(\\d\\d|XX)");
  static Pattern canonicalFreebasePattern = Pattern.compile(
      "(\\d\\d\\d\\d)(-(\\d\\d)(-(\\d\\d))?)?");
  static String[] months = new String[]{"January","February","March","April",
    "May","June","July","August","September","October","November","December"};
*/
  
  
  static Pattern monthDayYearPattern = Pattern.compile(
      "([Oo]n )?(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)(\\.?|\\w*) " + 
      "([0123]?\\d)(st|nd|th)? ?, (\\d\\d\\d\\d)");
  static Pattern dayMonthYearPattern = Pattern.compile(
      "([Oo]n )?([0123]?\\d)(st|nd|th)? " + 
      "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)(\\.?|\\w*) " +
      " (\\d\\d\\d\\d)");
  static Pattern yearPattern = Pattern.compile(
      "([Ii]n |[Ii]n the year)?((17|18|19|20)\\d\\d)");
  static Pattern monthYearPattern = Pattern.compile(
      "([Ii]n )?(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)(\\.?|\\w*) " +
      " (\\d\\d\\d\\d)");
  static Pattern monthDayPattern = Pattern.compile(
      "([Oo]n )?(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)(\\.?|\\w*) " + 
      "([0123]?\\d)(st|nd|th)?");
  static Pattern dayMonthPattern = Pattern.compile(
      "([Oo]n )?([0123]?\\d)(st|nd|th)? " + 
      "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)(\\.?|\\w*)");
  static Pattern monthPattern = Pattern.compile(
      "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)(\\.?|\\w*)");
//  static Pattern strictCanonicalPattern_OLD = Pattern.compile(
//      "(([1]?\\d)/(([123]?\\d)/)?)?((17|18|19|20)\\d\\d)|([1]?\\d)/([123]?\\d)");
//  static Pattern canonicalPattern_OLD = Pattern.compile(
//      "(([1]?\\d)/(([123]?\\d)/)?)?((\\d\\d)\\d\\d)|([1]?\\d)/([123]?\\d)");
  static Pattern canonicalPattern_TIMEX2 = Pattern.compile(
      "(\\d\\d|XX)(\\d\\d|XX)-(\\d\\d|XX)-(\\d\\d|XX)");
  static Pattern canonicalFreebasePattern = Pattern.compile(
      "(\\d\\d\\d\\d)(-(\\d\\d)(-(\\d\\d))?)?");
  static String[] months = new String[]{"Jan","Feb","Mar","Apr",
    "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"};
  static String[] monthsLong = new String[]{"January","February","March","April",
    "May","June","July","August","September","October","November","December"};
  
  /*
  public static boolean patternMatches(String candidate) {    
    return monthDayYearPattern.matcher(candidate).matches() ||
        dayMonthYearPattern.matcher(candidate).matches() ||
        yearPattern.matcher(candidate).matches() ||
        monthYearPattern.matcher(candidate).matches() ||
        monthDayPattern.matcher(candidate).matches() ||
        dayMonthPattern.matcher(candidate).matches() ||
        strictCanonicalPattern.matcher(candidate).matches();    
  }*/
  
  /*
   * This is a heuristic to convert free text dates into TIMEX2 format.
   */
  public static String normalize_TIMEX2(String candidate) {
    String day = null;
    String year = null;
    String month = null;
    java.util.regex.Matcher m;
    
    m = monthPattern.matcher(candidate);   
    if (m.matches()) {
      month = m.group(1);
    }
    m = canonicalFreebasePattern.matcher(candidate);
    if (m.matches()) {
      year = m.group(1);
      month = m.group(3);
      day = m.group(5);
    }
    m = monthDayYearPattern.matcher(candidate);
    if (m.matches()) {
      month = m.group(2);
      day = m.group(4);
      year = m.group(6);
    }
    m = dayMonthYearPattern.matcher(candidate);
    if (m.matches()) {
      day = m.group(2);
      month = m.group(4);
      year = m.group(6);
    }
    m = yearPattern.matcher(candidate);
    if (m.matches()) {
      year = m.group(2);
    }
    m = monthYearPattern.matcher(candidate);
    if (m.matches()) {
      month = m.group(2);
      year = m.group(4);
    }
    m = monthDayPattern.matcher(candidate);
    if (m.matches()) {
      month = m.group(2);
      day = m.group(4);
    }
    m = dayMonthPattern.matcher(candidate);   
    if (m.matches()) {
      day = m.group(2);
      month = m.group(5);
    }
    m = canonicalPattern_TIMEX2.matcher(candidate);
    if (m.matches()) {
      String yearCentury = m.group(1);
      String yearDecimal = m.group(2);
      year = yearCentury + yearDecimal;
      month = m.group(3);
      day = m.group(5);
    }
    // obtain normalized forms
    // Numbers for months
    if (month != null) {
      for (int i = 0; i < months.length; ++i) {
        if (month.startsWith(months[i])) {
          month = Integer.toString(i + 1);
          break;
        }
      }
    }
    // Invalid year
    if (null == year || year.length() > 4) {
      year = "XXXX";
    }
    // Only decimal part given - fill with XX
    // TODO: could also be filled with "1900"
    if (year.length() < 4) {
      year = "XXXX".substring(0, 4 - year.length()) + year;
    }
    // Invalid month
    if (null == month || month.length() > 2 || month.length() == 0) {
      month = "XX";
    } else if (month.length() == 1) {
      month = "0" + month;
    }
    // Invalid day
    if (null == day || day.length() > 2 || day.length() == 0) {
      day = "XX";
    } else if (day.length() == 1) {
      day = "0" + day;
    }
    return year + "-" + month + "-" + day;
  }
  
  public static Set<String> alternatives(String matchString) {
    Set<String> retSet = new HashSet<String>();
    retSet.add(matchString);
    matchString = normalize_TIMEX2(matchString);
    java.util.regex.Matcher m = canonicalPattern_TIMEX2.matcher(matchString);
    if (m.matches()) {
      String yearCentury = m.group(1);
      String yearDecimal = m.group(2);
      String year = (yearCentury + yearDecimal).replace("X", "");
      String month = m.group(3).replace("X", "");
      String day = m.group(5).replace("X", "");
      if (year.isEmpty()) {
        year = null;
      }
      if (month.isEmpty()) {
        month = null;
      }
      if (day.isEmpty()) {
        day = null;
      }
      
      if (month != null) {
        int monthIdx = Integer.parseInt(month) - 1;
        month = monthsLong[monthIdx] ;
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

}
