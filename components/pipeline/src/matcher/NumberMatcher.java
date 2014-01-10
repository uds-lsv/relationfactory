package matcher;

import java.util.regex.Pattern;

public class NumberMatcher extends TextPatternMatcher {
  // TODO: normalization, expansion.
  static String numRegex = 
      "(one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|twenty" + 
      "thirty|fourty|fifty|sixty|seventy|eighty|ninty|hundred|\\d+([\\., \\d]+\\d)?)" + 
      " ?(hundred|thousand|million|billion)?";
  
  public NumberMatcher() {
    super(numRegex);
  }
  
  public static void main(String[] args) {
    Pattern pattern = Pattern.compile(numRegex);
    System.out.println(pattern.matcher("three hundred").matches());
    System.out.println(pattern.matcher("123.45 million").matches());
    System.out.println(pattern.matcher("123,45 million").matches());
    System.out.println(pattern.matcher("123 , 45").matches());
    System.out.println(pattern.matcher("twelve").matches());
    System.out.println(pattern.matcher("12").matches());
    System.out.println(pattern.matcher("lkj").matches());
    System.out.println(pattern.matcher("4x3").matches());
    System.out.println(pattern.matcher("4-3 6-1").matches());
  }
}
