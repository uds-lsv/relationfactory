package evaluation;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

public class MergePerEmployeeMemberKey {

  /**
   * This merges the relation per:employee_of and per:member_of into 
   * per:employee_or_member_of.
   * 
   * A filler is correct for per:employee_or_member_of if it is correct for
   * per:employee_of or per:member_of.
   * 
   * @param args
   * @throws IOException 
   */
  public static void main(String[] args) throws IOException {
    // 17 SF550:per:member_of APW_ENG_20080229.0949.LDC2009T13  -1  0 PPP 0 0:0 0 0:0
    
    // Contains tuple: QID + "\t" + DOCID + "\t" + RESPONSE
    Set<String> correctKeyPerEmployeeOrMember = new HashSet<String>();
    Set<String> writtenKeyPerEmployeeOrMember = new HashSet<String>();
    
    BufferedReader br = new BufferedReader(new FileReader(args[0]));
    for (String line; (line = br.readLine()) != null;) {
      String[] parts = line.split("\t");
      String rel = parts[1].split(":", 2)[1];
      if (rel.equals("per:employee_of") || rel.equals("per:member_of")) {
        String qid = parts[1].split(":", 2)[0];
        String docid = parts[2];
        String judgment = parts[3];
        String response = parts[5];
        if (judgment.equals("1")) {
          correctKeyPerEmployeeOrMember.add(qid + "\t" + docid + "\t" + response);
        }
      } else {
        System.out.println(line);
      }
    }
    br.close();
    
    br = new BufferedReader(new FileReader(args[0]));
    for (String line; (line = br.readLine()) != null;) {
      String[] parts = line.split("\t");
      String rel = parts[1].split(":", 2)[1];
      if (rel.equals("per:employee_of") || rel.equals("per:member_of")) {
        String qid = parts[1].split(":", 2)[0];
        String docid = parts[2];
        String judgment = parts[3];
        String response = parts[5];
        String tuple = qid + "\t" + docid + "\t" + response;
        if (writtenKeyPerEmployeeOrMember.contains(tuple)) {
          continue;
        }
        if (judgment.equals("1") || !correctKeyPerEmployeeOrMember.contains(tuple)) {
          String correctedLine = line.
              replace("per:employee_of\t", "per:employee_or_member_of\t").
              replace("per:member_of\t", "per:employee_or_member_of\t");
          System.out.println(correctedLine);
          writtenKeyPerEmployeeOrMember.add(tuple);
        }
      }
    }
    br.close();
  }
}
