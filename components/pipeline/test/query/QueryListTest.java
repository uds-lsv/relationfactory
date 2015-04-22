package query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.apache.log4j.Level;
import org.junit.BeforeClass;
import org.junit.Test;

import query.QueryList.Query;
import testutil.JunitTestBase;

/*
 * Beware: Integration test, interacting with the FS. Might be slow.
 */
public class QueryListTest extends JunitTestBase {
  
  @BeforeClass
  public static void setUpClass() throws Exception {
    QueryList.logger.setLevel(Level.ALL);
  }
  
  @Test
  public void testCreateQueryList() throws IOException {
    File xmlFile1 = createFileWithContent("<?xml version='1.0' encoding='UTF-8' ?>\n" + 
    		"<kbpslotfill>\n" + 
    		"  <query id=\"SF_ENG_001\">\n" + 
    		"    <name>Juanita Millender-McDonald</name>\n" + 
    		"    <docid>NYT_ENG_20080101.0037.LDC2009T13</docid>\n" + 
    		"    <beg>3998</beg>\n" + 
    		"    <end>4023</end>\n" + 
    		"    <enttype>PER</enttype>\n" + 
    		"    <nodeid>E0046379</nodeid>\n" + 
    		"    <ignore>per:date_of_birth per:city_of_birth per:date_of_death per:city_of_death per:religion per:stateorprovince_of_birth per:stateorprovince_of_death per:age</ignore>\n" + 
    		"  </query>\n" + 
    		"  <query id=\"SF_ENG_002\">\n" + 
    		"    <name>Paul Gillmor</name>\n" + 
    		"    <docid>NYT_ENG_20080101.0037.LDC2009T13</docid>\n" + 
    		"    <beg>4081</beg>\n" + 
    		"    <end>4092</end>\n" + 
    		"    <enttype>PER</enttype>\n" + 
    		"    <nodeid>E0526932</nodeid>\n" + 
    		"    <ignore>per:date_of_birth per:city_of_birth per:date_of_death per:city_of_death per:religion per:stateorprovince_of_birth per:stateorprovince_of_death per:age</ignore>\n" + 
    		"  </query>\n" + 
    		"  </kbpslotfill>");
    
    QueryList q = new QueryList(xmlFile1.getCanonicalPath());
    assertEquals("Number of queries mismatch", 2, q.getQueries().size());
    Query q1 = q.getQueryById("SF_ENG_001");
    assertTrue("Query not found", q1 != null);
    Query q2 = q.getQueryById("SF_ENG_002");
    assertTrue("Query not found", q2 != null);
    assertEquals("Query info missing", "SF_ENG_001", 
        q1.getId());
    assertEquals("Query info missing", "SF_ENG_002", 
        q2.getId());
    assertEquals("Query info missing", "PER", 
        q2.getEnttype());
    assertEquals("Query info missing: docid", "NYT_ENG_20080101.0037.LDC2009T13", 
        q1.getDocId());
    assertEquals("Query info missing", "Paul Gillmor", 
        q2.getName());
    assertEquals("Query ignore list wrong", 8, 
        q2.getIgnore().size());
    assertTrue("Query ignore list wrong", 
        q2.getIgnore().contains("per:date_of_birth"));
  }
  
  @Test
  public void testCreateQueryListExpanded() throws IOException {
    File xmlFile1 = createFileWithContent("<?xml version='1.0' encoding='UTF-8'?>\n" + 
    		"<kbpslotfill>\n" + 
    		"<query id=\"SF_ENG_001\">\n" + 
    		" <name>Juanita Millender-McDonald</name>\n" + 
    		" <enttype>PER</enttype>\n" + 
    		" <ignore>per:date_of_birth per:city_of_birth per:date_of_death per:city_of_death per:religion per:stateorprovince_of_birth per:stateorprovince_of_death per:age</ignore>\n" + 
    		" <alias>Juanita McDonald</alias>\n" + 
    		" <alias>Millender-McDonald</alias>\n" + 
    		" <rel listtype=\"true\">per:alternate_names</rel>\n" + 
    		" <rel listtype=\"true\">per:charges</rel>\n" + 
    		" <rel listtype=\"true\">per:children</rel>\n" + 
    		" <rel listtype=\"true\">per:cities_of_residence</rel>\n" + 
    		" <rel listtype=\"true\">per:countries_of_residence</rel>\n" + 
    		" <rel listtype=\"true\">per:employee_of</rel>\n" + 
    		" <rel listtype=\"true\">per:member_of</rel>\n" + 
    		" <rel listtype=\"true\">per:origin</rel>\n" + 
    		" <rel listtype=\"true\">per:other_family</rel>\n" + 
    		" <rel listtype=\"true\">per:parents</rel>\n" + 
    		" <rel listtype=\"true\">per:schools_attended</rel>\n" + 
    		" <rel listtype=\"true\">per:siblings</rel>\n" + 
    		" <rel listtype=\"true\">per:spouse</rel>\n" + 
    		" <rel listtype=\"true\">per:statesorprovinces_of_residence</rel>\n" + 
    		" <rel listtype=\"true\">per:title</rel>\n" + 
    		" <rel listtype=\"false\">per:cause_of_death</rel>\n" + 
    		" <rel listtype=\"false\">per:country_of_birth</rel>\n" + 
    		" <rel listtype=\"false\">per:country_of_death</rel>\n" + 
    		"<docid>NYT_ENG_20080101.0037.LDC2009T13</docid>\n" + 
    		"<beg>3998</beg>\n" + 
    		"<end>4023</end>\n" + 
    		"<nodeid>E0046379</nodeid>\n" + 
    		"</query>\n" + 
    		"<query id=\"SF_ENG_002\">\n" + 
    		" <name>Paul Gillmor</name>\n" + 
    		" <enttype>PER</enttype>\n" + 
    		" <ignore>per:date_of_birth per:city_of_birth per:date_of_death per:city_of_death per:religion per:stateorprovince_of_birth per:stateorprovince_of_death per:age</ignore>\n" + 
    		" <alias>Paul E. Gillmor</alias>\n" + 
    		" <alias>Gillmor</alias>\n" + 
    		" <rel listtype=\"true\">per:alternate_names</rel>\n" + 
    		" <rel listtype=\"true\">per:charges</rel>\n" + 
    		" <rel listtype=\"true\">per:children</rel>\n" + 
    		" <rel listtype=\"true\">per:cities_of_residence</rel>\n" + 
    		" <rel listtype=\"true\">per:countries_of_residence</rel>\n" + 
    		" <rel listtype=\"true\">per:employee_of</rel>\n" + 
    		" <rel listtype=\"true\">per:member_of</rel>\n" + 
    		" <rel listtype=\"true\">per:origin</rel>\n" + 
    		" <rel listtype=\"true\">per:other_family</rel>\n" + 
    		" <rel listtype=\"true\">per:parents</rel>\n" + 
    		" <rel listtype=\"true\">per:schools_attended</rel>\n" + 
    		" <rel listtype=\"true\">per:siblings</rel>\n" + 
    		" <rel listtype=\"true\">per:spouse</rel>\n" + 
    		" <rel listtype=\"true\">per:statesorprovinces_of_residence</rel>\n" + 
    		" <rel listtype=\"true\">per:title</rel>\n" + 
    		" <rel listtype=\"false\">per:cause_of_death</rel>\n" + 
    		" <rel listtype=\"false\">per:country_of_birth</rel>\n" + 
    		" <rel listtype=\"false\">per:country_of_death</rel>\n" + 
    		"<docid>NYT_ENG_20080101.0037.LDC2009T13</docid>\n" + 
    		"<beg>4081</beg>\n" + 
    		"<end>4092</end>\n" + 
    		"<nodeid>E0526932</nodeid>\n" + 
    		"</query>\n" + 
    		"</kbpslotfill>");
    
    QueryList q = new QueryList(xmlFile1.getCanonicalPath());
    Query q1 = q.getQueryById("SF_ENG_001");
    assertTrue("Query not found", q1 != null);
    assertEquals("Number of queries mismatch", 2, q.getQueries().size());
    assertEquals("List Relation list: wrong size.", 15, 
        q1.getListRelations().size());
    assertEquals("Single Relation list: wrong size.", 3, 
        q1.getSingleRelations().size());
    assertEquals("Alias list: wrong size.", 2, 
        q1.getAliases().size());
  }
  
  @Test
  public void testCreateQueryListBroken() throws IOException {
    File xmlFile1 = createFileWithContent("<?xml version='1.0' encoding='UTF-8' ?>\n" + 
    		"<!DOCTYPE kbpslotfill [\n" + 
    		"<!ELEMENT kbpslotfill (query+)>\n" + 
    		"<!ELEMENT query   (name, docid, beg, end, enttype, nodeid, ignore?)>\n" + 
    		"<!ELEMENT name   (#PCDATA)>\n" + 
    		"<!ELEMENT docid   (#PCDATA)>\n" + 
    		"<!ELEMENT beg   (#PCDATA)>\n" + 
    		"<!ELEMENT end   (#PCDATA)>\n" + 
    		"<!ELEMENT enttype (#PCDATA)>\n" + 
    		"<!ELEMENT nodeid (#PCDATA)>\n" + 
    		"<!ELEMENT ignore (#PCDATA)>\n" + 
    		"<!ATTLIST query id ID #REQUIRED>\n" + 
    		"]>\n" + 
    		"<kbpslotfill>\n" + 
    		"  <query>\n" + 
    		"    <!-- name is missing here, so is id -->\n" + 
    		"    <docid>NYT_ENG_20080101.0037.LDC2009T13</docid>\n" + 
    		"    <beg>3998</beg>\n" + 
    		"    <end>4023</end>\n" + 
    		"    <enttype>PER</enttype>\n" + 
    		"    <nodeid>E0046379</nodeid>\n" + 
    		"    <ignore>per:date_of_birth per:city_of_birth per:date_of_death per:city_of_death per:religion per:stateorprovince_of_birth per:stateorprovince_of_death per:age</ignore>\n" + 
    		"  </query>\n" + 
    		"  <query id=\"SF_ENG_002\">\n" + 
    		"    <name>Paul Gillmor</name>\n" + 
    		"    <docid>NYT_ENG_20080101.0037.LDC2009T13</docid>\n" + 
    		"    <beg>4081</beg>\n" + 
    		"    <end>4092</end>\n" + 
    		"    <enttype>PER</enttype>\n" + 
    		"    <nodeid>E0526932</nodeid>\n" + 
    		"    <ignore>per:date_of_birth per:city_of_birth per:date_of_death per:city_of_death per:religion per:stateorprovince_of_birth per:stateorprovince_of_death per:age</ignore>\n" + 
    		"  </query>\n" + 
    		"  </kbpslotfill>\n" + 
    		"");
    
    QueryList q = new QueryList(xmlFile1.getCanonicalPath());
    assertEquals("Number of queries mismatch", 1, q.getQueries().size());
  }

}
