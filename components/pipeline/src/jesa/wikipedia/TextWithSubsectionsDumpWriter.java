package jesa.wikipedia;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.mediawiki.importer.DumpWriter;
import org.mediawiki.importer.Page;
import org.mediawiki.importer.Revision;
import org.mediawiki.importer.Siteinfo;

import edu.uka.aifb.wikipedia.WikipediaTools;

public class TextWithSubsectionsDumpWriter implements DumpWriter {
  String title;
  BufferedWriter bw;
  
  private static final Pattern SUBHEADER_PATTERN = Pattern.compile("(\\r?\\n|\\r) *=+[^=\\r\\n]+=+ *(\\r?\\n|\\r)");
  
  public TextWithSubsectionsDumpWriter(BufferedWriter aWriter) {
    bw = aWriter;
  }
  
  public void close() throws IOException {
    bw.close();
  }
  
  public void writeStartPage(Page page) throws IOException {
    title = page.Title.toString();//.replaceAll(" ", "_");
  }
  
  public void writeEndPage() throws IOException {

  }
  
  public void writeRevision(Revision rev) throws IOException {
    
    if (!title.isEmpty() && 
        !rev.Text.startsWith("#REDIRECT") && 
        !rev.Text.startsWith("#redirect")) {

      bw.append("<DOC ID=" + title + ">");
      bw.newLine();
      
      Matcher sectionMatcher = SUBHEADER_PATTERN.matcher(rev.Text);
      
      int textStart = 0;
      while (sectionMatcher.find()) {
        String xmlText = rev.Text.substring(textStart, sectionMatcher.start());
        String plainText = WikipediaTools.extractPlainText( xmlText ).replaceAll("\\s+", " ").trim();
        
        bw.append(plainText);
        bw.newLine();
        
        textStart = sectionMatcher.end();
        
        String secTitle = 
            sectionMatcher.group().replace("[[", "").replace("]]", "").trim();
        bw.append("<SUBHEADING>");
        bw.newLine();
        bw.append(secTitle);
        bw.newLine();
        bw.append("</SUBHEADING>");
        bw.newLine();
      }
      
      String xmlText = rev.Text.substring(textStart);
      String plainText = WikipediaTools.extractPlainText( xmlText ).replaceAll("\\s+", " ").trim();

      bw.append(plainText);
      bw.newLine();
      
      bw.append("</DOC>");
      bw.newLine();
    }
  }

  public void writeEndWiki() throws IOException {
    // do nothing 
  }
  
  public void writeSiteinfo(Siteinfo info) throws IOException {
    // do nothing
  }


  public void writeStartWiki() throws IOException {
    // do nothing
  }
}
