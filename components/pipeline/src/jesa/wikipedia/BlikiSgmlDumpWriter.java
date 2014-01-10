package jesa.wikipedia;

import info.bliki.wiki.filter.PlainTextConverter;
import info.bliki.wiki.model.WikiModel;

import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.net.URLEncoder;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringEscapeUtils;
import org.mediawiki.importer.DumpWriter;
import org.mediawiki.importer.Page;
import org.mediawiki.importer.Revision;
import org.mediawiki.importer.Siteinfo;

public class BlikiSgmlDumpWriter implements DumpWriter {
  
  
  static WikiModel wikiModel = new WikiModel("", "");
  static PlainTextConverter textConverter = new PlainTextConverter();
  
  String titleEscaped;
  String basedir;
  BufferedWriter bw = null;
  final int articlesPerFile;
  int articlesToWrite;
  boolean skipPage = false;
  
  private static final Pattern SUBHEADER_PATTERN = Pattern.compile("(\\r?\\n|\\r) *=+([^=\\r\\n]+)=+ *(\\r?\\n|\\r)");
  private static final Pattern LINK_PATTERN = Pattern.compile("\\{\\{[^\\}]*\\}\\}");
  
  public BlikiSgmlDumpWriter(String basedir, int articlesPerFile) {
    bw = null;
    this.articlesPerFile = articlesPerFile;
    this.basedir = basedir;
    articlesToWrite = 0;
  }
  
  public void close() throws IOException {
    bw.close();
  }
  // TODO: deal with the following unwanted pages: "File*.jpg" "List of*"
  public void writeStartPage(Page page) throws IOException {
    String title = page.Title.toString();
    if (TextDumpWriter.isNoArticleTitle(title)) {
      skipPage = true;
    } else {
      skipPage = false;
      titleEscaped = URLEncoder.encode(title, "UTF-8");//.replaceAll(" ", "_");
      if (articlesToWrite == 0) {
        String outputFN = basedir + "/" + titleEscaped;
        bw = new BufferedWriter( new OutputStreamWriter(
            new FileOutputStream( outputFN ),"UTF-8"));
        articlesToWrite = articlesPerFile;
      }
      articlesToWrite -= 1;
    }
  }
  
  public void writeEndPage() throws IOException {
    if (skipPage) {
      return;
    }
    if (articlesToWrite == 0) {
      bw.close();
    }
  }
  
  public void writeRevision(Revision rev) throws IOException {
    if (skipPage
        || rev.Text.startsWith("#REDIRECT")
        || rev.Text.startsWith("#redirect")) {
      return;
    }
    /*
    <DOC ID="Article_Name">
    <TEXT>
    <P>subheading</P>
    <P>Text</P>
    ...
    </TEXT>
    </DOC>
         */

    bw.append("<DOC ID=\"" + titleEscaped + "\">");
    bw.newLine();
    bw.append("<TEXT>");
    bw.newLine();
    
    // TODO
    Matcher sectionMatcher = SUBHEADER_PATTERN.matcher(rev.Text);
    int textStart = 0;
    while (sectionMatcher.find()) {
      String xmlText = rev.Text.substring(textStart, sectionMatcher.start());
      writeSgmlOfXml(xmlText, bw);
      
      textStart = sectionMatcher.end();
      
      String secTitle = 
          sectionMatcher.group(2).replace("[[", "").replace("]]", "").trim();
      
      writeSgmlOf(secTitle, bw);
    }
    
    String xmlText = rev.Text.substring(textStart);

    writeSgmlOfXml(xmlText, bw);
    bw.append("</TEXT>");      
    bw.newLine();
    bw.append("</DOC>");
    bw.newLine();
  }

  private static void writeSgmlOf(String plainText, BufferedWriter bw) throws IOException {
    if (!plainText.isEmpty()) {
      bw.append("<P>");
      bw.newLine();        
      bw.append(StringEscapeUtils.escapeXml(plainText));
      bw.newLine();
      bw.append("</P>");
      bw.newLine();
    }
  }
  
  private static void writeSgmlOfXml(String xmlText, BufferedWriter bw) throws IOException {
    
    String plainText;
    
    try {
      plainText = wikiModel.render(textConverter, xmlText);
    } catch (Exception e) {
      System.err.println("Exception when parsing wiki text:");
      System.err.println(e);
      System.err.println("Text:");
      System.err.println(xmlText);
      return;
    }
    
    plainText = LINK_PATTERN.matcher(plainText).replaceAll(" ").trim().
        replaceAll("\\s+", " ");;
    if (!plainText.isEmpty()) {
      bw.append("<P>");
      bw.newLine();        
      bw.append(StringEscapeUtils.escapeXml(plainText));
      bw.newLine();
      bw.append("</P>");
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