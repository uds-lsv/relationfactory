package training.wikipedia;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

import jesa.wikipedia.LinkDumpWriter;
import jesa.wikipedia.RedirectDumpWriter;

import org.mediawiki.importer.XmlDumpReader;

public class LinkText {

  /**
   * @param args
   * @throws IOException 
   * @throws UnsupportedEncodingException 
   */
  public static void main(String[] args) throws IOException {
    String wikiXmlFn = args[0];
    String redirectsFn = args[1];
    
    Map<String, String> titleToRedirection = new HashMap<String, String>();

    BufferedReader br = new BufferedReader(new InputStreamReader(
        new FileInputStream(redirectsFn), "UTF-8"));
    for (String line; (line = br.readLine()) != null;) {
      if (line.isEmpty()) {
        continue;
      }
      String[] titleRedirect = line.split(" ");
      if (titleRedirect.length != 2) {
        System.err.println("line must have two white-space separated columns, found " + titleRedirect.length);
        System.err.println(line);
        continue;
      }
      /*if (titleRedirect.length != 2) {
        throw new IllegalArgumentException(
            "line must have two white-space separated columns, found " + titleRedirect.length);
      }*/
      titleToRedirection.put(titleRedirect[0], titleRedirect[1]);
      
    }
    br.close();
    BufferedWriter bw = new BufferedWriter(
        new OutputStreamWriter(System.out, Charset.forName("UTF-8").newEncoder()));
    LinkDumpWriter cw = new LinkDumpWriter(bw, null, titleToRedirection);
    XmlDumpReader xr = new XmlDumpReader(new BufferedInputStream(new FileInputStream(wikiXmlFn)), cw);
    xr.readDump();
    bw.close();
  }

}
