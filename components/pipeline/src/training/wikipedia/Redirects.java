package training.wikipedia;

import java.io.BufferedInputStream;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;

import jesa.wikipedia.RedirectDumpWriter;

import org.mediawiki.importer.XmlDumpReader;

public class Redirects {

  /**
   * @param args
   * @throws IOException 
   * @throws UnsupportedEncodingException 
   */
  public static void main(String[] args) throws IOException {
    String wikiXmlFn = args[0];
    String redirectFn = args[1];

    System.out.println("Dump dump...");
    BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(
        new FileOutputStream(redirectFn), "UTF-8"));
    RedirectDumpWriter cw = new RedirectDumpWriter(bw);
    XmlDumpReader xr = new XmlDumpReader(new BufferedInputStream(new FileInputStream(wikiXmlFn)), cw);
    xr.readDump();
    System.out.println("...done!");
  }

}
