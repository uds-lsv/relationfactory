package indexir;

import java.io.BufferedInputStream;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;

import jesa.wikipedia.SgmlWithSubsectionsDumpWriter;
import jesa.wikipedia.TextWithSubsectionsDumpWriter;

import org.mediawiki.importer.XmlDumpReader;

public class WikipediaToSgmlDump {

  /**
   * @param args
   * @throws IOException 
   * @throws UnsupportedEncodingException 
   */
  public static void main(String[] args) throws IOException {
    String inputFN = args[0];
    String outputDir = args[1];

    System.out.println("Dump dump...");
    SgmlWithSubsectionsDumpWriter cw = new SgmlWithSubsectionsDumpWriter(outputDir, 1000);
    XmlDumpReader xr = new XmlDumpReader(new BufferedInputStream(new FileInputStream(inputFN)), cw);
    xr.readDump();
    System.out.println("...done!");
  }

}
