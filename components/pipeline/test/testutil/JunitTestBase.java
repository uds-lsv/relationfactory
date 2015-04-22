package testutil;

import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;

import org.junit.After;
import org.junit.Rule;
import org.junit.rules.TemporaryFolder;

public class JunitTestBase {
  @Rule  
  public TemporaryFolder folder = new TemporaryFolder();
  
  private PrintStream oldStdout;
  
  protected File createFileWithContent(String content) throws IOException {
    File f = folder.newFile();
    BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(
        new FileOutputStream(f), "UTF-8"));
    try {
      writer.write(content);
    } finally {
      writer.close();
    }
    return f;
  }
  
  protected OutputStream redirectStdOut() {
    if (oldStdout != null)  return System.out;
    oldStdout = System.out;
    OutputStream captureStream = new ByteArrayOutputStream();
    System.setOut(new PrintStream(captureStream));
    return captureStream;
  }
  
  @After
  public void resetStdOut() {
    if (oldStdout == null)  return;
    System.out.close();
    System.setOut(oldStdout);
    oldStdout = null;
  }
}
