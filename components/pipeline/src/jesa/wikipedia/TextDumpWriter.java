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

/**
 * 
 * This writes the tokenized dumping result to a text file.
 * 
 * @author Benjamin Roth, beroth-AT-coli.uni-saarland.de, 2009
 *
 */
public class TextDumpWriter implements DumpWriter {
	// TODO: include page to  filter out more than redirects?
	BufferedWriter bw;// TODO: Writer/OutputStream?
	String title;
	
	private static final Pattern LETTER_PATTERN = Pattern.compile("[\\p{L}]+");
	
	public TextDumpWriter(BufferedWriter aWriter) {
		bw = aWriter;
	}
	
	public void close() throws IOException {
		bw.close(); // TODO: here?
		
	}

	public void writeEndPage() throws IOException {
	}

	public void writeEndWiki() throws IOException {
		// do nothing
		
	}
	
	public static boolean isNoArticleTitle(String title) {
	  // Captures many unwanted cases, e.g. 'File:' - colons are omitted for 
	  // actual articles as it seems.
	  return title.contains(":") 
    || title.startsWith("List of")
    || title.isEmpty();
	}

	public void writeRevision(Revision rev) throws IOException {
		
		if (!title.isEmpty() && 
				!rev.Text.startsWith("#REDIRECT") && 
				!rev.Text.startsWith("#redirect")) {

			bw.append( title );
			
			String plainText = WikipediaTools.extractPlainText( rev.Text );		
			
			Matcher termMatcher = LETTER_PATTERN.matcher(plainText);
			
			while (termMatcher.find()) {
				bw.append(' ');
				bw.append(termMatcher.group());				
			}
			
			bw.newLine();
		}
	}

	public void writeSiteinfo(Siteinfo info) throws IOException {
		// do nothing
	}

	public void writeStartPage(Page page) throws IOException {
		title = page.Title.toString().replaceAll(" ", "_");
	}

	public void writeStartWiki() throws IOException {
		// do nothing
	}

}
