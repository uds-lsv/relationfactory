package jesa.wikipedia;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.mediawiki.importer.DumpWriter;
import org.mediawiki.importer.Page;
import org.mediawiki.importer.Revision;
import org.mediawiki.importer.Siteinfo;

/**
 * 
 * This writes the redirect sources with their destinations to a text file.
 * TODO: handling linking to subsections like "TIE_bomber TIE_fighter#Other_TIE_craft"
 * 
 * @author Benjamin Roth, beroth-AT-coli.uni-saarland.de, 2009
 *
 */
public class RedirectDumpWriter implements DumpWriter {
	// TODO: include page to filter out more than redirects?
	private BufferedWriter bw;// TODO: Writer/OutputStream?
	private Map<String , Integer> term2df;
	private int nDocs;
	private String title;
	
	static Pattern redirectTargetPattern = Pattern.compile(
			"\\[\\[([^\\]]+)\\]\\]" );
	
	public RedirectDumpWriter(BufferedWriter aWriter) {
		this.bw = aWriter;
	}


	public void close() throws IOException {
		bw.close(); // TODO: here?		
	}

	public void writeEndPage() throws IOException {
	}

	public void writeEndWiki() throws IOException {
		// console output:
		// line break after last dot
		System.out.println();
	}

	public void writeRevision(Revision rev) throws IOException {
		if ( rev.Text.startsWith("#REDIRECT") || 
				rev.Text.startsWith("#redirect") ) {
			
			
			Matcher m = redirectTargetPattern.matcher( rev.Text );
			
			if ( m.find() ){
				bw.append( title );
				bw.append(' ');
				bw.append( m.group(1).replace(' ', '_') );
				bw.append('\n');
			}
				
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

	public Map<String, Integer> getDf() {
		return this.term2df;
	}

	public int getNDocs() {
		return nDocs;
	}

}