package jesa.wikipedia;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.mediawiki.importer.DumpWriter;
import org.mediawiki.importer.Page;
import org.mediawiki.importer.Revision;

public abstract class LinkWriter implements DumpWriter  {
	protected static Pattern s_linkPattern = Pattern.compile(
			//"\\[\\[([^\\]]+)\\]\\]" );
			"\\[\\[([^\\]]+)\\]\\]([\\p{L}]*)" );// TODO: it seems like there are line-breaks within anchor texts... but only with this regex??

	protected Map<String, String> redirect2target;
	protected BufferedWriter bw;// TODO: Writer/OutputStream?
	protected String title;
	protected Set<String> admissableLinks;
	
	public LinkWriter(BufferedWriter aWriter, Set<String> articles, Map<String,String> redirects) {
		this.bw = aWriter;
		this.admissableLinks = articles;
		this.redirect2target = redirects;
	}
	
	protected boolean ignoreTitle(String aTitle) {
	  // Two filtering criteria:
	  // - implicit: no list of articles given - use heuristic
	  // - explicit: use list of articles
	  return 
	      (null == this.admissableLinks && 
	        TextDumpWriter.isNoArticleTitle(aTitle)) || 
	      (null != this.admissableLinks && 
	        !this.admissableLinks.contains(aTitle));
	}
	
	public void writeRevision(Revision rev) throws IOException {
		if ( !ignoreTitle(title) ) {
			writeAdmissableRevision(rev);
		}
	}
	
	public abstract void writeAdmissableRevision(Revision rev) throws IOException;
	
	public void writeStartPage(Page page) throws IOException {
		title = page.Title.toString().replace(' ', '_');
	}
	
	public void close() throws IOException {
		bw.flush();
		bw.close(); // TODO: here?		
	}
}
