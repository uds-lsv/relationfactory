package jesa.wikipedia;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.mediawiki.importer.Revision;
import org.mediawiki.importer.Siteinfo;

/**
 * 
 * This file holds a file describing the graph: enwiki.graph
 * 
 * @author beroth
 *
 */
// TODO: generate the same files from output from LinkDumpWriter (enwiki.lnks.redir)
public class LinkGraphWriter extends LinkWriter {
	
	Map<String,Integer> art2id;
	Pattern linkDiv = Pattern.compile("\\|");
	DataOutputStream dos;
	
	public LinkGraphWriter(OutputStream aDos, List<String> articles, Map<String,String> redirects) {
		super(null, new HashSet<String>(), redirects);
		admissableLinks.addAll(articles);
		art2id = new HashMap<String, Integer>(redirects.size());
		int cnt=0;
		for ( String art : articles){
			art2id.put(art, cnt);
			cnt++;
		}
		dos = new DataOutputStream(aDos);
	}

	@Override
	public void writeAdmissableRevision(Revision rev) throws IOException {
		Matcher linkMatcher = s_linkPattern.matcher( rev.Text );
		
		dos.writeInt( art2id.get(title) );
		
		Set<Integer> artSet = new HashSet<Integer>();
		
		while( linkMatcher.find() )
		{// TODO: also include strings
			String linkText = linkMatcher.group( 1 );
			if( linkText != null ){
				String[] linkparts = linkDiv.split(linkText, -1);
					//linkText.split("\\|",-1);
				String link = linkparts[0].replace(' ', '_');
				
				String normalForm = link;
				// TODO: recursive checking for normal-forms, maybe by preprocessing redirect table
				if ( this.redirect2target.containsKey(link) ){
					//TODO: debugging
					normalForm = redirect2target.get(link);
				}
				
				if ( this.admissableLinks.contains( normalForm ) ){
					artSet.add( art2id.get(normalForm) );
				}
			}
		}
		
		dos.writeInt( artSet.size() );
		for ( int id : artSet )
			dos.writeInt( id );
	}

	public void writeEndPage() throws IOException {
		// TODO Auto-generated method stub
	}

	public void writeEndWiki() throws IOException {
		// TODO Auto-generated method stub		
	}

	public void writeSiteinfo(Siteinfo info) throws IOException {
		// TODO Auto-generated method stub		
	}

	public void writeStartWiki() throws IOException {
		// TODO Auto-generated method stub
	}
	
	public void close() throws IOException {
		dos.flush();
		dos.close(); // TODO: here?		
	}

}
