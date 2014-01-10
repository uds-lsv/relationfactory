package jesa.wikipedia;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.mediawiki.importer.Revision;
import org.mediawiki.importer.Siteinfo;

// TODO: handling linking to subsections like "TIE_bomber TIE_fighter#Other_TIE_craft"
public class LinkDumpWriter extends LinkWriter /*implements DumpWriter*/ {
	// TODO: configure
  //boolean doLowercase = true;
  
	static Pattern whitespaces = Pattern.compile("[\\w]+");
	
	 public LinkDumpWriter(BufferedWriter aWriter) {
	    super(aWriter, null, new HashMap<String,String>() );
	  }
	
	public LinkDumpWriter(BufferedWriter aWriter, Set<String> articles) {
		super(aWriter, articles, new HashMap<String,String>() );
	}
	
	public LinkDumpWriter(BufferedWriter aWriter, Set<String> articles, Map<String,String> redirects) {
		super(aWriter, articles, redirects);
		//doLowercase = lowercaseTitles;
	}

	public void writeEndPage() throws IOException {
	}

	public void writeEndWiki() throws IOException {
		// console output:
		// line break after last dot
		System.out.println();
	}
	
	public void writeAdmissableRevision(Revision rev) throws IOException {
		
		Matcher linkMatcher = s_linkPattern.matcher( rev.Text );
		// TODO" write self-link to article? (i.e. map the erticle name to itself as
		// if it were a link?)
		while( linkMatcher.find() )
		{// TODO: also include strings
			String linkText = linkMatcher.group( 1 );
			if( linkText != null ){
				String[] linkparts = linkText.split("\\|",-1);
				String link = linkparts[0].replaceAll(" ", "_");
				
				if (link.length() < 1) 
				  continue;
				
				String text;
				if (linkparts.length > 1){
					text = linkparts[1] + linkMatcher.group( 2 );
				} else {
					text = linkparts[0] + linkMatcher.group( 2 );
				}
				
				String normalForm = link.substring(0,1).toUpperCase() + link.substring(1);
				// TODO: recursive checking for normal-forms, maybe by preprocessing redirect table
        if ( this.redirect2target.containsKey(normalForm) ){
          //TODO: debugging
          normalForm = redirect2target.get(normalForm);
        } else if ( this.redirect2target.containsKey(link) ){
					//TODO: debugging
					normalForm = redirect2target.get(link);
				}
        normalForm = normalForm.substring(0,1).toUpperCase() + normalForm.substring(1);
				
				if ( !ignoreTitle(normalForm) ){
					bw.append(title); // TODO: change in format!
					bw.append(" ");
					bw.append(normalForm);
					bw.append(" ");
					bw.append( text.replace('\n', ' ').trim() );// TODO: why is this necessary?? or, delete instead of replace??
					bw.newLine();
				}
			}
		}			

	}

	public void writeSiteinfo(Siteinfo info) throws IOException {
		// do nothing
	}

	public void writeStartWiki() throws IOException {
		// do nothing
	}

}
