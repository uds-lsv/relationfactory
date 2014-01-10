package jesa.wikipedia;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;

import jesa.datatypes.StringPair;

import org.mediawiki.importer.Revision;
import org.mediawiki.importer.Siteinfo;

public class LinkPPMIDumpWriter extends LinkWriter {
	// TODO: include page to  filter out more than redirects?
	
	static final int MAX_LINKS = 10; // just to make it tracktable for the first runs - try to increase later!
	
	Map<StringPair, Integer> pair2c = new HashMap<StringPair, Integer>();
	Map<String, Integer> l2c = new HashMap<String, Integer>();
	Map<String,String> uniqString = new HashMap<String, String>();
	
	int articleCounter = 0;
		
	
	public LinkPPMIDumpWriter(BufferedWriter aWriter, Set<String> articles, Map<String,String> redirects) {
		super(aWriter, articles, redirects);
	}
	
	public void close() throws IOException {
		Map<StringPair, Double> p2ppmi = new HashMap<StringPair, Double>();
		//Map<String, Double> l2ppmiSum = new HashMap<String, Double>();
		
		double logN = Math.log( articleCounter );
		
		// TODO: ppmi for self translation
		
		for ( String l : l2c.keySet() ){
			double prob = l2c.get(l) / (double) articleCounter;
			double ppmi = prob * ( - Math.log( prob ) );
			//l2ppmiSum.put(l, ppmi);
			p2ppmi.put( new StringPair(l, l) , ppmi);
		}
		
		for ( StringPair p : pair2c.keySet() ){
			int c_pair = pair2c.get(p);
			String s1 = p.getFirst();
			String s2 = p.getSecond();
			int c1 = l2c.get( s1 );
			int c2 = l2c.get( s2 );
			
			double prob = c_pair / (double) articleCounter;
			double pmi = prob * ( Math.log( c_pair ) + logN - Math.log(c1) - Math.log(c2) );
			
			if ( pmi > 0 ){
				p2ppmi.put(p, pmi);
				//l2ppmiSum.put(s1, l2ppmiSum.get( s1 ) + pmi);
				//l2ppmiSum.put(s2, l2ppmiSum.get( s2 ) + pmi);
			} //else
				//System.out.println("pmi<0: " + s1 + " " + s2);
		}
		
		for ( StringPair p : p2ppmi.keySet() ){
			String s1 = p.getFirst();
			String s2 = p.getSecond();
			double ppmi = p2ppmi.get(p);
			
			bw.append( s1 );
			bw.append(' ');
			bw.append( s2 );
			bw.append(' ');
			bw.append( Double.toString( ppmi /*/ l2ppmiSum.get(s1)*/ ) );
			bw.append( '\n' );
			/*
			if ( !s1.equals(s2) ){			
				bw.append( s2 );
				bw.append(' ');
				bw.append( s1 );
				bw.append(' ');
				bw.append( Double.toString( ppmi / l2ppmiSum.get(s2) ) );
				bw.append( '\n' );
			}*/
		}
		bw.flush();
		bw.close(); // TODO: here?
	}

	public void writeEndPage() throws IOException {
	}

	public void writeEndWiki() throws IOException {
	}
	
	public void writeAdmissableRevision(Revision rev) throws IOException {
		/*if (!title.isEmpty() && !title.startsWith("Wikipedia:") && 
				 !title.startsWith("File:") && 
				 !title.startsWith("Category:") && 
				 !title.startsWith("Template:") && 
				 !title.startsWith("Portal:") &&
				 !title.startsWith("MediaWiki:") &&
				 !title.startsWith("Help:") && 
				 !title.endsWith("(disambiguation)") &&
				!rev.Text.startsWith("#REDIRECT") && 
				!rev.Text.startsWith("#redirect") ) */
		articleCounter++;
		
		if(articleCounter % 10000 == 0 )
			System.out.print(".");
		
		Matcher linkMatcher = s_linkPattern.matcher( rev.Text );
		
		Set<String> links = new HashSet<String>();
		
		while( linkMatcher.find() && links.size() < MAX_LINKS)
		{// TODO: also include strings
			String linkText = linkMatcher.group( 1 );
			if( linkText != null ){
				String[] linkparts = linkText.split("\\|",-1);
				String link = linkparts[0].replaceAll(" ", "_");
				
				if ( admissableLinks.contains(link) ){					
					String normalForm = link;
					// TODO: recursive checking for normal-forms, maybe by preprocessing redirect table
					if ( this.redirect2target.containsKey(link) ){
						//TODO: debugging
						normalForm = redirect2target.get(link);
					}
					
					// make sure every article is just represented by one String object
					if (uniqString.containsKey(normalForm) ){
						normalForm = uniqString.get(normalForm);
					} else {
						normalForm = new String(normalForm);
						uniqString.put(normalForm, normalForm);
					}
							
					links.add(normalForm);
				}
			}				
		}	
		
		for (String s1 : links){
			int uniCnt = l2c.containsKey(s1) ? l2c.get(s1) : 0;
			l2c.put(s1, uniCnt + 1);
			
			for (String s2 : links){
				if ( s1.compareTo(s2) > 0){
					StringPair p = new StringPair(s1, s2);
					int pairCnt = pair2c.containsKey( p ) ? pair2c.get(p) : 0;
					pair2c.put( p , pairCnt + 1 );
					//System.out.println(s1 + " " + s2 + " " + pairCnt);
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
