package jesa.wikipedia;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.mediawiki.importer.Revision;
import org.mediawiki.importer.Siteinfo;

public class ContinouusLinkWindowWriter extends LinkWriter {
	private int wSize;
	
	static Pattern whitespaces = Pattern.compile("[\\w]+");
	private static final Pattern LETTER_PATTERN = Pattern.compile("[\\p{L}]+");
	private static final Pattern NON_LETTER_PATTERN = Pattern.compile("[^\\p{L}]+");

	
	Set<String> admissableLinks;
	
	public ContinouusLinkWindowWriter(BufferedWriter aWriter, Set<String> articles , int aWSize) {
		super(aWriter, articles , new HashMap<String, String>());
		this.wSize = aWSize;
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

	public void writeAdmissableRevision(Revision rev) throws IOException {
		
			Matcher linkMatcher = s_linkPattern.matcher( rev.Text );
			Matcher textMatcher = LETTER_PATTERN.matcher(rev.Text );
			
			int linkWinPos = 2 * wSize + 1;
			
			int endPos = rev.Text.length(); // TODO: -1 ?
			int currPos = 0;
			
			LinkedList<String> slidingWindow = new LinkedList<String>();
			
			boolean linkmatches = true;
			boolean textmatches = true;
			
			do {

				
				if ( linkmatches && (currPos == 0 || linkMatcher.start() < currPos) ){ // if it stops once to match, it will never match
					linkMatcher.region( currPos, endPos );
					linkmatches = linkMatcher.find();
				}

				if ( textmatches && ( currPos == 0 || textMatcher.start() < currPos ) ){
					textMatcher.region( currPos, endPos );				
					textmatches = textMatcher.find();
				}
				
				if ( textmatches && ( !linkmatches || textMatcher.start() < linkMatcher.start() ) ) { // next thing to find is text
					//System.out.print(textMatcher.group() + " ");
					slidingWindow.add( textMatcher.group() );
					if ( slidingWindow.size() > wSize + 1 ){
						slidingWindow.removeFirst();
					}
					linkWinPos += 1;
					if ( linkWinPos <= 2 * wSize )
						bw.append( slidingWindow.getFirst() + " " );
					else if ( linkWinPos == 2 * wSize + 1 )
						bw.append('\n');
											
					currPos = textMatcher.end();				
				} else if ( linkmatches ) { // next is a link			
					String linkText = linkMatcher.group( 1 );
					if( linkText != null ){
						String[] linkparts = linkText.split("\\|",-1);
						String link = linkparts[0].replaceAll(" ", "_");
						String text;
						if (linkparts.length > 1){
							text = linkparts[1] + linkMatcher.group( 2 );
						} else {
							text = linkparts[0] + linkMatcher.group( 2 );
						}
						
						if (admissableLinks.contains(link)){ // todo: also make second q to print following tokens
							slidingWindow.add( "#" + link );
							if ( slidingWindow.size() > wSize + 1  ){
								slidingWindow.removeFirst();
							}
							
							linkWinPos = 0;
							bw.append( slidingWindow.getFirst() + " " );
							
						} else {
							for ( String word : NON_LETTER_PATTERN.split(text) ){
								slidingWindow.add( word );
								if ( slidingWindow.size() > wSize + 1 ){
									slidingWindow.removeFirst();
								}
								
								linkWinPos += 1;
								if ( linkWinPos <= 2 * wSize )
									bw.append( slidingWindow.getFirst() + " " );
								else if ( linkWinPos == 2 * wSize + 1 )
									bw.append('\n');
							}
						}
					}

					currPos = linkMatcher.end();	
				}
				

				
			} while ( linkmatches || textmatches );
	}

	public void writeSiteinfo(Siteinfo info) throws IOException {
		// do nothing
	}

	public void writeStartWiki() throws IOException {
		// do nothing
	}
}
