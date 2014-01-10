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

// TODO: consider two-sided context!
public class LinkWindowWriter extends LinkWriter {
	
	int wSize;
	// this splits the wiki source text into units
	public static final Pattern TOKENIZE_TEXT_PATTERN = Pattern.compile("[\\p{L}]+");
	// this removes everything that is not "reasonable text", e.g. wiki markup
	public static final Pattern VERIFY_TOKEN_PATTERN = Pattern.compile("[\\p{L}]+"); // TODO: this is redundant, remove again
	
	Set<String> admissableLinks;
	
	static Pattern s_templatesPattern = Pattern.compile(
	"\\{\\{([^\\}]+)\\}\\}" );

	static Pattern s_externalLinkPattern = Pattern
			.compile("\\[([^\\[\\]]+)\\]");

	static Pattern s_htmlTagPattern = Pattern.compile("<[\\!-/=\"'\\w\\s]+>");

	static Pattern s_wikiMarkupPattern = Pattern
			.compile("'{2,}|={2,}|(----)|(^[ \t]*[\\*#]+)");

	static Pattern s_tableMarkupPattern = Pattern
			.compile("\\{\\||\\|[\\+\\}\\-]?|!");

	static Pattern s_languageLinkPattern = Pattern
			.compile("\\[\\[\\w{2,3}([-_]\\w+)?:[^\\]]+\\]\\]");

	static Pattern s_formatPattern = Pattern
			.compile("\\w+=(\"[\\w\\s#;:\\-_]+\"|\\w+)"); // e.g. style=
															// "border: 1px #aaa solid; border-collapse: collapse;"

	static Pattern s_webLinkPattern = Pattern
			.compile("https?://[\\w\\.\\-_/?&%#]+");

	static public String extractPlainText(String wikiText) {
		// logger.trace( wikiText );

		/*
		 * find all templates and only keep argument values
		 */
		Matcher templatesMatcher = s_templatesPattern.matcher(wikiText);
		StringBuffer sb = new StringBuffer();
		while (templatesMatcher.find()) {
			String templateText = templatesMatcher.group(1);

			if (templateText == null) {
				templatesMatcher.appendReplacement(sb, " ");
			} else {
				String[] templateTextSplit = templateText.split("\\|");
				StringBuilder newTemplateTextBuilder = new StringBuilder();
				for (int i = 1; i < templateTextSplit.length; i++) {
					newTemplateTextBuilder.append(" ");
					newTemplateTextBuilder.append(templateTextSplit[i]
							.replaceAll("^\\s*\\w+\\s*=", ""));
				}
				newTemplateTextBuilder.append(" ");

				String newTemplateText = newTemplateTextBuilder.toString();
				newTemplateText = newTemplateText.replaceAll("\\$", "\\\\\\$");

				templatesMatcher.appendReplacement(sb, newTemplateText);
			}
		}
		templatesMatcher.appendTail(sb);
		String currentText = wikiText;

		/*
		 * remove all language links from text
		 */
		currentText = s_languageLinkPattern.matcher(currentText)
				.replaceAll(" ");

		/*
		 * remove external link markup
		 */
		currentText = s_externalLinkPattern.matcher(currentText).replaceAll(
				"$1");

		/*
		 * remove html tags
		 */
		currentText = s_htmlTagPattern.matcher(currentText).replaceAll(" ");

		/*
		 * remove wiki markup
		 */
		currentText = s_wikiMarkupPattern.matcher(currentText).replaceAll(" ");

		/*
		 * remove table markup
		 */
		currentText = s_tableMarkupPattern.matcher(currentText).replaceAll(" ");

		/*
		 * remove formating markup
		 */
		currentText = s_formatPattern.matcher(currentText).replaceAll(" ");

		/*
		 * remove formating markup
		 */
		currentText = s_formatPattern.matcher(currentText).replaceAll(" ");

		/*
		 * remove web links
		 */
		currentText = s_webLinkPattern.matcher(currentText).replaceAll(" ");

		// logger.trace( currentText );
		return currentText;
	}

	public LinkWindowWriter(BufferedWriter aWriter, Set<String> articles , int aWSize) {
		super( aWriter, articles, new HashMap<String, String>() );
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
			//Matcher textMatcher = LETTER_PATTERN.matcher(rev.Text );
			Matcher textMatcher = TOKENIZE_TEXT_PATTERN.matcher(rev.Text );
			
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
					
					String word = textMatcher.group();
					
					if (VERIFY_TOKEN_PATTERN.matcher( word ).matches() ){
					
						slidingWindow.add( word.replaceAll("[0-9]", "\\$") );
						if ( slidingWindow.size() > wSize ){
							slidingWindow.removeFirst();
						}
					
					}
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
							
							//System.out.println(rev.Text.substring(Math.max(0, linkMatcher.start() - 30), linkMatcher.end()));
							
							for ( String ctxtTok : slidingWindow ){
								bw.append( ctxtTok );
								bw.append( " " );
								//System.out.print( ctxtTok + " ");
							}
							
							bw.append("#");
							bw.append(link);
							bw.append('\n');
							//System.out.println("#" + link);

						} 
						
						Matcher anchorTokenizer = TOKENIZE_TEXT_PATTERN.matcher(text);
						
						
						while ( anchorTokenizer.find() ){
						
							String word = anchorTokenizer.group();
							
							if (VERIFY_TOKEN_PATTERN.matcher( word ).matches() ){
								slidingWindow.add( word.replaceAll("[0-9]", "\\$") );
								if ( slidingWindow.size() > wSize ){
									slidingWindow.removeFirst();
								}
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