package jesa.wikipedia;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jesa.datatypes.WordCounts;

import org.mediawiki.importer.DumpWriter;
import org.mediawiki.importer.Page;
import org.mediawiki.importer.Revision;
import org.mediawiki.importer.Siteinfo;
import org.tartarus.snowball.SnowballStemmer;

import edu.uka.aifb.wikipedia.WikipediaTools;

/**
 * 
 * This writes the tokenized dumping result to a text file.
 * 
 * @author Benjamin Roth, beroth-AT-coli.uni-saarland.de, 2009
 *
 */
public class CountDumpWriter implements DumpWriter {
	// TODO: include page to filter out more than redirects?
	private BufferedWriter bw;// TODO: Writer/OutputStream?
	private String title;
	private Map<String , Integer> term2df;
	private SnowballStemmer stemmer;
	private Set<String> stopWords;
	private int nDocs;
	private boolean lowercase;
	private int minwordlength = 2; // TODO
	private int maxwordlength = 64; // TODO: make constant in ToCountConverter
	private int mindoclength;
	private int doclengthCutoff;
	
	// TODO: multilingual... maybe in another class?
	private List<Pattern> langLinkPatterns = null;
	private BufferedWriter linkFileWriter;
	
	private static final Pattern LETTER_PATTERN = Pattern.compile("[\\p{L}]+");
	
	public CountDumpWriter(BufferedWriter aWriter, SnowballStemmer aStemmer, Set<String> someWords, boolean lowercase, int mindl, int dlcutoff) {
		mindoclength = mindl;
		doclengthCutoff = dlcutoff;
		bw = aWriter;
		stemmer = aStemmer;
		stopWords = someWords;
		nDocs = 0;
		this.lowercase = lowercase;
		this.term2df = new HashMap<String, Integer>();
	}
	
	public CountDumpWriter(BufferedWriter bw, SnowballStemmer stemmer, Set<String> stopWords, boolean lowercase, int mindl, int maxdl, String[] langIds, BufferedWriter lbw) throws IOException {
		this(bw, stemmer, stopWords, lowercase, mindl, maxdl );
		this.linkFileWriter = lbw;
		this.initLangLinkPatterns(langIds);
	}

	private void initLangLinkPatterns(String[] linkLangs) throws IOException {
		this.langLinkPatterns = new ArrayList<Pattern>();
		
		boolean isFirst = true;
		for (String langId : linkLangs){
			if( !isFirst ){
				this.linkFileWriter.append(" ");
				this.linkFileWriter.append(langId);
				langLinkPatterns.add(
					Pattern.compile("\\[\\["+ Pattern.quote(langId) +":([^\\]\\[\\n]+)\\]\\]" ));
			} else {
				this.linkFileWriter.append(langId);
				isFirst = false;
			}
		}
		
		this.linkFileWriter.newLine();
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
		
		// TODO: also Language specific stuff like "Vorlage:" "Datei:" "Hilfe:" "Portal:" "Animation:" "MediaWiki:" "Kategorie:"
		//maybe: all colon seperated? but stuff like "2001:_Odyssee_im_Weltraum"
		if (!title.isEmpty() && !title.startsWith("Wikipedia:") && 
				 !title.startsWith("File:") && 
				 !title.startsWith("Category:") && 
				 !title.startsWith("Template:") && 
				 !title.startsWith("Portal:") &&
				 !title.startsWith("MediaWiki:") &&
				 !title.startsWith("Help:") && 
				 !title.endsWith("(disambiguation)") &&
				!rev.Text.startsWith("#REDIRECT") && 
				!rev.Text.startsWith("#redirect") ) {
			
			WordCounts wc = new WordCounts();

			wc.setName( title );
			
			String plainText = WikipediaTools.extractPlainText( rev.Text );		
			
			Matcher termMatcher = LETTER_PATTERN.matcher(plainText);
			
			while ( termMatcher.find() && wc.getTypeCount() < doclengthCutoff ) {
				String term = termMatcher.group();
                if (lowercase)
                	term = term.toLowerCase();
                if( term.length() >= minwordlength && term.length() <= maxwordlength &&
                		( null == stopWords || !stopWords.contains(term) ) 
                		){
                	// term is not a stopword
                	
                	if (null != stemmer){
                		stemmer.setCurrent(term);
                		stemmer.stem();
                		term = stemmer.getCurrent();
                	}
                	
                    wc.incCount(term);
                }			
			}

			if (wc.getTypeCount() >= mindoclength ){
				 wc.writeTo(bw);
				 
				 if ( null != this.langLinkPatterns ){
					 
					 this.writeLinks( rev.Text);
					 
				 }
				 
				 for ( String term : wc.getTerm2Count().keySet() ){
					 int df = term2df.containsKey(term)? term2df.get(term) : 0;
					 term2df.put(term, df + 1);        	
				 }
				 
				 
				nDocs++;
				
				if ( 0 == nDocs % 1000 )
					System.out.print('.');
			}
		}
	}

	private void writeLinks(String text) throws IOException {
		linkFileWriter.append( title );
		
		for ( Pattern p : this.langLinkPatterns ){
			Matcher m = p.matcher(text);
			linkFileWriter.append(" ");
			if ( m.find() ){
				linkFileWriter.append( m.group(1).replaceAll(" ", "_") );
			}
		}
		
		linkFileWriter.newLine();
	}

	public void writeSiteinfo(Siteinfo info) throws IOException {
		// do nothing
	}

	public void writeStartPage(Page page) throws IOException {
		title = page.Title.toString().trim().replaceAll(" ", "_");
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
