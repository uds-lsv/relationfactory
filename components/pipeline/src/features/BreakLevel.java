package features;

import rerac.protos.Corpus.Document;
import rerac.protos.Corpus.Document.AnnotationType;

/**
 * This specifies the mapping from integer valued break levels to constants
 * that are to be globally used.
 * 
 * @author Benjamin Roth
 *
 */
public class BreakLevel {
	public static final AnnotationType TYPE = Document.AnnotationType.BOUNDARY;
	// Break level wrt. previous token.
	public static final int NO_BREAK = 0;
	public static final int SPACE = 1;
	public static final int SENTENCE = 2;
	public static final int PARAGRAPH = 3;
}
