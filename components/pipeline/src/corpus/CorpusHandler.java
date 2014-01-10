package corpus;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.log4j.Logger;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;


/*
Variant 1:

<DOC>
<DOCID> eng-NG-31-137585-9798571 </DOCID>
<DOCTYPE SOURCE="usenet"> USENET TEXT </DOCTYPE>
<DATETIME> 2007-12-17T12:53:00 </DATETIME>
<BODY>
<HEADLINE>
....
</HEADLINE>
<TEXT>
<POST>
<POSTER> prot...@protectyourwaters.net </POSTER>
<POSTDATE> 2007-12-17T12:53:00 </POSTDATE>
....
</POST>
</TEXT>
</BODY>
</DOC>


Variant 2:

<DOC>
<DOCID> NYT_ENG_20070818.0008.LDC2009T13 </DOCID>
<DOCTYPE SOURCE="newswire"> NEWS STORY </DOCTYPE>
<DATETIME> 2007-08-18 </DATETIME>
<BODY>
<HEADLINE>
CAROLYN GOODMAN, RIGHTS CHAMPION, 91
</HEADLINE>
<TEXT>
<P>
....
</P>
</TEXT>
</BODY>
</DOC>

Variant 3: (?!)
<DOC id="WPB_ENG_20100301.0003" type="story">
...
</DOC>
*/
public class CorpusHandler extends DefaultHandler {
  
  static Logger logger = Logger.getLogger(CorpusHandler.class.getName());
  
  private StringBuilder fieldBuilder;
  private StringBuilder textBuilder;
  
  List<CorpusDocument> documents = new ArrayList<CorpusDocument>();
  private CorpusDocument currentDoc;
  
  // Intentionally package-visible. Not meant to be used outside this package.
  CorpusHandler() {  }
      
  @Override
  public void characters(char[] ch, int start, int length)
      throws SAXException {
    fieldBuilder.append(Arrays.copyOfRange(ch, start, start + length));
  }

  @Override
  public void endElement(String uri, String localName, String qName)
      throws SAXException {
    if (currentDoc == null) {
      return;
    }
    if (localName.equalsIgnoreCase("doc")) {
      
      // no text element seen
      if (textBuilder != null) {
        currentDoc.text = textBuilder.toString().replaceAll("\n{3,}", "\n\n").
            replace("\u00a0", " ").trim();  // replace no-break whitespace with normal whitespace
        textBuilder = null;
      }
      
      documents.add(currentDoc);
      currentDoc = null;
    } else if (localName.equalsIgnoreCase("docid")) {
      currentDoc.id = fieldBuilder.toString().trim();
    } else if (localName.equalsIgnoreCase("doctype")) {
      currentDoc.type = fieldBuilder.toString().trim();
    } else if (localName.equalsIgnoreCase("headline")) {
      currentDoc.title = fieldBuilder.toString().trim();
    } else if (localName.equalsIgnoreCase("p")) {
      textBuilder.append(fieldBuilder.toString().trim() + "\n\n");
    } else if (localName.equalsIgnoreCase("poster") || 
        localName.equalsIgnoreCase("post") ||
        localName.equalsIgnoreCase("quote") ||
        localName.equalsIgnoreCase("speaker") ||
        localName.equalsIgnoreCase("turn") ||
        localName.equalsIgnoreCase("postdate")) {
      textBuilder.append("\n\n" + fieldBuilder.toString().trim() + "\n\n");
    } else if (localName.equalsIgnoreCase("text")) {
      textBuilder.append(fieldBuilder.toString());
      currentDoc.text = textBuilder.toString().replaceAll("\n{3,}", "\n\n").
          replace("\u00a0", " ").trim();  // replace no-break whitespace with normal whitespace
      textBuilder = null;
    } else {
      if (textBuilder != null) {
        logger.warn("Unahndled element withoin text: " + localName);
        textBuilder.append("\n\n" + fieldBuilder.toString().trim() + "\n\n");
      }
    }
    fieldBuilder = new StringBuilder();
  }
  
  @Override
  public void startElement(String uri, String localName, String qName,
      Attributes atts) throws SAXException {
    if (textBuilder != null && fieldBuilder != null && 
        fieldBuilder.length() > 0) {
      textBuilder.append("\n\n" + fieldBuilder.toString().trim() + "\n\n");
    }
    fieldBuilder = new StringBuilder();
    if (localName.equalsIgnoreCase("doc")) {
      currentDoc = new CorpusDocument();
      if (atts.getValue("id") != null) {
        currentDoc.id = atts.getValue("id");
      }
      if (atts.getValue("type") != null) {
        currentDoc.type = atts.getValue("type");
      }
      logger.debug("New document found, " + currentDoc);
    } else if (localName.equalsIgnoreCase("text")) {
      textBuilder = new StringBuilder();
    } else if (localName.equalsIgnoreCase("post") && 
        textBuilder == null) {  // 2013 discussion corpus
      textBuilder = new StringBuilder();
    } else if (localName.equalsIgnoreCase("quote") && 
        atts.getValue("quote") != null) {
      textBuilder.append("\n\n" + atts.getValue("quote") + "\n\n");
    }
  }
  
  @Override
  public void warning(SAXParseException e) throws SAXException {
    logger.warn(e.getMessage());
  }
  
  @Override
  public void error(SAXParseException e) throws SAXException {
    logger.error(e.getMessage());
  }
  
  @Override
  public void fatalError(SAXParseException e) throws SAXException {
    logger.fatal(e.getMessage());
  }
  
  private String normalizeBody(String text) {
    return text.replaceAll("\n{3,}", "\n\n").
                replaceAll("[\u00a0", " "). // replace no-break whitespace with normal whitespace
                trim();  
  }
  
}
