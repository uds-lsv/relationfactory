package features;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import opennlp.tools.postag.POSModel;
import opennlp.tools.postag.POSTaggerME;

import rerac.protos.Corpus.Document;
import rerac.protos.Corpus.Document.Annotation;
import rerac.protos.Corpus.Document.AnnotationType;
import rerac.protos.Corpus.Document.Method;
import rerac.protos.Corpus.Document.Token;

// TODO: something similar for ne tagging: 
// see e.g. http://www.asksunny.com/drupal/?q=node/4
public class OpenNlpTagger implements DocumentTagger {
  
  POSTaggerME tagger;

  public OpenNlpTagger(String modelFN) throws IOException {
    InputStream in = new FileInputStream(modelFN);
    POSModel model = new POSModel(in);
    in.close();
    tagger = new POSTaggerME(model);
  }
  
  public Document tag(Document doc) {
    String[] sent = new String[doc.getTokenCount()];
    for (int i = 0; i < doc.getTokenCount(); ++i) {
      Token tok = doc.getToken(i);
      sent[i] = tok.getText();
    }
    String[] tags = tagger.tag(sent);
    
    Document.Builder taggedDoc = Document.newBuilder(doc);
    int taggerIdx = taggedDoc.getMethodCount();
    String taggerName = DocumentTagger.POS;
    AnnotationType taggerType = AnnotationType.TAG;
    Method.Builder m = 
      Method.newBuilder().setId(taggerName).setType(taggerType);
    taggedDoc.setConsistentMethod(false);
    taggedDoc.addMethod(m);
    if (taggedDoc.getTokenCount() != tags.length) {
      throw new IllegalStateException(
          "Number of tags not equal to tokens in doc: " + doc.getId());
    }
    for (int i = 0; i < tags.length; ++i) {
      Token.Builder tokB = taggedDoc.getTokenBuilder(i);
      Annotation.Builder anno = Annotation.newBuilder();
      anno.setType(taggerType);
      anno.setMethodIndex(taggerIdx);
      anno.setText(tags[i]);
      tokB.addAnnotation(anno);
    }
    return taggedDoc.build();
  }
  
}
