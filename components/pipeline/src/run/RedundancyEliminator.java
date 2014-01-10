package run;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;

import query.QueryList;

import util.Responses;
import util.TextIdentifier;

import entity_expansion.MaxLinkEntityExpander;

public class RedundancyEliminator {
  
  /**
   * This removes redundant answers from a response file.
   * Of those answers that are mapped to a same normalized 'prototype' only the 
   * highest scoring one per slot is retained.
   * The normalization is based on Wikipedia anchor text, lowercasing and 
   * punctuation removal.
   * 
   * For conforming to 2013 task-definition for per:title, optionally a mapping 
   * file can be provided that contains line of tab-separated tuples:
   * 
   * sentenceId title organization
   * 
   * where the 'title' in the particular sentence is a job/function/role at 
   * 'organization' (currently matched heuristically by patterns).
   * If a job title is not associated with an organization, no line is contained
   * in the file.
   * The file is not expected to contain several organizations for the same 
   * sentenceId-title pair (in such a case it is undefined which organization 
   * is taken).
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 3 && args.length != 4) {
      System.err.println("RedundancyEliminator <linkstat> <orig_response> <query_xml> [<sentid_title_org_mapping>]");
      return;
    }
    String linkStatFn = args[0];
    String inFile = args[1];
    QueryList ql = new QueryList(args[2]);
    
    Map<String, String> sentIdTitleToOrg = new HashMap<String, String>();
    if (args.length > 3) {
      String mappingFn = args[3];
      BufferedReader br = new BufferedReader(new FileReader(mappingFn));
      for (String line; (line = br.readLine()) != null;) {
        String[] parts = line.split("\t");
        sentIdTitleToOrg.put(parts[0] + "\t" + parts[1], parts[2]);
      }
    }

    MaxLinkEntityExpander mle = new MaxLinkEntityExpander(linkStatFn);
    Responses r = new Responses(ql);
    r.readFile2012(inFile);
    r.eliminateRedundancy(mle, sentIdTitleToOrg);
    BufferedWriter bw = new BufferedWriter(
        new OutputStreamWriter(System.out, Charset.forName("UTF-8").newEncoder()));
    r.writeResponse(bw);
    bw.flush();
  }
}
