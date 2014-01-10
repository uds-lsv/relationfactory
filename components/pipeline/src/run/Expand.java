package run;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import query.QueryList;


public class Expand {
  /**
   * Expands a query.xml:
   * 1) aliases are added
   * 2) relations are listet explicitly
   * 
   * @param args
   * @throws IOException
   */
  public static void main(String[] args) throws IOException {
    if (args.length != 7 && args.length != 8) {
      System.out.println("Expand <query_xml> <relations> <relation_config> <expansions> <maxN> <org_suffixes> <expanded.xml> [<require_backlinks=true|false|none|wiki|rules|suffix|lastname>]");
      System.out.println("for 'require_backlinks', 'true' means precision expansion, 'false' means standard expansion, 'none' means no expansion");
      System.out.println(" 'wiki' means only wiki expansion with backlinks (no rules), 'rules' means only rule-based (org: suffixes, per: last name).");
      System.out.println(" 'suffix' means only org: suffixes, 'lastname' for only per: last name.");
      return;
    }
    String qXmlFn = args[0];
    String relsFn = args[1];
    String relsCfgFn = args[2];
    String expansionStatFn = args[3];
    int maxN = Integer.parseInt(args[4]);
    String orgSuffixFn = args[5];
    String outFn = args[6];
    
    // Do expansion unless explicitely not.
    boolean doExpansion = args.length < 7 || !"none".equals(args[7]);
    
    // Default is precision oriented expansion.
    boolean requireLinkBack = args.length > 7 ? Boolean.parseBoolean(args[7]) 
        : true; // TODO: change default if profitable.
    
    boolean onlyWiki = "wiki".equals(args[7]);
    boolean onlyRules = "rules".equals(args[7]);
    boolean onlySuffix = "suffix".equals(args[7]);
    boolean onlyLastname = "lastname".equals(args[7]);
    
    if (!doExpansion) {
      System.err.println("No query expansion.");
    } else if (onlyWiki) {
      System.err.println("Only wiki expansion (with link-back).");
    } else if (onlyRules) {
      System.err.println("Only rule-based expansion.");
    } else if (requireLinkBack) {
      System.err.println("Query expansion need to be linked to same WP entity as query.");
    } else {
      System.err.println("Query expansion do NOT need to be linked to same WP entity as query.");
    }
    
    List<String> orgSuffixes = new ArrayList<String>();
    
    if (!onlyLastname && !onlyWiki) {
      BufferedReader br = new BufferedReader(new FileReader(orgSuffixFn));
      for (String suffix; (suffix = br.readLine()) != null;) {
        orgSuffixes.add(suffix);
      }
      br.close();
    }
    
    QueryList ql = new QueryList(qXmlFn);
    ql.expandRelations(relsFn, relsCfgFn);
    
    if (doExpansion) {
      if (onlyWiki) {
        ql.addWikiExpansions(expansionStatFn, maxN, requireLinkBack);
      } else if (onlyRules) {
        ql.addRuleExpansions(true, orgSuffixes);
      } else if (onlySuffix) {
        ql.addRuleExpansions(false, orgSuffixes);
      } else if (onlyLastname) {
        ql.addRuleExpansions(true, orgSuffixes);
      } else {
        ql.addExpansions(expansionStatFn, maxN, true, requireLinkBack, orgSuffixes);
      }
    }

    BufferedWriter bw = new BufferedWriter(new FileWriter(outFn));
    ql.writeTo(bw);
    bw.close();
  }
}
