#!/usr/bin/perl
use Lingua::EN::Sentence qw( get_sentences add_acronyms );
{ local $/ = ''; # paragraph mode
     while(<>) {
          my $cleanPar = "";#
          my @lines = split(/\n/,$_);
          foreach my $line(@lines){
              if($line =~ /^#/){
     my $sentences=get_sentences($cleanPar);     ## Get the sentences.
     foreach my $sentence (@$sentences) {
         $sentence =~ s/\n/ /gm;
         $sentence =~ s/^#*//;
         print $sentence, "\n";
     }
     $cleanPar = "";
                  print $line, "\n";
              } else {
                  $cleanPar = $cleanPar . $line . "\n";
              }
          }
     my $sentences=get_sentences($cleanPar);     ## Get the sentences.
     foreach my $sentence (@$sentences) {
         $sentence =~ s/\n/ /gm;
         $sentence =~ s/^#*//;
         print $sentence, "\n";
     }
     $cleanPar = "";
     }
}

