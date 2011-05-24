#!/usr/bin/perl

# Based off: http://use.perl.org/~schwern/journal/15301
# Use this Perl script to crawl the Sneps directory structure changing the 
# copyright year
# usage: perl change-copyright.pl <sneps-directory> <license-file>



 use Fcntl ':mode';

# The directory to start in
my $top_path = shift;

# The license file to write to each sneps file
my $license_file = shift;

sub searchmod {
  my $path = shift;
  my ($file, $fullpath);
  opendir(DIR, $path) || die "Can't open $path: $!";
  my @files = grep { !/^[\.#]/ } readdir(DIR);
  closedir DIR;
  
  foreach $file (@files){
    $fullpath = "$path/$file";

    if(-d $fullpath){  
      call &searchmod($fullpath);
    }
    elsif($fullpath =~ /.*\.lisp$/){

      my $temp_fn =  getppid() . "-" . $file;

      open TEMPFILE, ">/tmp/" . $temp_fn;

      open IN, "<" . $fullpath;
      my $line;
      while($line = <IN>){
	if($line =~ /;; This file is part of SNePS\./){
	  print TEMPFILE $line;
	  print TEMPFILE "\n";
	  print TEMPFILE ";; \$BEGIN LICENSE\$\n\n";
	  while(!($line =~ /;; 201 Bell Hall, Buffalo, NY 14260, USA/)){
	    $line = <IN>;
	  }
	  
	  open LICENSE, "<" . $license_file;
	  while($line = <LICENSE>){
	    print TEMPFILE ";; " . $line;
	  }
	  print TEMPFILE "\n\n;; \$END LICENSE\$\n\n";
	  
	}
	else {
	  print TEMPFILE $line;
	}
      }
      
      close IN;
      close TEMPFILE;

      system("mv /tmp/" . $temp_fn . " " . $fullpath);

    }
  }
}


if (-d $top_path){
  print "Examining $top_path for license information.\n";
}
searchmod($top_path);

