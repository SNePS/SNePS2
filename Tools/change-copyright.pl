#!/usr/bin/perl

# Based off: http://use.perl.org/~schwern/journal/15301
# Use this Perl script to crawl the Sneps directory structure changing the 
# copyright year
# usage: perl change-copyright.pl <sneps-directory> <new-year>



 use Fcntl ':mode';

my $top_path = shift;
my $copy_date = shift;

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
      my $found_copyright = 0;

      my $temp_fn =  getppid() . "-" . $file;

      open TEMPFILE, ">/tmp/" . $temp_fn;

      open IN, "<" . $fullpath;
      my $line;
      while($line = <IN>){
	if($line =~ /Copyright\s\(C\)\s(\d{4,4})\-\-(\d{4,4})/){
	  $found_copyright = 1;
	  $line =~ s/$2/$copy_date/;
	}
	
	if($line =~ /;;\s(\d{4,4})/){
	  $found_copyright = 1;
	  $line =~ s/$1/$copy_date/;
	}
	print TEMPFILE $line;
      
      }
      
      close IN;
      close TEMPFILE;

      system("mv /tmp/" . $temp_fn . " " . $fullpath);

      if(!$found_copyright){
	print "Found a file without copyright information: $fullpath \n";
      }
    }
  }
}


if (-d $top_path){
  print "Examining $top_path for copyright information.\n";
}
searchmod($top_path);

