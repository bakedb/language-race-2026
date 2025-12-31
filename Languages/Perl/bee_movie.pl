#!/usr/bin/perl

# Hello, World!
print "Hello, World!\n";

use strict;
use warnings;

sub analyze_bee_movie {
    my $script_path = "../beemoviescript.txt";
    
    unless (-f $script_path) {
        print "File not found: $script_path\n";
        return;
    }
    
    print "Bee Movie Script:\n";
    print "--------------------------------------------------\n";
    
    my %letter_counts;
    my $total_letters = 0;
    
    # Read file line by line and print each line
    open my $fh, '<', $script_path or die "Cannot open file: $!";
    while (my $line = <$fh>) {
        chomp $line;
        print "$line\n";
        
        # Count letters
        for my $char (split //, $line) {
            if ($char =~ /[a-zA-Z]/) {
                my $lower = lc $char;
                $letter_counts{$lower}++;
                $total_letters++;
            }
        }
    }
    close $fh;
    
    print "--------------------------------------------------\n";
    print "Analysis complete.\n";
    
    if ($total_letters > 0) {
        # Sort by count (descending)
        my @sorted = sort { $letter_counts{$b} <=> $letter_counts{$a} } keys %letter_counts;
        
        print "\nTop 3 most commonly used letters:\n";
        for my $i (0..2) {
            last if $i >= @sorted;
            my $letter = $sorted[$i];
            my $count = $letter_counts{$letter};
            printf "%d. '%s': %d times\n", $i + 1, $letter, $count;
        }
    } else {
        print "No letters found in the script.\n";
    }
}

analyze_bee_movie();
