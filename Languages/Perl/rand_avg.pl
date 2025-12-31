#!/usr/bin/perl

# Hello, World!
print "Hello, World!\n";

use strict;
use warnings;

sub generate_random_numbers {
    my $count = 1000;
    my $output_dir = "../rand_avg output";
    my $output_file = "$output_dir/random_numbers.txt";
    
    # Create output directory
    unless (-d $output_dir) {
        mkdir $output_dir, 0755 or die "Cannot create directory: $!";
    }
    
    # Generate 1000 random numbers
    my @random_numbers;
    my $sum = 0;
    
    for my $i (1..$count) {
        my $num = int(rand(1000));
        push @random_numbers, $num;
        $sum += $num;
    }
    
    # Calculate mean
    my $mean = $sum / $count;
    
    # Save to file
    open my $fh, '>', $output_file or die "Cannot open file: $!";
    for my $num (@random_numbers) {
        print $fh "$num\n";
    }
    close $fh;
    
    print "Generated 1000 random numbers\n";
    printf "Mean: %.2f\n", $mean;
    print "Saved to: $output_file\n";
}

generate_random_numbers();
