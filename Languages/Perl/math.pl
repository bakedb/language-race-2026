#!/usr/bin/perl

# Hello, World!
print "Hello, World!\n";

print "\nProcessing math equations...\n";

use strict;
use warnings;

sub evaluate_expression {
    my ($expr) = @_;
    
    # Remove extra whitespace
    $expr =~ s/^\s+|\s+$//g;
    
    # Use eval to solve the math equation (safe in this controlled environment)
    my $result;
    {
        no warnings 'all';
        $result = eval($expr);
    }
    
    if ($@) {
        return "Error";
    }
    
    return $result;
}

sub solve_equation {
    my ($line) = @_;
    
    my $equation = $line;
    
    # Remove "= ?" part
    $equation =~ s/=.*$//;
    
    # Trim whitespace
    $equation =~ s/^\s+|\s+$//g;
    
    my $result = evaluate_expression($equation);
    print "$equation = $result\n";
}

sub process_file {
    my ($filename) = @_;
    
    unless (-f $filename) {
        print "Could not open file: $filename\n";
        return;
    }
    
    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    
    while (my $line = <$fh>) {
        chomp $line;
        
        # Skip empty lines and markdown headers
        next if $line =~ /^\s*$/ or $line =~ /^\s*#/;
        
        # Handle markdown list items
        my $start = $line;
        if ($line =~ /^\s*-\s/) {
            $start =~ s/^\s*-\s//;
        }
        
        if ($start =~ /=/) {
            solve_equation($start);
        }
    }
    
    close $fh;
}

# Process all files
process_file("../test_data/math_equations.txt");
process_file("../test_data/math_equations.md");
process_file("../test_data/math_equations.json");
process_file("../test_data/math_equations.yaml");
process_file("../test_data/math_equations");
