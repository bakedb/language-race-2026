#!/usr/bin/perl

# Hello, World!
print "Hello, World!\n";

use strict;
use warnings;
use Digest::SHA;
use JSON;
use File::Find;
use Time::HiRes qw(time);

sub calculate_file_hash {
    my ($filepath) = @_;
    
    eval {
        my $sha = Digest::SHA->new(256);
        open my $fh, '<', $filepath or die "Cannot open file: $!";
        binmode $fh;
        $sha->addfile($fh);
        close $fh;
        return $sha->hexdigest;
    };
    
    if ($@) {
        return "";
    }
}

sub get_file_size {
    my ($filepath) = @_;
    
    eval {
        return -s $filepath;
    };
    
    if ($@) {
        return 0;
    }
}

sub get_txt_files {
    my ($directory) = @_;
    
    unless (-d $directory) {
        print "Directory not found: $directory\n";
        return [];
    }
    
    my @files;
    find(sub {
        if (-f $_ && /\.txt$/) {
            push @files, $File::Find::name;
        }
    }, $directory);
    
    @files = sort @files;
    return @files;
}

sub get_timestamp {
    return scalar localtime;
}

sub hash_files_in_directory {
    my ($directory) = @_;
    
    my $files = get_txt_files($directory);
    
    if (@$files == 0) {
        print "No .txt files found in directory: $directory\n";
        return;
    }
    
    print "Found " . scalar(@$files) . " files to hash\n";
    
    my $results = {
        total_files => scalar(@$files),
        successful_hashes => 0,
        failed_hashes => 0,
        algorithm => "sha256",
        processing_time_seconds => 0,
        average_time_per_file_ms => 0,
        timestamp => get_timestamp(),
        directory => $directory,
        results => []
    };
    
    my $start_time = time();
    
    for my $i (0..$#$files) {
        my $filepath = $files->[$i];
        my $filename = (split '/', $filepath)[-1];
        my $size = get_file_size($filepath);
        
        my $result = {
            filename => $filename,
            filepath => $filepath,
            size => $size,
            algorithm => "sha256",
            hash => "",
            status => ""
        };
        
        my $hash = calculate_file_hash($filepath);
        if ($hash ne "") {
            $result->{hash} = $hash;
            $result->{status} = "SUCCESS";
            $results->{successful_hashes}++;
        } else {
            $result->{hash} = "";
            $result->{status} = "FAILED";
            $result->{error} = "Hash calculation failed";
            $results->{failed_hashes}++;
        }
        
        push @{$results->{results}}, $result;
        
        # Progress indicator
        if (($i + 1) % 100 == 0) {
            print "Processed " . ($i + 1) . "/" . scalar(@$files) . " files...\n";
        }
    }
    
    my $end_time = time();
    $results->{processing_time_seconds} = $end_time - $start_time;
    $results->{average_time_per_file_ms} = ($results->{processing_time_seconds} / scalar(@$files)) * 1000;
    
    # Save to file
    eval {
        open my $fh, '>', 'hash_results.json' or die "Cannot open output file: $!";
        print $fh encode_json($results);
        close $fh;
        print "Results saved to: hash_results.json\n";
    };
    
    if ($@) {
        print "Error saving results: $@\n";
    }
    
    print "\nHashing completed!\n";
    print "Total files: " . $results->{total_files} . "\n";
    print "Successful: " . $results->{successful_hashes} . "\n";
    print "Failed: " . $results->{failed_hashes} . "\n";
    printf "Processing time: %.3f seconds\n", $results->{processing_time_seconds};
    printf "Average time per file: %.2f ms\n", $results->{average_time_per_file_ms};
}

hash_files_in_directory('../hashfiles');
