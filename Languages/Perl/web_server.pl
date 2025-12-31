#!/usr/bin/perl

# Hello, World!
print "Hello, World!\n";

use strict;
use warnings;
use JSON;
use LWP::UserAgent;
use HTTP::Request;

sub make_http_request {
    my ($url) = @_;
    
    my $ua = LWP::UserAgent->new(
        timeout => 5,
        agent => 'LanguageRace/1.0'
    );
    
    my $request = HTTP::Request->new('GET', $url);
    my $response = $ua->request($request);
    
    if ($response->is_success && $response->code == 200) {
        return $response->content;
    } else {
        return '';
    }
}

sub test_web_server {
    my $base_url = "http://localhost:3000";
    my $compare_file = "../webserver/compare.json";
    my $output_file = "test-result.json";
    
    # Load expected hashes
    unless (-f $compare_file) {
        print "Error: Could not find $compare_file\n";
        return;
    }
    
    my $expected_hashes;
    {
        local $/;
        open my $fh, '<', $compare_file or die "Cannot open $compare_file: $!";
        my $json_text = <$fh>;
        close $fh;
        $expected_hashes = decode_json($json_text);
    }
    
    print "Testing 100 endpoints...\n";
    
    my @results;
    my $passed = 0;
    my $failed = 0;
    
    # Test each endpoint
    for my $i (0..99) {
        my $endpoint = "test-$i";
        my $url = "$base_url/$endpoint";
        
        my $response = make_http_request($url);
        
        my $result = {
            endpoint => $endpoint,
            url => $url,
            expected_hash => $expected_hashes->{$endpoint} || ''
        };
        
        if (!$response) {
            $result->{status} = "FAILED";
            $result->{error} = "HTTP request failed";
            $failed++;
        } else {
            eval {
                my $data = decode_json($response);
                my $server_hash = $data->{hash} || '';
                $result->{server_hash} = $server_hash;
                
                if ($server_hash eq $result->{expected_hash}) {
                    $result->{status} = "PASSED";
                    $passed++;
                } else {
                    $result->{status} = "FAILED";
                    $failed++;
                }
            };
            if ($@) {
                $result->{status} = "FAILED";
                $result->{error} = "JSON parse error";
                $failed++;
            }
        }
        
        push @results, $result;
        
        # Progress indicator
        if (($i + 1) % 10 == 0) {
            print "Tested " . ($i + 1) . "/100 endpoints...\n";
        }
    }
    
    # Create final result
    my $total_tests = scalar @results;
    my $success_rate = sprintf("%.1f", ($passed / $total_tests) * 100);
    
    my $final_result = {
        total_tests => $total_tests,
        passed => $passed,
        failed => $failed,
        success_rate => $success_rate . "%",
        timestamp => scalar localtime,
        results => \@results
    };
    
    # Save results to file
    eval {
        open my $fh, '>', $output_file or die "Cannot open $output_file: $!";
        print $fh encode_json($final_result);
        close $fh;
        print "Results saved to: $output_file\n";
    };
    if ($@) {
        print "Error saving results: $@\n";
    }
    
    print "\nTest completed!\n";
    print "Passed: $passed/$total_tests ($success_rate%)\n";
    print "Failed: $failed/$total_tests (" . (100 - $success_rate) . "%)\n";
}

test_web_server();
