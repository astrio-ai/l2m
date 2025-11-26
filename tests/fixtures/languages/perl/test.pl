package Greeter;
use strict;
use warnings;

sub greet {
    my ($name) = @_;
    print "Hello, $name!\n";
}

sub greet_times {
    my ($name, $count) = @_;
    for (1 .. $count) {
        greet($name);
    }
}

my $name = "World";
greet_times($name, 3);

1;

