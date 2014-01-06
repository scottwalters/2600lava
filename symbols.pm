
package symbols;

use strict;
use warnings;

# figure out what address a symbol resolves to; hackish

#0123456789012\t  \t  \t  \t0123
#    480  f0df				   platlevelclear		; hit end of the level:  clear out all incremental stuff and go to the zeroith platform
#0123456789012\t  \t0123456789
#    484  f0e3		       84 8a		      sty	deltaz

sub symbols {

    my $fn = shift;

    my %symbols;

    open my $fh, '<', $fn or die $!;
    while( my $line = readline $fh ) {
        if( my @line = $line =~ m/^[ 0-9]{7}  ([a-z0-9]{4})\t{4} {3}(\w+)/ ) {
            $symbols{$2} = $1;
        } elsif( my @line = $line =~ m/^[ 0-9]{7} U([a-z0-9]{4})\t\t +00\t {3}(\w+)/ ) {
            $symbols{$2} = $1;
        }
    }
    close $fh;

    return %symbols;

}

1;
