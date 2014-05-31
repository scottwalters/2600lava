
package symbols;

use 5.18.0;

use strict;
use warnings;

# figure out what address a symbol resolves to; hackish

#0123456789012\t  \t  \t  \t0123
#    480  f0df				   platlevelclear		; hit end of the level:  clear out all incremental stuff and go to the zeroith platform
#0123456789012\t  \t0123456789
#    484  f0e3		       84 8a		      sty	deltaz
#     30 U0089		       00 87	   collision_bits =	tmp1
#     31 U0089		       00 88	   collision_platform =	tmp2

#     62 U00ff		       00 e0	   terminal_velocity =	$ff - $1f	; ie -$1f


#      6  f0b8		       84 8d		      sty	collision_bits

sub symbols {

    my $fn = shift;

    my %symbols;

    my @source;

    open my $fh, '<', $fn or die $!;
    while( my $line = readline $fh ) {

        if( $line =~ m/^[ 0-9]{7}  ([a-z0-9]{4})\t{2} *([0-9a-f ]{2,})\t(.*)$/ ) {
            $source[hex($1)] = $3;
            # warn "source: $1 = $3";
        } else {
            # warn "not source: $line";
        }

        if( $line =~ m/^[ 0-9]{7}  ([a-z0-9]{4})\t{4} {3}([\.\w]+)/ ) {
            $symbols{$2} = hex($1);
        } elsif( $line =~ m/^[ 0-9]{7} U([a-z0-9]{4})\t\t +00\t {3}(\w+)/ ) {
            $symbols{$2} = hex($1);
        } elsif( $line =~ m/^[ 0-9]{7} U([a-z0-9]{4})\t\t +00 00 00 00\*(\w+)/ ) { # this shit is just wonky
            $symbols{$2} = hex($1);
        #                                   31 U0089                00            88               collision_platform = tmp2
        } elsif( $line =~ m/^[ 0-9]{7} U([a-z0-9]{4})\t{2} *([0-9a-f]{2}) ([0-9a-f]{2})\t *(\w+)\s*=\s*(.*)$/ ) {
            warn "$4 = $3";
            $symbols{$4} = hex($3);
        }
    }
    close $fh;

    $symbols{source} = \@source; # XXX possible name conflict with the assembly; hackish UI

    package symbols::autoload {

        sub AUTOLOAD {
            my $self = shift;
            my $method = our $AUTOLOAD; $method =~ s/.*:://;
            return if $method eq 'DESTROY';
            exists $self->{$method} or die "no symbol for the method ``$method''";
            $self->{$method};
        }

        sub name_that_location {  
            # XXX possible name conflict with the assembly
            my $self = shift;
            my $loc = shift; 
            my %locations = reverse %$self;
            return $locations{$loc} if $locations{$loc}; 
            # it looks like one more instruction executes after run_cpu() stops things, so try to deal with 1-3 byte instructions that don't branch again to try to figure out which label we tried to stop at
            return $locations{$loc-1} if $locations{$loc-1};
            return $locations{$loc-2} if $locations{$loc-2};
            return $locations{$loc-3} if $locations{$loc-3};
            return 'unknown location';
        }

        return bless \%symbols;
    }

}

sub test {
    use Data::Dumper;
    # print Dumper symbols('newbies.lst');
    symbols('newbies.lst')->view;
}



1;
