#!/usr/local/bin/perl

# test collision detection logic

use strict;
use warnings;

use Test::More;

use Acme::6502;

use lib '.';
use symbols;

use Data::Dumper;
use Carp; $SIG{__DIE__} = sub { Carp::confess @_ };

use PadWalker;

my $symbols = symbols::symbols('newbies.lst');

my $cpu = Acme::6502->new();
$cpu->load_rom( 'newbies.bin', 0xf000 );

sub run_cpu {
    my @stop_symbols = @_;
    my $cycles = 0;
    $cpu->run(10000, sub {
        my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
        my $name = name_that_location($pc);
        # diag $name if $name !~ m/unknown/;
        # diag sprintf "pc = %x inst = %x a = %s x = %s y = %x", $pc, $inst, $a, $x, $y;
        if( grep $pc == $_, @stop_symbols ) {
            ${ PadWalker::peek_my(1)->{'$ic'} } = 0;
        }
    });
    return $cycles;
}

sub name_that_location {
    my $loc = shift;
    my %locations = reverse %$symbols;
    return $locations{$loc} if $locations{$loc};
    return $locations{$loc-1} if $locations{$loc-1};
    return $locations{$loc-2} if $locations{$loc-2};
    return $locations{$loc-3} if $locations{$loc-3};
    return 'unknown location';
}


#
# init
#

my $level0 = $symbols->level0 or die;
for my $sym (
        1, 11, 0x1e,  0xe0,
        20, 25, 0x14, 0x60,
        30, 40, 0x18, 0x20,
        0, 0, 0, 0,
) {
    $cpu->write_8( $level0++, $sym );
}

# test standing on a platform

$cpu->set_pc( $symbols->collisions);
$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14+1 );
 
run_cpu( $symbols->collisions1a, $symbols->collisions9 );

my $loc = name_that_location( $cpu->get_pc );
is $loc, 'collisions1a', 'stopped at collisions1a, where tmp2 is written, not collisions9, where it gave up and existed';

is $cpu->read_8( $symbols->tmp2 ), 4, 'collision logic decided that we are standing on the second platform which has index 4';

done_testing();
