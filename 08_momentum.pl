#!/usr/local/bin/perl

# test _gamelogic, which applies momentum

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
        1, 11, 0x1e,  0xe0,          # 0 (0)
        20, 25, 0x14, 0x60,          # 1 (4)
        30, 40, 0x18, 0x20,          # 2 (8)
        0, 0, 0, 0,
) {
    $cpu->write_8( $level0++, $sym );
}

#
# test Z momentum not being enough to move the player forward one unit
#

$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14+5 );

$cpu->write_8( $symbols->playerzlo, 0x70 );
$cpu->write_8( $symbols->playerzspeed, 0x05 );

$cpu->write_8( $symbols->playerylo, 0x00 );
$cpu->write_8( $symbols->playeryspeed, 0x00 );

$cpu->set_pc( $symbols->gamelogic0);
run_cpu( $symbols->gamelogic4);

is $cpu->read_8( $symbols->playerz), 20;        # unchanged
is $cpu->read_8( $symbols->playery), 0x14+5;    # unchanged

#
# test Z momentum moving the player forward one unit
#

$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14+5 );

$cpu->write_8( $symbols->playerzlo, 0x70 );
$cpu->write_8( $symbols->playerzspeed, 0x70 );  # adding those together should carry and increase playerz from 20 to 21

$cpu->write_8( $symbols->playerylo, 0x00 );
$cpu->write_8( $symbols->playeryspeed, 0x00 );

$cpu->set_pc( $symbols->gamelogic0);
run_cpu( $symbols->gamelogic4);

is $cpu->read_8( $symbols->playerz), 21;        # changed
is $cpu->read_8( $symbols->playery), 0x14+5;    # unchanged

# diag sprintf "playerzlo: %x\n", $cpu->read_8( $symbols->playerzlo);

#
# XXX test Z momentum failing to move the player forward one unit because the space is occupied
#

# XXX need to call into collision detection and have it fall through to gamelogic/momentum

#
# test Z momentum moving the player backwards one unit
#

$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14+5 );

$cpu->write_8( $symbols->playerzlo, 0x8f );
$cpu->write_8( $symbols->playerzspeed, 0x8f );  # added together, they're 286.  adding those should carry and decrease playerz from 20 to 19.

$cpu->write_8( $symbols->playerylo, 0x00 );
$cpu->write_8( $symbols->playeryspeed, 0x00 );

$cpu->set_pc( $symbols->gamelogic0);
run_cpu( $symbols->gamelogic4);

is $cpu->read_8( $symbols->playerz), 19;         # changed
is $cpu->read_8( $symbols->playery), 0x14+5;    # unchanged

#
# test Y momentum moving the player forward up unit
#

$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14+5 );

$cpu->write_8( $symbols->playerylo, 0x70 );
$cpu->write_8( $symbols->playeryspeed, 0x70 );  # adding those together should carry and increase playery from 25 to 26

$cpu->write_8( $symbols->playerzlo, 0x00 );
$cpu->write_8( $symbols->playerzspeed, 0x00 );

$cpu->set_pc( $symbols->gamelogic0);
run_cpu( $symbols->gamelogic4);

is $cpu->read_8( $symbols->playerz), 20;        # unchanged
is $cpu->read_8( $symbols->playery), 0x14+6;    # changed



#
# 
#

done_testing();
