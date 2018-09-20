#!/usr/local/bin/perl

# the right most 5 bits of view[] should contain a value from 0-19, otherwise something has gone wrong.

my $available_cycles = 10564;

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
    $cpu->run(100000, sub {
        my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
        if( grep $pc == $_, @stop_symbols ) {
            ${ PadWalker::peek_my(1)->{'$ic'} } = 0;
        }
    });
}

my $view = $symbols->view;
my $viewsize = 0xff - $view;

#
# init
#

$cpu->write_8( $symbols->SWCHB, 0b00000010 ); # select switch off (apparently 0 indicates it is being pressed)

my $level0 = $symbols->level0 or die;
for my $sym (
        1, 11, 0x1e,  0xe0,
        20, 25, 0x14, 0x60,
        30, 40, 0x18, 0x20,
        0, 0, 0, 0,
) {
    $cpu->write_8( $level0++, $sym );
}

$cpu->write_8( $symbols->INTIM, 76 );

my $cycles;

#
# tests
#

# pretty normal case

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x00 );
$cpu->write_8( $symbols->playery, 0x20 );
 
run_cpu( $symbols->nomoreplatforms);

check_lines_drawn();

# one screwed up case observed from manual testing

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0xaa );
$cpu->write_8( $symbols->playery, 0x00 );
 
run_cpu( $symbols->nomoreplatforms);

check_lines_drawn();


#
# subroutines
#

sub check_lines_drawn {
    my $num = 0;
    my $fuckups;
    for my $i ( 0 .. $viewsize - 1 ) {
        my $line = $cpu->read_8( $view + $i );
        my $platform_width = ( $line & 0b00011111);
        if( $platform_width > 19 ) {
            $fuckups++; 
            ok 0, "at line $i, platform width > 19: $platform_width";
        }
    }
    ok 1, "no fuckups" if ! $fuckups;
}

#
#
#

done_testing();

