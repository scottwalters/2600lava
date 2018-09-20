#!/usr/local/bin/perl

# make sure various bits of logic do the right thing with deltaz/deltay

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
    $cpu->run(10000, sub {
            my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
            # diag sprintf "pc = %x inst = %x a = %s x = %s y = %x", $pc, $inst, $a, $x, $y;
            if( grep $pc == $_, @stop_symbols ) {
                ${ PadWalker::peek_my(1)->{'$ic'} } = 0;
            }
    });
}

#
# platrenderline checks the slope first and bails to platnext if deltay > deltaz
#

$cpu->set_pc( $symbols->platrenderline );
$cpu->write_8( $symbols->deltaz, 2 );
$cpu->write_8( $symbols->deltay, 2 );
run_cpu( $symbols->platrenderline1, $symbols->platnext );
is $cpu->get_pc, $symbols->platrenderline1 + 2, "deltaz = 2, deltay = 2 slope";  # +2 because the CPU went one more instruction and it was a two byte instruction?

$cpu->set_pc( $symbols->platrenderline );
$cpu->write_8( $symbols->deltaz, 2 );
$cpu->write_8( $symbols->deltay, 3 );
run_cpu( $symbols->platrenderline1, $symbols->platnext );
is $cpu->get_pc, $symbols->platnext + 2, "deltaz = 2, deltay = 3 slope";

$cpu->set_pc( $symbols->platrenderline );
$cpu->write_8( $symbols->deltaz, 3 );
$cpu->write_8( $symbols->deltay, 2 );
run_cpu( $symbols->platrenderline1, $symbols->platnext );
is $cpu->get_pc, $symbols->platrenderline1 + 2, "deltaz = 3, deltay = 2 slope";

#
# platnextline checks deltaz > 0, and deltaz >= level0,y - playerz 
#

# init

$cpu->write_8( $symbols->INTIM, 20 );

$cpu->write_8( $symbols->curplat, 4 );
$cpu->write_8( $symbols->playerz, 0 );
$cpu->write_8( $symbols->deltay, 2 );

my $level0 = $symbols->level0 or die;
for my $sym (
        1, 11, 0x1e,  0xe0,
        20, 25, 0x14, 0x60,
        30, 40, 0x18, 0x20,
        0, 0, 0, 0,
) {
    $cpu->write_8( $level0++, $sym );
}

# test test:  does SBC work how I think it does?

sub name_that_location {
    my $loc = shift;
    my %locations = reverse %$symbols;
    return $locations{$loc} if $locations{$loc};
    # it looks like one more instruction executes after run_cpu() stops things, so try to deal with 1-3 byte instructions that don't branch again to try to figure out which label we tried to stop at
    return $locations{$loc-1} if $locations{$loc-1};
    return $locations{$loc-2} if $locations{$loc-2};
    return $locations{$loc-3} if $locations{$loc-3};
    return 'unknown location';
}

# tests

$cpu->set_pc( $symbols->platnextline );
$cpu->write_8( $symbols->deltaz, 22 );           # deltaz gets dec'd before the logic runs, so this really compares a deltaz of 21; 21 > ( 20 - 0 ); render this line
run_cpu( $symbols->platrenderline, $symbols->platnext );
# diag "A with deltaz of 21: " . $cpu->get_a; # meaningless; at platrenderline, A gets loaded from deltay, so this is always 2
is name_that_location( $cpu->get_pc ), 'platrenderline'; # less fragile than:  is $cpu->get_pc, $symbols->platrenderline + 2;

$cpu->set_pc( $symbols->platnextline );
$cpu->write_8( $symbols->deltaz, 21 );           # deltaz gets dec'd to 20, but 20 should still be rendered
run_cpu( $symbols->platrenderline, $symbols->platnext );
# diag "A with a deltaz of 20: " . $cpu->get_a;
is name_that_location( $cpu->get_pc ), 'platrenderline';

$cpu->set_pc( $symbols->platnextline );
$cpu->write_8( $symbols->deltaz, 20 );
run_cpu( $symbols->platrenderline, $symbols->platnext );
is name_that_location( $cpu->get_pc ), 'platnext';

$cpu->set_pc( $symbols->platnextline );
$cpu->write_8( $symbols->deltaz, 19 );
run_cpu( $symbols->platrenderline, $symbols->platnext );
is name_that_location( $cpu->get_pc ), 'platnext';

$cpu->set_pc( $symbols->platnextline );
$cpu->write_8( $symbols->deltaz, 18 );
run_cpu( $symbols->platrenderline, $symbols->platnext );
is name_that_location( $cpu->get_pc ), 'platnext';

done_testing();
