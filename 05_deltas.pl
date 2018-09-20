#!/usr/local/bin/perl

# make sure we don't try to render things with a deltay > deltaz

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

my $cycles_per_opcode;
open my $fh, '6502_formatted.txt' or die $!;
while( my $line = readline $fh ) {
    chomp $line;
    my @line = split m/ /, $line;
    @line >= 2 or die $line;
    # warn "$line[0] = $line[1]\n";
    $cycles_per_opcode->[ hex($line[0]) ] = $line[1];
}

sub run_cpu {
    my $extra_cb = pop @_;
    my @stop_symbols = @_;
    my $cycles = 0;
    $cpu->run(10000, sub {
            my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
            # diag sprintf "pc = %x inst = %x a = %s x = %s y = %x", $pc, $inst, $a, $x, $y;
            $cycles_per_opcode->[$inst] or die sprintf( "%2x (%d) has no cycle count", $inst, $inst) . "\n" . Dumper( $cycles_per_opcode );
            $cycles += $cycles_per_opcode->[$inst];
            if( grep $pc == $_, @stop_symbols ) {
                ${ PadWalker::peek_my(1)->{'$ic'} } = 0;
            }
            $extra_cb->(@_);
    });
    return $cycles;
}


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


# here's a troublesome one:

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x00 );
$cpu->write_8( $symbols->playery, 0x20 );
 
$cycles = run_cpu( $symbols->nomoreplatforms, sub {
    my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
    my $deltay = $cpu->read_8( $symbols->deltay );
    my $deltaz = $cpu->read_8( $symbols->deltaz );
    if( name_that_location($pc) eq '.platlinedelta' or name_that_location($pc) eq 'platrenderline1' ) {
         diag sprintf("%2x", $pc) . ' (' . name_that_location($pc) . '): ' . " deltay $deltay vs deltaz $deltaz on platform " . $cpu->read_8( $symbols->curplat ) . "\n";
         ok $deltaz >= $deltay, "deltaz ($deltaz) >= deltay ($deltay)";
    }
    # if( $deltay > $deltaz ) {
    #     warn sprintf("%2x", $pc) . ' (' . name_that_location($pc) . '): ' . " deltay $deltay > deltaz $deltaz on platform " . $cpu->read_8( $symbols->curplat ) . "\n";
    # }
} );

done_testing();
