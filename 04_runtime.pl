#!/usr/local/bin/perl

# do some basic checks on how long it takes to render things

# 76*(30+37+3) +            76 cycles per line, 30 lines are overscan, 3 are vsync, 37 are vblank
# 76*(192-viewsize+11) =    192 display scanlines but viewsize is 122 and we waste about 11 lines XXX on other things (rendering enemies)
# ~10564                    total number of cycles we have to work with  XXX waiting on nearly expired timers eats some of this up -- compute that

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

my $viewsize = $symbols->viewsize or die;

my $available_cycles = 76 * ( 30 + 37 + 3 ) + 76 * ( 192 - $viewsize + 11 );
diag "available_cycles = $available_cycles";


my $cpu = Acme::6502->new();
$cpu->load_rom( 'newbies.bin', 0xf000 );

my $cycles_per_opcode;
open my $fh, '6502_formatted.txt' or die $!;
while( my $line = readline $fh ) {
    chomp $line;
    my @line = split m/ /, $line, 3;
    @line >= 2 or die $line;
    my $op = hex($line[0]);
    $cycles_per_opcode->[ $op ] = $line[1];
    $cycles_per_opcode->[ $op ] += 1 if defined $line[2] and $line[2] =~ m/\+1/;  # assume the worst case, that we're crossing page boundaries or taking branches
    $cycles_per_opcode->[ $op ] += 1 if defined $line[2] and $line[2] =~ m/\+2/;  # assume the worst case
}

sub run_cpu {
    my @stop_symbols = @_;
    my $cycles = 0;
    my $plot_same_line_count = 0;
    my $lines_of_gap_filled = 0;
    my $times_plot_simple_is_called = 0;
    my $times_plot_on_screen_called = 0;
    $cpu->run(100000, sub {
        my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
        # diag sprintf "pc = %x inst = %x a = %s x = %s y = %x", $pc, $inst, $a, $x, $y;
        $cycles_per_opcode->[$inst] or die sprintf( "%2x (%d) has no cycle count", $inst, $inst) . "\n" . Dumper( $cycles_per_opcode );
        $cycles += $cycles_per_opcode->[$inst];
        if( $pc == $symbols->{'.plotonscreen'} ) {
            $times_plot_on_screen_called++;
        } 
        if( $pc == $symbols->{'.plot0'} ) {
            $plot_same_line_count++;
        } 
        if( $pc == $symbols->{'.plot_simple'} ) {
            $times_plot_simple_is_called++;
        } 
        if( grep $pc == $_, $symbols->{'.plot_down1'}, $symbols->{'.plot_up1'} ) {
            $lines_of_gap_filled++;
        }
        if( grep $pc == $_, @stop_symbols ) {
            diag "$times_plot_on_screen_called calls to _plot_on_screen";
            diag "$plot_same_line_count attempts were made to draw over the same line that we just drew";
            diag "$times_plot_simple_is_called calls to .plot_simple";
            diag "$lines_of_gap_filled lines of gap filled";
            ${ PadWalker::peek_my(1)->{'$ic'} } = 0;
        }
    });
    return $cycles;
}

#
# check how long it takes to render different perspectives
#

# init

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

diag "supposedly, there are 5320 machine cycles total available during vblank and overscan";

# here's a troublesome one:

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x00 );
$cpu->write_8( $symbols->playery, 0x20 );
 
$cycles = run_cpu( $symbols->vblanktimerendalmost );

diag "ran in $cycles cycles"; # 10744
ok $cycles < $available_cycles, "finishes in less than $available_cycles cycles";

# and another troublesome one:

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x02 );
$cpu->write_8( $symbols->playery, 0x1d );
 
$cycles = run_cpu( $symbols->vblanktimerendalmost );

diag "ran in $cycles cycles";
ok $cycles < $available_cycles, "finishes in less than $available_cycles cycles";


# and a made up one

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x00 );
$cpu->write_8( $symbols->playery, 0x10 );
 
$cycles = run_cpu( $symbols->vblanktimerendalmost );

diag "ran in $cycles cycles";
ok $cycles < $available_cycles, "finishes in less than $available_cycles cycles";


# starting view (looking out over a platform, with $39 = 57 bits of gap filled)

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x02 );
$cpu->write_8( $symbols->playery, 0x20 );
 
$cycles = run_cpu( $symbols->vblanktimerendalmost );

diag "ran in $cycles cycles";
ok $cycles < $available_cycles, "finishes in less than $available_cycles cycles";


# a view with only $0f gaps filled

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x02 );
$cpu->write_8( $symbols->playery, 0x18 );
 
$cycles = run_cpu( $symbols->vblanktimerendalmost );

diag "ran in $cycles cycles";
ok $cycles < $available_cycles, "finishes in less than $available_cycles cycles";


# and another troublesome one:

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x03 );
$cpu->write_8( $symbols->playery, 0x1f );
 
$cycles = run_cpu( $symbols->vblanktimerendalmost );

diag "ran in $cycles cycles";
ok $cycles < $available_cycles, "finishes in less than $available_cycles cycles";


# and another troublesome one:

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x00 );
$cpu->write_8( $symbols->playery, 0x1d );
 
$cycles = run_cpu( $symbols->vblanktimerendalmost );

diag "ran in $cycles cycles";
ok $cycles < $available_cycles, "finishes in less than $available_cycles cycles";



done_testing();
