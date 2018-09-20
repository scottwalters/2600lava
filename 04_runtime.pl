#!/usr/local/bin/perl

# do some basic checks on how long it takes to render things

use strict;
use warnings;

use Test::More;
use Acme::6502;
use Tie::Scalar;

use lib '.';
use symbols;

use Data::Dumper;
use Carp; $SIG{__DIE__} = sub { Carp::confess @_ };

use PadWalker;

my $symbols = symbols::symbols('newbies.lst');

my $viewsize = $symbols->viewsize or die;

# 30 lines are overscan
# get some more during vblank but because other stuff happens in vblank, I'm not sure how much
# have to deduct at least 64 cycles (1 tick of the TIM64T) for every time we test INTIM since when we catch the timer at 0 can vary by thoes 64 cycles
# observing vblankrendermore occupring on scanline 11 or 12, and vblankdonerendering/vblankburntimer on 33, so that's 21 lines of additional rendering

my $available_cycles = 76 * ( 30 + 192 - $viewsize ) + 76 * 14 - 64 * 2;     # XXX not automatically extracting the vblank timer value

my $cycles = 0;        # absolute number of elapsed cycles since the CPU was started
my $timer_cycles = 0;  # number of cycles in $cycles when the timer was last set

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
    $cycles_per_opcode->[ $op ] += 2 if defined $line[2] and $line[2] =~ m/\+2/;  # assume the worst case
}

sub run_cpu {

    my @stop_symbols = @_;

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
# register implementations
#

package Register::TIM64T {
    # start a new timer
    use base 'Tie::StdScalar';
    sub TIESCALAR { my $class = shift; $_[0] ||= 0; return bless \$_[0] => $class; }
    sub STORE {
        $timer_cycles = $cycles;
        Test::More::diag "setting TIM64T for $_[1] which gives @{[ 64 * $_[1] ]} cycles\n";
        ${$_[0]} = $_[1];
    }
};
tie $cpu->{mem}->[ $symbols->TIM64T || die ], 'Register::TIM64T';

package Register::INTIM {
    # read the timer
    use base 'Tie::StdScalar';
    sub TIESCALAR { my $class = shift; $_[0] ||= 0; return bless \$_[0] => $class; }
    sub FETCH {
         my $ret = $cpu->{mem}->[ $symbols->TIM64T ] - int( ( $cycles - $timer_cycles ) / 64 );
         Test::More::diag "fetching timer with $ret left on it at " . $symbols->name_that_location( $cpu->get_pc );
         $ret;
    }
};
tie $cpu->{mem}->[ $symbols->INTIM || die ], 'Register::INTIM';


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

diag "processingtimer = " . $symbols->processingtimer;
$cpu->write_8( $symbols->INTIM, $symbols->processingtimer );

#
# tests
#

sub do_a_test {

    $cycles = 0;

    $cpu->write_8( $symbols->playerz, 0x00 );
    $cpu->write_8( $symbols->playery, 0x20 );

    diag "calling renderplatforms:";
    $cpu->set_pc( $symbols->renderplatforms );
    run_cpu( $symbols->nomoreplatforms );
    ok $cpu->read_8( $symbols->INTIM ) >= 0, "timer didn't go negative";  # apparently does return signed values
    diag "done in $cycles cycles";

    ok $cycles < $available_cycles, "finishes in less than $available_cycles cycles";

}

# troublesome one
$cpu->write_8( $symbols->playerz, 0x00 );
$cpu->write_8( $symbols->playery, 0x20 );
do_a_test();

# troublesome one
$cpu->write_8( $symbols->playerz, 0x02 );
$cpu->write_8( $symbols->playery, 0x1d );
do_a_test();
 
# troublesome one
$cpu->write_8( $symbols->playerz, 0x00 );
$cpu->write_8( $symbols->playery, 0x10 );
do_a_test();
 
# starting view (looking out over a platform, with $39 = 57 bits of gap filled)
$cpu->write_8( $symbols->playerz, 0x02 );
$cpu->write_8( $symbols->playery, 0x20 );
do_a_test();
 
# a view with only $0f gaps filled
$cpu->write_8( $symbols->playerz, 0x02 );
$cpu->write_8( $symbols->playery, 0x18 );
do_a_test();

# and another troublesome one:
$cpu->write_8( $symbols->playerz, 0x03 );
$cpu->write_8( $symbols->playery, 0x1f );
do_a_test();
 
# and another troublesome one:
$cpu->write_8( $symbols->playerz, 0x00 );
$cpu->write_8( $symbols->playery, 0x1d );
do_a_test();
 
done_testing();
