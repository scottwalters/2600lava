#!/usr/local/bin/perl

# how many times is the 64 cycle timer observed in each position while rendering?
# looks like it falls between 4 and 7 per call to plotonscreen
# the largest drop, 7, it draw one line segment then gap filled 6 segments, so it apparently takes around 64 cycles to fill one bit of gap

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

$symbols->{'.plot_down1'} or die;
$symbols->{'.plot_up1'} or die;

my $cpu = Acme::6502->new();
$cpu->load_rom( 'newbies.bin', 0xf000 );

#
# opcode cycle counts
#

my $cycles_per_opcode;
open my $fh, '6502_formatted.txt' or die $!;
while( my $line = readline $fh ) {
    chomp $line;
    my @line = split m/ /, $line;
    @line >= 2 or die $line;
    # warn "$line[0] = $line[1]\n";
    $cycles_per_opcode->[ hex($line[0]) ] = $line[1];
}

my $timer_cycles = 0;  # number of cycles in $cycles when the time was last restarted
my $cycles = 0;

#
# register implementations
#

package Register::TIM64T {
    # start a new timer
    use base 'Tie::StdScalar';
    sub TIESCALAR { my $class = shift; $_[0] ||= 0; return bless \$_[0] => $class; }
    sub STORE {
        $timer_cycles = $cycles;
        warn "setting timer for $_[1] which gives @{[ 64 * $_[1] ]} cycles\n";
        ${$_[0]} = $_[1];
    }
};
tie $cpu->{mem}->[ $symbols->TIM64T ], 'Register::TIM64T';

package Register::INTIM {
    # read the timer
    use base 'Tie::StdScalar';
    sub TIESCALAR { my $class = shift; $_[0] ||= 0; return bless \$_[0] => $class; }
    sub FETCH {
         my $ret = $cpu->{mem}->[ $symbols->TIM64T ] - int( ( $cycles - $timer_cycles ) / 64 );
         my $stuff = join ' ', map { $_ . '=' . $cpu->read_8( $symbols->$_ ) } qw/curplat deltaz lastline/;
         warn "fetching timer with $ret left on it at " . $symbols->name_that_location( $cpu->get_pc ) . " $stuff\n";

         $ret;
    }
};
tie $cpu->{mem}->[ $symbols->INTIM ], 'Register::INTIM';

#
# run_cpu
#

sub run_cpu {
    my @stop_symbols = @_;

    $cpu->run(100000, sub {
        my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
        # diag sprintf "pc = %x inst = %x a = %s x = %s y = %x", $pc, $inst, $a, $x, $y;
        $cycles_per_opcode->[$inst] or die sprintf( "%2x (%d) has no cycle count", $inst, $inst) . "\n" . Dumper( $cycles_per_opcode );
        $cycles += $cycles_per_opcode->[$inst];
        if( grep $pc == $_, $symbols->{'.plot_down1'}, $symbols->{'.plot_up1'} ) {
           $_ = $cpu->read_8( $symbols->INTIM ); # trigger a message from reading the tied memory location
        } 
        if( grep $pc == $_, @stop_symbols ) {
            ${ PadWalker::peek_my(1)->{'$ic'} } = 0;
        }
    });
    return $cycles;
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

#
# tests
#

#
# make sure that it doesn't run out of time, as set up
#

$cpu->write_8( $symbols->TIM64T, 76 );

diag "code is checking timer against this constant: " . $cpu->read_8( $symbols->platnextline0a+1 );

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x00 );
$cpu->write_8( $symbols->playery, 0x1F );

run_cpu( $symbols->vblanktimerendalmost, $symbols->ranoutoftime );

diag "stopped at symbol " .  $symbols->name_that_location( $cpu->get_pc );
ok $symbols->name_that_location( $cpu->get_pc ) ne 'ranoutoftime', "didn't stop on the 'ranoutoftime' label";
ok $symbols->name_that_location( $cpu->get_pc ) eq 'vblanktimerendalmost', "did stop on the 'vblanktimerendalmost' label";

diag "ran in $cycles cycles";
ok $cpu->read_8( $symbols->INTIM ) >= 0, "timer didn't go negative";

#
# test other values to try to mess it up
#

$cpu->write_8( $symbols->TIM64T, 76 );

$cpu->write_8( $symbols->platnextline0a+1, 17 );  # this is the INTIM timer expired test; according to the above (currently), this should be enough; it has 18 left when it hits vblanktimerendalmost
diag "code is checking timer against this constant: " . $cpu->read_8( $symbols->platnextline0a+1 );

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x00 );
$cpu->write_8( $symbols->playery, 0x1F );

run_cpu( $symbols->vblanktimerendalmost, $symbols->ranoutoftime );

diag "stopped at symbol " .  $symbols->name_that_location( $cpu->get_pc );
ok $symbols->name_that_location( $cpu->get_pc ) ne 'ranoutoftime', "didn't stop on the 'ranoutoftime' label";
ok grep( $_ eq $symbols->name_that_location( $cpu->get_pc ), 'vblanktimerendalmost', 'vblanktimerendalmost1'), "did stop on the 'vblanktimerendalmost' label (or something close)";

diag "ran in $cycles cycles";
ok $cpu->read_8( $symbols->INTIM ) >= 0, "timer didn't go negative";



done_testing();
