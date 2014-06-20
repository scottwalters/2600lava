#!/usr/local/bin/perl

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

$symbols->{'.drawenemies'} or die;

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
my $ran_out_of_time = 0;
my $last_intim_value;

#
# register implementations
#

package Register::TIM64T {
    # start a new timer
    use base 'Tie::StdScalar';
    sub TIESCALAR { my $class = shift; $_[0] ||= 0; return bless \$_[0] => $class; }
    sub STORE {
        $timer_cycles = $cycles;
        Test::More::diag "setting timer for $_[1] which gives @{[ 64 * $_[1] ]} cycles\n";
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
         Test::More::diag "fetching timer with $ret left on it at " . $symbols->name_that_location( $cpu->get_pc ) . " $stuff\n";
         $last_intim_value = $ret;

         $ret;
    }
};
tie $cpu->{mem}->[ $symbols->INTIM ], 'Register::INTIM';

#
# run_cpu
#

sub run_cpu {
    my @stop_symbols = @_;
    $ran_out_of_time = 0;

    $cpu->run(100000, sub {
        my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
        # diag sprintf "pc = %x inst = %x a = %s x = %s y = %x", $pc, $inst, $a, $x, $y;
        $cycles_per_opcode->[$inst] or die sprintf( "%2x (%d) has no cycle count", $inst, $inst) . "\n" . Dumper( $cycles_per_opcode );
        $cycles += $cycles_per_opcode->[$inst];
        # if( grep $pc == $symbols->{'platnextline0a'} and $cpu->get_a < 0 ) 
        # if( $cpu->{mem}->[ $symbols->TIM64T ] and $cpu->read_8( $symbols->INTIM ) < 0 ) # generates spam
        if( defined $last_intim_value and $cpu->{mem}->[ $symbols->TIM64T ] and $last_intim_value < 0 ) {
            $ran_out_of_time = 1;
        } 
        if( $ran_out_of_time or grep $pc == $_, @stop_symbols ) {
            ${ PadWalker::peek_my(1)->{'$ic'} } = 0;
        }
    });
    return $cycles;
}


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
# draw an emeny and time it
#

$cpu->write_8( $symbols->playerz, 30 );
$cpu->write_8( $symbols->playery, 0x14+1 );

$cpu->write_8( $symbols->monster1z, 39 );      # same platform, a little further down; he should be straight in front of us
$cpu->write_8( $symbols->monster1y, 0x14+1 );

# $cpu->set_pc($symbols->{'.drawenemies'});
$cpu->set_pc( $symbols->startofframe );     # _drawenemies happens in there (currently) after TIM64T is set

run_cpu( $symbols->{'.drawenemies8'}, );

diag "ran in $cycles cycles";   # 438 cycles; up to 546 with packing sprite data into view; up to 656 with z buffering
ok $cpu->read_8( $symbols->INTIM ) >= 0, "timer didn't go negative";
ok $cpu->read_8( $symbols->INTIM ) != 0, "timer didn't hit zero";

done_testing();
