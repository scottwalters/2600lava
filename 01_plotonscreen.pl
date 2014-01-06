#!/usr/local/bin/perl

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
    $cpu->run(10000, sub {
            my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
            # diag printf "pc = %x inst = %x x = %s", $pc, $inst, $x;
            if( $pc == $symbols->{'.plotonscreen9'} ) {
                ${ PadWalker::peek_my(1)->{'$ic'} } = 0;
            }
    });
}

$cpu->write_8( $symbols->curplat, 0 );  # controls what color the line drawn will be

$cpu->set_pc($symbols->{'.plotonscreen1'} || die);
diag sprintf ".plotonscreen1 = %x\n", $symbols->{'.plotonscreen1'};
diag sprintf ".plotonscreen9 = %x\n", $symbols->{'.plotonscreen9'};

my $expected_width = $cpu->read_8( $symbols->perspectivetable + 10 );
my $expected_color = $cpu->read_8( $symbols->level0 + 4*0 + 3 );

is $cpu->read_8( $symbols->view + 50 ), 0, "is line 50 blank to start with?";

# Y gets the distance, which we use to figure out which size of line to draw
# X gets the scan line to draw at

$cpu->set_y( 10 );   # should result in a line drawn of width 14
$cpu->set_x( 50 );

run_cpu();

my $line = $cpu->read_8( $symbols->view + 50 );
is $line & 0b11100000, $expected_color, "line drawn with expected color";
is $line & 0b00011111, $expected_width, "line drawn with expected width";

sub dump_screen {
    my $view = $symbols->view;
    my $viewsize = 0xff - 2 - $view; # my $viewsize = $symbols->viewsize; 
    for my $i ( 0 .. $viewsize - 1 ) {
        my $line = $cpu->read_8( $view + $i );
        printf "%03d (%02x) %08b\n", $i, $view+$i, $line;
    }
}

# dump_screen();

done_testing();
