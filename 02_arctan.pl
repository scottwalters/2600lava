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

diag sprintf ".platlinedelta= %x\n", $symbols->{'.platlinedelta'};
diag sprintf ".platarctan9= %x\n", $symbols->{'.platarctan9'};

my $cpu = Acme::6502->new();
$cpu->load_rom( 'newbies.bin', 0xf000 );

sub run_cpu {
    my $stop_symbol = shift;
    $cpu->run(10000, sub {
            my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
            # diag sprintf "pc = %x inst = %x a = %s x = %s y = %x", $pc, $inst, $a, $x, $y;
            if( $pc == $symbols->{$stop_symbol} ) {
                ${ PadWalker::peek_my(1)->{'$ic'} } = 0;
            }
    });
}

my $viewsize = 0xff - 2 - $symbols->view; 
my $half_way_down_screen = int( $viewsize / 2 );

# this is based on the perl embedded in newbies.asm for generating the 'arctangent' table

use Math::Trig;
my $field_of_view_in_angles = 90;
my $multiplier = $viewsize / $field_of_view_in_angles;
sub scanline_according_to_perl {
    my $y = shift;
    my $z = shift;
    my $abs_y = abs $y;
    return $half_way_down_screen if $y == 0;
    my $angle = int(rad2deg(atan($abs_y/$z))*$multiplier);
    # diag "angle $angle";
    # return $y < 0 ? $half_way_down_screen - $angle : $half_way_down_screen + $angle; # the 6502 version flipped the framebuffer upside down
    return $y < 0 ? $half_way_down_screen + $angle : $half_way_down_screen - $angle;
}

# deltay could be -127 to +127
# deltaz will always be >= 1 (in front of us)
# abs(deltay) will always be < deltaz

# for(1..20) { print int(rand 256) - 127, ', ', 1 + int rand 127, "\n" }

for my $deltas (
    [-64, 105],
    [64, 105],
    [-26, 71],
    [-50, 55],
    [4, 101],
    [83, 94],
    [-8, 41],
    [-19, 94],
    [-21, 70],
    [-58, 118],
    [0, 30],     # test special case where Y = 0
) {
    my $deltay = $deltas->[0];
    my $deltaz = $deltas->[1];
    diag "=============== deltay $deltay deltaz $deltaz ===============";

    my $scanline_according_to_perl = scanline_according_to_perl( $deltay, $deltaz );
    diag "scanline_according_to_perl $scanline_according_to_perl";

    my $truncated_deltay = abs($deltay);
    my $truncated_deltaz = $deltaz - $truncated_deltay;
    while( $truncated_deltay > 0x0f or $truncated_deltaz > 0x0f ) {
        $truncated_deltay >>= 1;
        $truncated_deltaz >>= 1;
    }
    # my $truncated_scanline_according_to_perl = scanline_according_to_perl( $truncated_deltay, $truncated_deltaz );
    diag "truncated deltay $truncated_deltay truncated deltaz $truncated_deltaz";
    my $looked_up_arctan = $cpu->read_8( $symbols->arctangent + ( ( $truncated_deltay << 4 ) | $truncated_deltaz ) , $deltay );
    # my $half_way_down_screen_plus_looked_up_arctan = ( $half_way_down_screen + ( $deltay < 0 ? - $looked_up_arctan : $looked_up_arctan ) );
    my $half_way_down_screen_plus_looked_up_arctan = ( $half_way_down_screen + ( $deltay < 0 ? $looked_up_arctan : - $looked_up_arctan ) ); # the 6502 version flipped the framebuffer upside down
    diag "looked_up_arctan + half_way_down_screen = $half_way_down_screen_plus_looked_up_arctan";

    if( $deltay < 0 ) {
        $deltay = 256 + $deltay;  # checked and confirmed correct
        # diag "deltay 2's compliment is $deltay";
    }

    $cpu->write_8( $symbols->deltay, $deltay );
    $cpu->write_8( $symbols->deltaz, $deltaz );
    $cpu->set_pc($symbols->{'.platlinedelta'} || die);
    run_cpu('.platarctan9');
    my $scanline_according_to_6502 = $cpu->get_a;
    diag "scanline_according_to_6502 $scanline_according_to_6502";

    is $scanline_according_to_6502, $half_way_down_screen_plus_looked_up_arctan, "we're able to replicate what the 6502 did";
    ok abs($scanline_according_to_6502 - $scanline_according_to_perl) < 6, "what the 6502 came up with is within 6 scanlines of accurate";
    ok abs($scanline_according_to_6502 - $scanline_according_to_perl) < 10, "what the 6502 came up with is within 10 scanlines of accurate";

}

done_testing();
