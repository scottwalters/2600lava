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

diag sprintf ".plotonscreen1 = %x\n", $symbols->{'.plotonscreen1'};
diag sprintf ".plotonscreen9 = %x\n", $symbols->{'.plotonscreen9'};

my $cpu = Acme::6502->new();
$cpu->load_rom( 'newbies.bin', 0xf000 );

sub run_cpu {
    $cpu->run(10000, sub {
            my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
            # diag sprintf "pc = %x inst = %x x = %s", $pc, $inst, $x;
            if( $pc == $symbols->{'.plotonscreen9'} ) {
                ${ PadWalker::peek_my(1)->{'$ic'} } = 0;
            }
    });
}

my $view = $symbols->view;
my $viewsize = 0xff - 2 - $view;

#
# draw one initial line
#

diag 'drawing one initial line';

$cpu->write_8( $symbols->curplat, 0 );  # controls what color the line drawn will be
$cpu->write_8( $symbols->lastline, 0 );

$cpu->write_8( $symbols->SWCHB, 0b00000010 ); # select switch off (apparently 0 indicates it is being pressed)

ok my $expected_width = $cpu->read_8( $symbols->perspectivetable + 10 ), 'there is an expected width';
ok my $expected_color = $cpu->read_8( $symbols->level0 + 0 + 3 ), 'there is an expected color';

is $cpu->read_8( $symbols->view + 50 ), 0, "is line 50 blank to start with?";

# Y gets the distance, which we use to figure out which size of line to draw
# X gets the scan line to draw at

$cpu->set_pc($symbols->{'.plotonscreen1'} || die);

$cpu->set_y( 10 );
$cpu->set_x( 50 );

run_cpu();

my $line = $cpu->read_8( $symbols->view + 50 );
is $line & 0b11100000, $expected_color, "line drawn with expected color";
is $line & 0b00011111, $expected_width, "line drawn with expected width";

is $cpu->read_8( $symbols->lastline ), 50, "lastline recorded";

is count_lines_drawn(), 1, "one line drawn";
is count_lines_drawn($expected_color), 1, "and it's the right color";

#
# draw a second line as part of the same platform
#

diag 'second line in the same platform';

$cpu->write_8( $symbols->INTIM, 10 );  # enough time left on the timer

ok $expected_width = $cpu->read_8( $symbols->perspectivetable + 9 ), 'there is an expected width';

$cpu->set_pc($symbols->{'.plotonscreen1'} || die);

$cpu->set_y( 9 );
$cpu->set_x( 53 );

run_cpu();

$line = $cpu->read_8( $symbols->view + 53 );
is $line & 0b11100000, $expected_color, "line drawn with expected color";
is $line & 0b00011111, $expected_width, "line drawn with expected width";

is $cpu->read_8( $symbols->lastline ), 53, "lastline recorded";

is count_lines_drawn(), 4, "four lines drawn";
is count_lines_drawn_by_color($expected_color), 4, "and they're the right color";

#
# draw a third line as part of the same platform
#

diag 'third line in the same platform';

ok $expected_width = $cpu->read_8( $symbols->perspectivetable + 8 ), 'there is an expected width';

$cpu->set_pc($symbols->{'.plotonscreen1'} || die);

$cpu->set_y( 8 );
$cpu->set_x( 57 );

run_cpu();

$line = $cpu->read_8( $symbols->view + 57 );
is $line & 0b11100000, $expected_color, "line drawn with expected color";
is $line & 0b00011111, $expected_width, "line drawn with expected width";

is $cpu->read_8( $symbols->lastline ), 57, "lastline recorded";

is count_lines_drawn(), 8, "eight lines drawn";
is count_lines_drawn_by_color($expected_color), 8, "and they're the right color";

#
# try to draw a 2nd platform over top of part of the first where Z buffering should not allow it to be drawn
#

diag 'drawing a line from a second platform that should be further away in the Z buffer';

$cpu->write_8( $symbols->curplat, 4 );  # controls what color the line drawn will be; this number is a multiple of four
$cpu->write_8( $symbols->lastline, 0 );

ok my $expected_width_2 = $cpu->read_8( $symbols->perspectivetable + 17 ), 'there is an expected width'; # 17 in is width 8 right now by the way
ok my $expected_color_2 = $cpu->read_8( $symbols->level0 + 4 + 3 ), 'there is an expected color';
isnt $expected_color, $expected_color_2, "second platformis to be a different color than the first";

$cpu->set_pc($symbols->{'.plotonscreen1'} || die);

$cpu->set_y( 17 ); # much further away
$cpu->set_x( 52 ); # previously, lines 50-57 have been plotted; aim for somewhere in the middle of that

run_cpu();

$line = $cpu->read_8( $symbols->view + 52 );
isnt $line & 0b11100000, $expected_color_2, "line not drawn with requested color";
isnt $line & 0b00011111, $expected_width_2, "line not drawn with requested width";

is $cpu->read_8( $symbols->lastline ), 52, "lastline not updated";

is count_lines_drawn(), 8, "still eight lines drawn";
is count_lines_drawn_by_color($expected_color), 8, "and they're the right color (that of the first platform)";
ok ! count_lines_drawn_by_color($expected_color_2), "no lines drawn with the second platforms color";

#
# try to draw a 3rd platform over top of part of the first where Z buffering should allow it to be drawn
#

diag 'drawing a line from a third platform that should be closer in the Z buffer';

$cpu->write_8( $symbols->curplat, 8 );  # controls what color the line drawn will be; this number is a multiple of four
$cpu->write_8( $symbols->lastline, 0 );

ok my $expected_width_3 = $cpu->read_8( $symbols->perspectivetable + 7 ), 'there is an expected width';
ok my $expected_color_3 = $cpu->read_8( $symbols->level0 + 8 + 3 ), 'there is an expected color';
isnt $expected_color, $expected_color_2, "second platformis to be a different color than the first";

$cpu->set_pc($symbols->{'.plotonscreen1'} || die);

$cpu->set_y( 7 ); # closer
$cpu->set_x( 54 ); # previously, lines 50-57 have been plotted; aim for somewhere in the middle of that

run_cpu();

$line = $cpu->read_8( $symbols->view + 54 );
is $line & 0b11100000, $expected_color_3, "line drawn with requested color";
is $line & 0b00011111, $expected_width_3, "line drawn with requested width";

is $cpu->read_8( $symbols->lastline ), 54, "lastline updated";

is count_lines_drawn(), 8, "still eight lines drawn";
is count_lines_drawn_by_color($expected_color), 7, "7 of them are the color of the first platform";
is count_lines_drawn_by_color($expected_color_3), 1, "one lines drawn with the third platforms color";

#
# add a second line to the 3rd platform
#

diag 'drawing a line from a second line on the third platform';

$cpu->set_pc($symbols->{'.plotonscreen1'} || die);

$cpu->set_y( 7 ); # closer still
$cpu->set_x( 60 ); # draw down below what's been drawn so far

run_cpu();

$line = $cpu->read_8( $symbols->view + 60 );
is $line & 0b11100000, $expected_color_3, "line drawn with requested color";

is $cpu->read_8( $symbols->lastline ), 60, "lastline updated";

is count_lines_drawn(), 11, "eleven lines drawn now";
is count_lines_drawn_by_color($expected_color), 4, "4 of them are the color of the first platform";
is count_lines_drawn_by_color($expected_color_3), 7, "seven lines drawn with the third platforms color now";


#
# util routines
#

sub count_lines_drawn {
    my $num = 0;
    for my $i ( 0 .. $viewsize - 1 ) {
        $num++ if $cpu->read_8( $view + $i );
    }
    return $num;
}

sub count_lines_drawn_by_color {
    my $num = 0;
    my $color = shift;
    for my $i ( 0 .. $viewsize - 1 ) {
        my $line = $cpu->read_8( $view + $i );
        $num++ if $color == ( $line & 0b11100000 );
    }
    return $num;
}


sub dump_screen {
    for my $i ( 0 .. $viewsize - 1 ) {
        my $line = $cpu->read_8( $view + $i );
        diag sprintf "%03d (%02x) %08b\n", $i, $view+$i, $line;
    }
}

# dump_screen();

done_testing();
