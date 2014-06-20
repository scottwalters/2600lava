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

$symbols->{'.drawenemies'} or die;

my $cpu = Acme::6502->new();
$cpu->load_rom( 'newbies.bin', 0xf000 );

$cpu->write_8( $symbols->INTIM, 76 );   # stuff the timer

my $view = $symbols->view;
my $viewsize = $symbols->viewsize;
my $half_way_down_screen = int( $viewsize / 2 );

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
# draw an emeny
#

#
# let it compute deltaz and deltay first
#

$cpu->write_8( $symbols->playerz, 30 );
$cpu->write_8( $symbols->playery, 0x14+1 );

$cpu->write_8( $symbols->monster1z, 39 );      # same platform, a little further down; he should be straight in front of us
$cpu->write_8( $symbols->monster1y, 0x14+1 );

$cpu->set_pc($symbols->{'.drawenemies'});
$symbols->run_cpu( cpu => $cpu, stop => [ '.drawenemies0c', ], );

# A is the scanline, X is 0..2 depending on range

is $cpu->read_8( $symbols->deltay ), 0;
is $cpu->read_8( $symbols->deltaz ), 9;

is $cpu->read_8( $symbols->view + $cpu->get_a ), 0, "is line 50 blank to start with?";

#
# then let _arctan run
#

$symbols->run_cpu( cpu => $cpu, stop => [ '.drawenemies0d', '.drawenemies8', ], );

diag "scanline = " . $cpu->read_8( $symbols->lastline );
diag "A = " . $cpu->get_a;

is $cpu->read_8( $symbols->lastline ), $cpu->get_a;

is $symbols->name_that_location( $cpu->get_pc ), '.drawenemies0d', "did stop on the '.drawenemies0d' label";
isnt $symbols->name_that_location( $cpu->get_pc ), '.drawenemies8', "did not stop on the '.drawenemies8' label";  # happens when that enemy is outside of the field of view; shouldn't happen in this case

is $cpu->get_a, int( $viewsize / 2 );

my $scanline_according_to_perl = scanline_according_to_perl( $cpu->read_8( $symbols->deltay ), $cpu->read_8( $symbols->deltaz ) );  # essentially the same as immediately above but less crude
diag "scanline_according_to_perl $scanline_according_to_perl";
is $cpu->get_a, $scanline_according_to_perl;

#
# then let _plathypot run
#

$symbols->run_cpu( cpu => $cpu, stop => [ '.drawenemies0e', ], );

diag "line width = " . $cpu->get_a;

#
# compute sprite width
#

$symbols->run_cpu( cpu => $cpu, stop => [ '.drawenemies1', ], );

my $sprite_size = $cpu->get_y;
diag "sprite size = $sprite_size";

#
# compute enemy starting scanline (height mixed in)
#

$symbols->run_cpu( cpu => $cpu, stop => [ '.drawenemies1a', ], );

my $enemy_height = $cpu->read_8( $symbols->enemyheights + $sprite_size );
diag "enemy height should be $enemy_height";
diag "enemy start scanline = " . $cpu->get_a;
is $cpu->get_a, $enemy_height + $scanline_according_to_perl;

#
# start looping over lines
#

$symbols->run_cpu( cpu => $cpu, stop => [ '.drawenemies2', '.drawenemies7', '.drawenemies8' ], );

diag "Y: " . $cpu->get_y;

is $symbols->name_that_location( $cpu->get_pc ), '.drawenemies2', "did stop on the '.drawenemies2' label";    # happens when it loops around to draw another line of enemy data
isnt $symbols->name_that_location( $cpu->get_pc ), '.drawenemies7', "did not stop on the '.drawenemies7' label";  # happens when the start of the enemy to draw is off the top of the screen
isnt $symbols->name_that_location( $cpu->get_pc ), '.drawenemies8', "did not stop on the '.drawenemies8' label";  # happens when we've drawn all the way to the bottom of the enemy

ok $cpu->get_y > $cpu->read_8( $symbols->lastline ), "haven't made it down (up) to lastline yet";

#
# loop more
#

my $iterations = 8;
while($iterations--) {
    
    $symbols->run_cpu( cpu => $cpu, stop => [ '.drawenemies2', '.drawenemies8' ], );
    
    diag "Y: " . $cpu->get_y . " lastline: " . $cpu->read_8( $symbols->lastline );
    
    is $symbols->name_that_location( $cpu->get_pc ), '.drawenemies2', "did stop on the '.drawenemies2' label";    # happens when it loops around to draw another line of enemy data
    isnt $symbols->name_that_location( $cpu->get_pc ), '.drawenemies8', "did not stop on the '.drawenemies8' label";  # happens when we've drawn all the way to the bottom of the enemy
    
}

#
# stop looping
#

$symbols->run_cpu( cpu => $cpu, stop => [ '.drawenemies2', '.drawenemies8' ], );
diag "Y: " . $cpu->get_y . " lastline: " . $cpu->read_8( $symbols->lastline );
is $symbols->name_that_location( $cpu->get_pc ), '.drawenemies8', "did stop on the '.drawenemies8' label";  # happens when we've drawn all the way to the bottom of the enemy





# XXX run again, making it to .drawenemies7 or 8 (7 to draw just one line or 8 to finish drawing all of them that we're going to)

# my $line = $cpu->read_8( $symbols->view + $cpu->get_a );
# is $line & 0b11100000, $expected_color, "line drawn with expected color"; # XXX
# is $line & 0b00011111, $expected_width, "line drawn with expected width"; # XXX

# is count_lines_drawn(), 1, "one line drawn";
# is count_lines_drawn($expected_color), 1, "and it's the right color";


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
