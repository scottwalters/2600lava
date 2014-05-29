#!/usr/local/bin/perl

# test _momentum

use strict;
use warnings;

use Test::More;

use Acme::6502;

use lib '.';
use symbols;

use Data::Dumper;
use Carp; $SIG{__DIE__} = sub { Carp::confess @_ };

use PadWalker;

my $debug = 0;

my $symbols = symbols::symbols('newbies.lst');

my $cpu = Acme::6502->new();
$cpu->load_rom( 'newbies.bin', 0xf000 );

sub run_cpu {
    my @stop_symbols = @_;
    my $cycles = 0;
    $cpu->run(10000, sub {
        my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
        my $name = name_that_location($pc);
        diag $name . ':' if $name !~ m/unknown/ and $debug;
        diag sprintf "pc = %x inst = %x a = %s x = %s y = %x", $pc, $inst, $a, $x, $y if $debug;
        if( grep $pc == $_, @stop_symbols ) {
            ${ PadWalker::peek_my(1)->{'$ic'} } = 0;
        }
    });
    return $cycles;
}

sub name_that_location {
    my $loc = shift;
    my %locations = reverse %$symbols;
    return $locations{$loc} if $locations{$loc};
    return $locations{$loc-1} if $locations{$loc-1};
    return $locations{$loc-2} if $locations{$loc-2};
    return $locations{$loc-3} if $locations{$loc-3};
    return 'unknown location';
}


#
# init
#

my $level0 = $symbols->level0 or die;
for my $sym (
        # start Z, end Z, Y, color
        1, 11, 0x1e,  0xe0,          # 0 (0)
        20, 25, 0x14, 0x60,          # 1 (4)
        30, 40, 0x18, 0x20,          # 2 (8)
        0, 0, 0, 0,
) {
    $cpu->write_8( $level0++, $sym );
}

#
# test Z momentum not being enough to move the player forward one unit
#

$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14+5 );

$cpu->write_8( $symbols->playerzlo, 0x70 );
$cpu->write_8( $symbols->playerzspeed, 0x05 );

$cpu->write_8( $symbols->playerylo, 0x00 );
$cpu->write_8( $symbols->playeryspeed, 0x00 );

$cpu->set_pc( $symbols->momentum0);
run_cpu( $symbols->momentum4);

is $cpu->read_8( $symbols->playerz), 20;        # unchanged
is $cpu->read_8( $symbols->playery), 0x14+5;    # unchanged

#
# test Z momentum moving the player forward one unit
#

$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14+5 );

$cpu->write_8( $symbols->playerzlo, 0x70 );
$cpu->write_8( $symbols->playerzspeed, 0x70 );  # adding those together should carry and increase playerz from 20 to 21

$cpu->write_8( $symbols->playerylo, 0x00 );
$cpu->write_8( $symbols->playeryspeed, 0x00 );

$cpu->set_pc( $symbols->momentum0);
run_cpu( $symbols->momentum4);

is $cpu->read_8( $symbols->playerz), 21;        # changed
is $cpu->read_8( $symbols->playery), 0x14+5;    # unchanged

# diag sprintf "playerzlo: %x\n", $cpu->read_8( $symbols->playerzlo);

#
# test Z momentum moving the player backwards one unit
#

$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14+5 );

$cpu->write_8( $symbols->playerzlo, 0x20 );
$cpu->write_8( $symbols->playerzspeed, 0xff - 0x20 );

$cpu->write_8( $symbols->playerylo, 0x00 );
$cpu->write_8( $symbols->playeryspeed, 0x00 );

$cpu->set_pc( $symbols->momentum0);
run_cpu( $symbols->momentum4);

is $cpu->read_8( $symbols->playerz), 19;         # changed
is $cpu->read_8( $symbols->playery), 0x14+5;    # unchanged

# diag sprintf "playerzlo: %x\n", $cpu->read_8( $symbols->playerzlo); # $ff

#
# test Y momentum moving the player forward one unit
#

$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14+5 );

$cpu->write_8( $symbols->playerylo, 0x70 );
$cpu->write_8( $symbols->playeryspeed, 0x70 );  # adding those together should carry and increase playery from 25 to 26

$cpu->write_8( $symbols->playerzlo, 0x00 );
$cpu->write_8( $symbols->playerzspeed, 0x00 );

$cpu->set_pc( $symbols->momentum0);
run_cpu( $symbols->momentum4);

is $cpu->read_8( $symbols->playerz), 20;        # unchanged
is $cpu->read_8( $symbols->playery), 0x14+6;    # changed




#
# test Y momentum moving the player down one unit
#

#        20, 25, 0x14, 0x60,          # 1 (4)

$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14+5 );   # 5 units above this platform

$cpu->write_8( $symbols->playerylo, 0x30 );
$cpu->write_8( $symbols->playeryspeed, 0xff - 0x30 );  # eg -0x30

$cpu->write_8( $symbols->playerzlo, 0x00 );
$cpu->write_8( $symbols->playerzspeed, 0x00 );

$cpu->set_pc( $symbols->momentum0);
run_cpu( $symbols->momentum4);

is $cpu->read_8( $symbols->playerz), 20;        # unchanged
is $cpu->read_8( $symbols->playery), 0x14+4;    # changed

# diag sprintf "playerylo: %x\n", $cpu->read_8( $symbols->playerylo); # $ff




#
# test Y momentum failing to move us down one unit because we've collided with a platform
#

# start Z, end Z, Y, color
# 20, 25, 0x14, 0x60,          # 1 (4)

$cpu->write_8( $symbols->SWCHA, 0xff );         # neither joystick is pushed any direction

$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14+1 );     # one unit above the platform; this should keep us from being able to go down

$cpu->write_8( $symbols->playerzlo, 0 );
$cpu->write_8( $symbols->playerzspeed, 0 );

$cpu->write_8( $symbols->playerylo, 0x30 );
$cpu->write_8( $symbols->playeryspeed, 0xff - 0x31 );  # eg -0x31

$cpu->set_pc( $symbols->collisions );
# $debug = 1;
run_cpu( $symbols->collisions9 );
# $debug = 0;

is $cpu->read_8( $symbols->collision_platform ), 4, 'collision logic decided that we are standing on the second platform which has index 4';

# resume execution...

run_cpu( $symbols->momentum4 );

is $cpu->read_8( $symbols->playerz), 20;        # unchanged
is $cpu->read_8( $symbols->playery), 0x14+1;    # unchanged

is $cpu->read_8( $symbols->playeryspeed), 0;    # downward momentum zero'd out

# diag sprintf "playerzlo: %x\n", $cpu->read_8( $symbols->playerzlo);



#
# test Y momentum failing to move up one unit because we're hitting our head on a platform
#

# start Z, end Z, Y, color
# 20, 25, 0x14, 0x60,          # 1 (4)

$cpu->write_8( $symbols->SWCHA, 0xff );         # neither joystick is pushed any direction

$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14-1 );     # one unit below the platform; this should keep us from being able to go up

$cpu->write_8( $symbols->playerzlo, 0 );
$cpu->write_8( $symbols->playerzspeed, 0 );

$cpu->write_8( $symbols->playerylo, 0x71 );
$cpu->write_8( $symbols->playeryspeed, 0x70 ); # adding these together should carry, but if/when 0x70 gets negated, 0x71 will be larger than it so we won't immediately go down

$cpu->set_pc( $symbols->collisions );
run_cpu( $symbols->collisions9 );

is $cpu->read_8( $symbols->collision_bits ), 0b00000001, 'collision logic decided that we are hitting our head and cannot go up';

# resume execution...

run_cpu( $symbols->momentum4 );

is $cpu->read_8( $symbols->playerz), 20;        # unchanged
is $cpu->read_8( $symbols->playery), 0x14-1;    # unchanged

is $cpu->read_8( $symbols->playeryspeed), 0xff - 0x70;    # upwards momentum turned into downward momentum



#
# test Z momentum failing to move the player forward one unit when they run in to the front of a platform
#

# start Z, end Z, Y, color
# 20, 25, 0x14, 0x60,          # 1 (4)

$cpu->write_8( $symbols->playerz, 20-1 );
$cpu->write_8( $symbols->playery, 0x14 );

$cpu->write_8( $symbols->playerzlo, 0x71 );
$cpu->write_8( $symbols->playerzspeed, 0x70 );  # adding those together should carry, but subtracting 0x70 from 0x71 (after we bounce off of the platform) should avoid rolling over

$cpu->write_8( $symbols->playerylo, 0x00 );
$cpu->write_8( $symbols->playeryspeed, 0x00 );

$cpu->set_pc( $symbols->collisions );
run_cpu( $symbols->collisions9 );

is $cpu->read_8( $symbols->collision_bits ), 0b00000010, 'collision logic decided that we are hitting our head and cannot go forward';
is $cpu->read_8( $symbols->collision_platform ), 0xff, 'not standing on anything';

$cpu->set_pc( $symbols->momentum0);
run_cpu( $symbols->momentum4);

is $cpu->read_8( $symbols->playerz), 20-1;    # unchanged
is $cpu->read_8( $symbols->playery), 0x14;    # unchanged

is $cpu->read_8( $symbols->playerzspeed), 0xff - 0x70;    # forward momentum turned into backwards momentum



#
# test applying gravity when it doesn't make us hit terminal velocity
#

$cpu->write_8( $symbols->playeryspeed, 0 );  # not moving up or down, yet

$cpu->set_pc( $symbols->momentum4 );
run_cpu( $symbols->momentum9 );

is $cpu->read_8( $symbols->playeryspeed ), 0xff;
# diag sprintf "after gravity, playeryspeed: %x\n", $cpu->read_8( $symbols->playeryspeed);

$cpu->set_pc( $symbols->momentum4 );
run_cpu( $symbols->momentum9 );

is $cpu->read_8( $symbols->playeryspeed ), 0xfe;


#
# test applying gravity when it does make us hit terminal velocity
#

my $terminal_velocity = $symbols->terminal_velocity;   # constant
diag "terminal velocity = " . sprintf "%x", $terminal_velocity;
diag "terminal velocity = aka " . - ( 0xff - $terminal_velocity );

my $gravity = $symbols->gravity;  # constant
diag "gravity = $gravity";

$cpu->write_8( $symbols->playeryspeed, $terminal_velocity + $gravity );

$cpu->set_pc( $symbols->momentum4 );
run_cpu( $symbols->momentum9 );

# diag sprintf "after gravity, playeryspeed: %x\n", $cpu->read_8( $symbols->playeryspeed);
is $cpu->read_8( $symbols->playeryspeed ), $terminal_velocity;

$cpu->set_pc( $symbols->momentum4 );
run_cpu( $symbols->momentum9 );

# diag sprintf "after gravity, playeryspeed: %x\n", $cpu->read_8( $symbols->playeryspeed);
is $cpu->read_8( $symbols->playeryspeed ), $terminal_velocity - 1;  # since the test is less-than, gravity gets applied one extra time to bring us to falling just faster than terminal velocity.

$cpu->set_pc( $symbols->momentum4 );
run_cpu( $symbols->momentum9 );

# diag sprintf "after gravity, playeryspeed: %x\n", $cpu->read_8( $symbols->playeryspeed);
is $cpu->read_8( $symbols->playeryspeed ), $terminal_velocity - 1;  # ... but then we don't accelerate any faster than one more than terminal velocity (or one less than, since it is a negative number)



#
# 
#

done_testing();
