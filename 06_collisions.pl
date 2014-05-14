#!/usr/local/bin/perl

# test collision detection logic

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
    my @stop_symbols = @_;
    my $cycles = 0;
    $cpu->run(10000, sub {
        my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
        my $name = name_that_location($pc);
        # diag $name if $name !~ m/unknown/;
        # diag sprintf "pc = %x inst = %x a = %s x = %s y = %x", $pc, $inst, $a, $x, $y;
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
        1, 11, 0x1e,  0xe0,          # 0 (0)
        20, 25, 0x14, 0x60,          # 1 (4)
        30, 40, 0x18, 0x20,          # 2 (8)
        0, 0, 0, 0,
) {
    $cpu->write_8( $level0++, $sym );
}

#
# test standing on a platform
#

$cpu->set_pc( $symbols->collisions);
$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14+1 );
 
run_cpu( $symbols->collisions9 );

is $cpu->read_8( $symbols->tmp2 ), 4, 'collision logic decided that we are standing on the second platform which has index 4';

# 
# test hitting our head on a platform from below (overlap)
#

$cpu->set_pc( $symbols->collisions);
$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14 );   # exactly at platform level
 
run_cpu( $symbols->collisions9 );

is $cpu->read_8( $symbols->tmp1 ), 0b00000001, 'collision logic decided that we are hitting our head and cannot go upwards';
is $cpu->read_8( $symbols->tmp2 ), 0xff, 'not standing on anything';

# 
# test hitting our head on a platform from below (one below)
#

$cpu->set_pc( $symbols->collisions);
$cpu->write_8( $symbols->playerz, 20 );
$cpu->write_8( $symbols->playery, 0x14-1 );   # one below platform level
 
run_cpu( $symbols->collisions9 );

is $cpu->read_8( $symbols->tmp1 ), 0b00000001, 'collision logic decided that we are hitting our head and cannot go upwards from below';
is $cpu->read_8( $symbols->tmp2 ), 0xff, 'not standing on anything';


#
# test walking in to a platform
#

$cpu->set_pc( $symbols->collisions);
$cpu->write_8( $symbols->playerz, 20-1 );   # one unit before the platform starts
$cpu->write_8( $symbols->playery, 0x14 );   # exactly at platform level
 
run_cpu( $symbols->collisions9 );

is $cpu->read_8( $symbols->tmp1 ), 0b00000010, 'collision logic decided that we are hitting our head and cannot go forward';
is $cpu->read_8( $symbols->tmp2 ), 0xff, 'not standing on anything';

#
# 
#

done_testing();
