#!/usr/local/bin/perl

# make sure various bits of logic do the right thing with deltaz/deltay

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

my $cycles_per_opcode;
open my $fh, '6502_formatted.txt' or die $!;
while( my $line = readline $fh ) {
    chomp $line;
    my @line = split m/ /, $line;
    @line >= 2 or die $line;
    # warn "$line[0] = $line[1]\n";
    $cycles_per_opcode->[ hex($line[0]) ] = $line[1];
}

sub run_cpu {
    my @stop_symbols = @_;
    my $cycles = 0;
    $cpu->run(10000, sub {
            my ($pc, $inst, $a, $x, $y, $s, $p) = @_;
            # diag sprintf "pc = %x inst = %x a = %s x = %s y = %x", $pc, $inst, $a, $x, $y;
            $cycles_per_opcode->[$inst] or die sprintf( "%2x (%d) has no cycle count", $inst, $inst) . "\n" . Dumper( $cycles_per_opcode );
            $cycles += $cycles_per_opcode->[$inst];
            if( grep $pc == $_, @stop_symbols ) {
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

diag $cycles;  # not really cycles; just an instruction count; currently 2807 ... yeah, that's a lot
ok int( $cycles / 64 ) < 96, 'finishes in less than 96*64 cycles (6144 cycles)'; # XXX double check this to make sure the figure is accurate

# and another troublesome one:

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x02 );
$cpu->write_8( $symbols->playery, 0x1d );
 
$cycles = run_cpu( $symbols->vblanktimerendalmost );

diag $cycles;  # 2773
ok int( $cycles / 64 ) < 96, 'finishes in less than 96*64 cycles';


# and a made up one

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x00 );
$cpu->write_8( $symbols->playery, 0x10 );
 
$cycles = run_cpu( $symbols->vblanktimerendalmost );

diag $cycles;  # 2113
ok int( $cycles / 64 ) < 96, 'finishes in less than 96*64 cycles';


# starting view (looking out over a platform, with $39 = 57 bits of gap filled)

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x02 );
$cpu->write_8( $symbols->playery, 0x20 );
 
$cycles = run_cpu( $symbols->vblanktimerendalmost );

diag $cycles;  # 2591
ok int( $cycles / 64 ) < 96, 'finishes in less than 96*64 cycles';


# a view with only $0f gaps filled

$cpu->set_pc( $symbols->platlevelclear );
$cpu->write_8( $symbols->playerz, 0x02 );
$cpu->write_8( $symbols->playery, 0x18 );
 
$cycles = run_cpu( $symbols->vblanktimerendalmost );

diag $cycles;  # 2267
ok int( $cycles / 64 ) < 96, 'finishes in less than 96*64 cycles';


done_testing();
