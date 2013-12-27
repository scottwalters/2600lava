 
; dasm newbies.asm -onewbies.bin -lnewbies.lst -snewbies.sym -f3
; makeawav -ts a.out

		processor 6502
		include "vcs.h"

        SEG.U RAM_VARIABLES
        ORG $80

playerz ds 1		; how far in to the maze
playery	ds 1		; how close to splatting
playerzlo ds 1		; fractal part of their position
playerylo ds 1 
playeryspeed ds 1  ; 1.4.3 format -- sign, whole, fractional parts
playerzspeed ds 1  ; 1.4.3 format -- sign, whole, fractional parts

; numeric output
num0    ds 1		; number to output, left

; various
tmp1	ds 1
tmp2	ds 1

NUMG0	= tmp2		; pattern buffer temp -- number output (already uses temp)

; level render
; these must be persistent between calls to platresume so the routine can pause and resume
curplat ds 1		; which platform we are rendering or considering (increments by 4 each go)
deltaz  ds 1		; how far forward the current platform line is from the player
deltay ds 1 		; how far above or below the current platform the player is

; using the S register for curlineoffset

; framebuffer
view	ds [ $ff - 2 - view ]		; 100 or so lines; from $96 goes to $fa, which leaves $fb, $fc, $fd and $fe for the 6502 stack XXX let's fix so we can do 2 bytes for the stack


;
; constants
;

viewsize	= [ $ff - 2 - view ]
		ECHO "viewsize: ", [viewsize]d

flap_y		= %01111111;

;
; macros
;

;
; _absolute
;

; takes a value in A, returns it in A

		MAC _absolute
		bpl .abs1
		eor #$ff
		clc
		adc #$01
.abs1
		ENDM

;
; arctan (formerly platlinedelta)
;

; index the arctan table with four bits of each delta
; we use the most significant non-zero four bits of each delta
; the arctan table is indexed by the ratio of the y and z deltas
; this means we can scale the values up to get more precision as long as we scale them up together
; we shift right up to four times until ( deltay | delta ) <= 0x0f
; deltaz is in tmp1 and deltay is in tmp2 where they get shifted to the right
; this also adds half of the screen height or subtracts from half screen height as appropriate to convert to scan line number

		MAC arctan
.platlinedelta
		lda deltaz
		sta tmp1
		lda deltay
		_absolute
		sta tmp2
		ora tmp1				; combined bits so we only have to test one value to see if all bits are clear of the top nibble
.platlinedeltaagain
		cmp #$0F
		bmi .platlinedeltadone	; 0F is larger, had to barrow, so we know that no high nibble bits are set
		lsr
		lsr tmp1
		lsr tmp2
		jmp .platlinedeltaagain
.platlinedeltadone
		lda tmp2				; table loops over $z then over $y, so $z is down and $y is across
		asl						; tmp1 is our deltaz, tmp2 our deltay
		asl						; so we make tmp1 our high nibble so it goes down and tmp2 our low nibble so it goes across
		asl
		asl
        ora tmp1
		; sta num0
		tay
		bit deltay ; handle the separate cases of the platform above us and the platform below us
		bmi .platarctan1		; true value indicates platform is above us
		lda arctangent,y		; platform is below us; add the arctangent value to the middle of the screen
		clc
		adc #(viewsize/2)
        jmp .platarctan2
.platarctan1
		; platform is above us; subtract the arctangent value from the middle of the screen
		lda #(viewsize/2)
		sec
		sbc arctangent,y
.platarctan2
		ENDM

;
; plathypot
;

; use a hypotonose table to estimate distance to a line so that we can assign it a width to represent size
; projection: projected y = distance_of_"screen" * actual_y / distance_of_point
; we're just using angle as a direct index to scanline number, but I think that's okay... 0..45 degrees
; further away things move towards the middle of the screen, but atan(z/y) takes that into account

; compute line size:
; 1. zd,yd -> hypot table -> distance         (adding zd back into zd a fractional number of times depending on yd)
; 2. distance -> perspective table -> size    (looking distance in a table to figure out line width)

; #1 requires deltay to be less than 50, and we probably shouldn't be looking 50 paces ahead anyway
; this is just a situation optimized, table driven, unrolled multiplication
; it should probably be un-un-rolled a bit

		MAC plathypot
		lda deltay
		_absolute
		tay
		lda distancemods,y
		sta tmp2				; top three bits indicate whether 1/4th of deltaz should be re-added to itself, then 1/8th, etc
		lda deltaz
		lsr
		lsr
		sta tmp1				; tmp1 contains the fractional (1/4 at first, then 1/8th, then 1/16th) value of deltaz
		lda deltaz				; fresh copy to add fractional parts to
		clc
		asl tmp2
		bcc .plathypot1
		clc
		adc tmp1				; 1/4
.plathypot1
		lsr tmp1				; half again
		asl tmp2
		bcc .plathypot2
		clc
		adc tmp1				; 1/8th
.plathypot2
		lsr tmp1				; half again
		asl tmp2
		bcc .plathypot3
		clc
		adc tmp1				; 1/16th
.plathypot3
		ENDM

;
; plotonscreen
;

; Y gets the distance, which we use to figure out which size of line to draw
; X gets the scan line to draw at
; this macro is used in three different places

		MAC plotonscreen
		lda view,x				; get the line width of what's there already
		and #%00011111			; mask off the color part and any other data
		cmp perspectivetable,y	; compare to the fatness of line we wanted to draw
		bpl .plotonscreen3		; what we wanted to draw is smaller.  that means it's further away.  skip it.
		lda perspectivetable,y
		ldy curplat
		ora level0+3,y			; add the platform color (level0 contains records of:  start position, length, height, color)
		sta view,x
.plotonscreen3
		ENDM

;
; ROM
;

        SEG PROGRAM_CODE 
		ORG $f000

		
;
; reset
;

reset

		sei                     ; Disable interrupts
		cld                     ; Clear decimal bit
		ldx #$ff				; top of the stack
		txs                     ; Init Stack

		; initialize ram to 0
		lda #0
		ldx #$80
reset0  sta $80,x
		dex
		bne reset0

		; hardware
		sta $281				; all joystick pins for input

		; player location
		ldy #2
		sty playerz
		ldy #32
		sty playery

startofframe

; initialize registers

		lda #%00001110
		sta COLUPF

		lda #%00000000
		sta COLUBK

		lda #%00000101		; reflected playfield with priority over players
		sta CTRLPF

		lda #0
		sta VBLANK
		sta PF0
		sta PF1
		sta PF2

		sta WSYNC

;
;
; display
;
;

display

;
; platform graphics
;

		ldx #0			; platforms display loop init

platforms
		lda view,x			; +4    31
		lsr					; +2    33      upper 3 bits are platform color
		lsr					; +2    35
		lsr					; +2    37
		lsr					; +2    39
		lsr					; +2    41
		tay					; +2	43
		lda colors,y        ; +5    48
		sta WSYNC
		sta COLUPF			; +3    51
		lda view,x			; +4    55
		and #%00011111		; +3    58      lower 5 bits is platform width
		tay					; +2    60
		lda pf0lookup,y		; +4    64
		; sta WSYNC			; +3    67 .... of 76
		sta PF0				; +3     3
		lda pf1lookup,y		; +4     7
		sta PF1				; +3    10
		lda pf2lookup,y		; +4    14
		sta PF2				; +3    17
		
		inx					; +2    19
		cpx #viewsize		; +2    21
        bne platforms		; +5?   26
; scanline 102 now

; putting the color in the low bits frees up some time but I couldn't get the bugs out
;platforms
;		lda view,x			; +4    31
;		and #%00000111		;               lower 3 bits are platform color
;		tay
;		lda colors,y
;		tay
;		lda view,x
;		lsr					; +2    33
;		lsr					; +2    35
;		lsr					; +2    41
;		sta WSYNC
;		sty COLUPF
;		tay					; +2	43
;		lda pf0lookup,y		; +4    64
;		sta PF0				; +3     3
;		lda pf1lookup,y		; +4     7
;		sta PF1				; +3    10
;		lda pf2lookup,y		; +4    14
;		sta PF2				; +3    17
;		
;		inx					; +2    19
;		cpx #viewsize		; +2    21
;        bne platforms		; +5?   26
; scanline 102 now

;
; debugging output (a.k.a. score)
;

score
		sta WSYNC
		lda #0
		sta PF0
		sta PF1
		sta PF2
		lda #%00001110
		sta COLUPF
		lda  #4
		sta  CTRLPF             ; Double, instead of reflect.
		clc
        lda  #0
		sta  NUMG0              ; Clear the number graphics buffers... they won't be calculated yet,
		sta  PF1
		sta  tmp1				; using temp as our own scan line counter, since we need to adc it.
		        		        ; the game will try to draw with them anyway.
VSCOR	sta  WSYNC              ; Start with a fresh scanline.
		sta  PF1				; +3
		lda  num0				; +3 
		and  #$f0               ; +2    left digit
		lsr						; +2    offset 3 bits from right for *8 into lookup table
		adc  tmp1				; +3    which scanline
		tay						; +2 
		lda  NUMBERS,Y          ; +4    Get left digit.
		and  #$F0       		; +2 
		sta  NUMG0     			; +3 
		lda  num0				; +3 
		and  #$0f               ; +2    right digit
		asl						; +2    shift right 3 bits for *8 in lookup table
		asl						; +2 
		asl						; +2 
		adc  tmp1				; +3    which scanline
		tay						; +2 
		lda  NUMBERS,Y          ; +4    Get right digit.
		and  #$0f				; +2 
		ora  NUMG0				; +3 
		sta  PF1				; +3
		lda #0
		inc tmp1
		ldy  tmp1				; +3 
		cpy  #7					; +3 
		bne  VSCOR				; +5 taken (?) 
scoredone

		lda #0
		sta PF1
		sta WSYNC

; we're on scanline 112 or so now depending on viewsize
; picture is 192 - the 112 we've already done = 80 scan lines we need to waste
; 80 or so scan lines to waste before overscan

		; lda #96					; 81*76/64 = 96 odd, plus one for the WSYNC at the end
		lda #95					; = 80 scan lines * 76 cpu cycles per line / 64 clocks per time tick, plus one for the WSYNC at the end
		sta TIM64T

		jsr readstick
		jsr gamelogic
		jsr platlevelclear		; start at the beginning; was platresume
		sta WSYNC

; 30 lines of overscan, which means a timer for 29 frames plus a WSYNC

		lda #34					; 29*76/64 = 34.4375, plus the WSYNC at the end
		sta TIM64T
		jsr platresume
		sta WSYNC

; 3 scanlines of vsync signal

		lda #2
		sta VSYNC
		sta WSYNC
		sta WSYNC
		sta WSYNC
		lda #0
		sta VSYNC

;
; start vblank
;

		; lda #%00000010			; turn off joystick latches (bit 7) for VBLANK to reset them; bit 2 sets VBLANK
		lda #%01000010			; leave the joystick latches (bit 7) on and don't reset them here; bit 2 sets VBLANK
		sta VBLANK

; 37 lines of vblank
; (76*30)/64 = 35.625.  the Combat version had a wsync before it for 36.6, and if we do a wsync after the fact, then we're at 37.
; okay, we do that WSYNC now, before the RTS.

		lda #42					; 36*76/64 = 42.75, plus one for the WSYNC at the end
		sta TIM64T
		jsr platresume
		lda #%01000000 ; turn VBLANK off and the joystick latch on; joystick triggers will now right read 1 until trigger is pressed, then it will stick as 0
		sta VBLANK
		sta WSYNC

; and back to drawing the screen
;

		jmp startofframe

;
;
;

;
; update frame buffer
;

; iterate through the platforms ahead of the player and updates the little frame buffer of line widths
; atomic operation is one line; if inadequate CPU is left, it'll suspend before doing the next line and then resume
; at the same point when next invoked
; variables:
; curplat   -- which platform we're considering drawing or currently drawing (should be a multiple of 4)
; platend   -- stores level0[curplat][start] + level0[curplat][length]
; deltay    -- how far the player is above/below the currently being drawn platform
; deltaz    -- how far the player is from the currently being drawn line of the currently being drawn platform -- counts down from level0[curplat][end]-playerz to level0[curplat][end]-playerz (which is 0)
; using the S register now for curlineoffset

platlevelclear					; hit end of the level:  clear out all incremental stuff and go to the zeroith platform
		; start over rendering
		ldy #0
		; sty num0	; XX counting how many platform lines we render
		sty deltaz
		sty curplat

		; zero out the framebuffer
		ldy #viewsize-1
		ldx #0
platlevelclear2
		stx view,y
		dey
		bne platlevelclear2
		jmp platnext0
		
platresume
		lda curplat			; where we in middle of a platform (other than the 0th one)?
        ; bne platlineseek	; was doing this but going to plattryline is more direct/faster if things are in fact properly initialized; this change makes it render differently but it isn't especially bad or wrong; still could probably clean things up a bit and work kinks out...  yeah?  seek to the visible part of the current platform and start drawing; otherwise, fall through to find the next platform XXXX and now platlineseek doesn't exist any more, but this note about "hey, the engine is a bit fucked up" might be useful
        bne plattryline     ; yeah?  continue rendering that platform; otherwise, fall through to looping through platforms

platnext0
		; is there a current platform?  if not, go busy spin on the timer
		ldy curplat				; offset into the level0 table
		lda level0,y			; load the first byte, the Z start position, of the current platform
		bne platnext1			; not 0 yet, so we have a platform to evaluate and possibily render if it proves visible
		jmp vblanktimerendalmost	; no more platforms; just burn time until the timer expires
platnext1
		lda level0+1,y			; get the end point of the platform, since the end is the interesting part to test for to see if we can see any of this platform
		cmp playerz				; compare to where the player is
		bpl platfound			; playerz <= end-of-this-platform, so show the platform
		; otherwise, fall through to trying the next platform

platnext						; seek to the next platform and take a look at doing it
		ldy curplat
		iny
		iny
		iny
		iny
		sty curplat
		jmp platnext0

platfound
		; a platform was found that ends in front of us; initialize deltay, deltaz and start doing lines from a platform
		lda level0+1,y			; get platform end
		sec
		sbc playerz
		sta deltaz				; end of the platform minus playerz is deltaz
		lda playery
		sec
		sbc level0+2,y			; subtract the 3rd byte, the platform height
		sta deltay				; deltay is the difference between the player and the platform, signed

; work backwards from the last visible line; this way, we can double-plot lines and wider lines, drawn later, will overwrite
; the narrower lines, drawn earlier
; this approach limits us to drawing two lines and not drawing an additional line on the last line plotted
; XXX not doing this at the moment

plattryline
		arctan					;		jsr platlinedelta ; takes deltaz and deltay; uses tmp1 and tmp2 for scratch; returns an arctangent value in the accumulator from a table which we use as a scanline to draw too
		tax
		txs						; using the S register to store our value for curlineoffset

		plathypot				; jsr plathypot			; reads deltay and deltaz directly, returns the size aka distance of the line in the accumulator

		tay						; Y gets the distance, fresh back from plathypot, which we use to figure out which size of line to draw
		tsx						; X gets the scanline to draw at; value for curlineoffset is hidden in the S register

		plotonscreen			; jsr plotonscreen
		; fall through to platnextline

platnextline
		lda INTIM
		; cmp #6					; at least 5*64 cycles left?  have to keep fudging this.  last observed was 5, so one for safety. XXX something is screwed up here... in some cases, this is taking way too much time
		cmp #7					; at least 5*64 cycles left?  have to keep fudging this.  last observed was 5, so one for safety. XXX something is screwed up here... in some cases, this is taking way too much time
		bmi vblanktimerendalmost

		; inc num0				; XX counting how many platform lines we evaluate in a frame
		dec deltaz				; deltaz goes down to zero; doing this after the timer test instead of before probably means that when we come back, we redo the same line XX
		bit deltaz
		bpl platnotclear		; branch to continue rendering if we haven't walked backwards past the players position
		jmp platnext

platnotclear
;fatlines
;		ldy curlinewidth		; Y gets the distance, which we use to figure out which size of line to draw
;		tsx						; X gets the scanline to draw at; value for curlineoffset is hidden in the S register
;		bit deltay				; playery - platform height is > 0 (positive) if platform is below us
;		bmi fatlines2			; branch if this line is in the top half of the screen (platform height > playery)
;fatlines1				; we're drawing in the bottom half of the screen, so add
;		inx
;		plotonscreen			; jsr plotonscreen
;		jmp fatlines3
;fatlines2				; we're drawing in the top half of the screen, so subtract		
;		dex
;		plotonscreen			; jsr plotonscreen
;fatlines3

		jmp plattryline

;
; timer
;

; execution is sent here when there isn't enough time left on the clock to render a line or do whatever other operation
; nothing to do but wait for the timer to actually go off

vblanktimerendalmost
		lda #0				; XX testing
		sta tmp1
vblanktimerendalmost1
		lda	INTIM
		beq vblanktimerendalmost2
		inc tmp1			; XX testing -- how much time do we have to burn before the timer actually expires?
        jmp vblanktimerendalmost1
vblanktimerendalmost2
		lda tmp1
;		sta num0 ; XX diagnostics to figure out how much time is left on the timer when platresume gives up
		ldx #$fd			; we use the S register as a temp so restore it to the good stack pointer value; we only ever call one level deep so we can hard code this
		txs
		rts

;
;
; small subroutines
;
;

;
; readstick
;

readstick
		; bit 3 = right, bit 2 = left, bit 1 = down, bit 0 = up, one stick per nibble
		lda SWCHA
		and #$f0
		eor #$ff
readstick0
        ; bit 0 = up
		tax
		and #%00010000
		beq readsticka
		inc playery ; XXX testing
readsticka
        ; bit 1 = down
		txa
		and #%00100000
		beq readstickb
		dec playery ; XXX testing
readstickb
		lda playery
		; sta num0 ; XXXXX
readstick5
		; bit 2 = left
		txa
		and #%01000000
		beq readstick6
		inc playerz ; XXX testing
readstick6
		; bit 3 = right
		txa
		and #%10000000
		beq readstick7
		dec playerz ; XXX testing
readstick7
		; button
		lda INPT4
		bmi readstick8  ; branch if button up (bit 7 stays 1 until the trigger is pressed, then it stays 0)
        ; button down -- make player go faster forward and upwards, mostly upwards
        ; also, reset the joy latches
		lda #%00000000			; bit 7 is joystick button latch enable; bit 2 is vblank enable
		sta VBLANK
		lda #%01000000
		sta VBLANK
		; XXX bump playerzspeed
readstick7a
readstick7b
		; inc playery XXX should inc it more or less depending on whether they're pushing forward or back
		clc
		lda playeryspeed
		adc #flap_y
		bvc readstick7c			; if there was no overflow, write the result back as-is
		lda #%01111111			; clamp to max signed int 8
readstick7c
		sta playeryspeed
readstick8
		rts

;
; game logic
;

gamelogic
		;
		; z speed
		;
		clc					; not sure about this, but setting carry if we're adding a negative number avoids subtracting one extra; since we're dealing with a fractal part of the speed, I'm just not going to worry about it
		lda playerzspeed
		adc playerzlo
		sta playerzlo
		lda playerz
		bit playerzspeed	; reset the flags so we can test again if this is negative
		bmi gamelogic1
		adc #0				; positive so add 0
		bcs gamelogic2		; don't write back to playerz if this addition would take it above $ff XXX actually, wouldn't this be the win condition for the level?
		sta playerz
		jmp gamelogic2
gamelogic1
		adc #$ff			; negative so add $ff
		beq gamelogic2		; don't write back to playerz if this subtraction would take it to zero
		sta playerz
gamelogic2
        ;
		; y speed
        ;
		clc
		lda playeryspeed
		adc playerylo
		sta playerylo
		lda playery
		bit playeryspeed	; reset the flags so we can test again if this is negative
		bmi gamelogic3
		adc #0				; positive so add 0
		bcs gamelogic4		; don't write back to playery if this addition would take it above $ff
		sta playery
		jmp gamelogic4
gamelogic3
		adc #$ff			; negative so add $ff
		beq gamelogic4		; don't write back to playery if this subtraction would take it to zero; XXX actually, wouldn't this be the death condition?
		sta playery
gamelogic4

gamelogic6
		;
		; subtract gravity from vertical speed (and stop that damn bounce! and then add the bounce back in later!)
		;
		lda playeryspeed
		sec
		; sbc #%00000011 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX hold off on gravity for a bit
		; sta num0 ; XX testing -- playeryspeed
		bpl gamelogic6a		; still >0, so just save it 
		;
		; terminal volicity
		;
		cmp #%11100000		; 1.4.3.  $df terminal volicity.
		; bcc gamelogic6b
		bmi gamelogic6b
gamelogic6a
		sta playeryspeed
gamelogic6b
		;
		; XXX land on platform?  land on ground?
		;
		lda playerz
		bpl gamelogic7
		lda #0
		sta playerz
		sta playerzlo
		sta playerzspeed
		; lda playerzspeed		; XXX okay, this logic isn't what's causing the bouncing
		; cmp #128				; playerzspeed > 128, no barrow, carry is still set, and it gets rotated in
		; ror
		; sta playerzspeed
		; lda #0					; negate the result
		; sbc playerzspeed
		; sta playerzspeed
gamelogic7
        ; return and diagnostic output
		; lda playery
		; lda playeryspeed
		; sta num0			;	XX -- testing -- num0 is playeryspeed
		rts
;
; mark and sweep
;

; mark and sweep sweeps everything previously marked and then marks everything that's left so it will be swept on the next call

;
;
;



;
;
; tables
;
;

; playfield data for each register, indexed by width of the platform to draw

pf0lookup
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%10000000
		dc.b #%11000000
		dc.b #%11100000
		dc.b #%11110000

pf1lookup
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000000
		dc.b #%00000001
		dc.b #%00000011
		dc.b #%00000111
		dc.b #%00001111
		dc.b #%00011111
		dc.b #%00111111
		dc.b #%01111111
		dc.b #%11111111
		dc.b #%11111111
		dc.b #%11111111
		dc.b #%11111111

pf2lookup
		dc.b #%00000000
		dc.b #%10000000
		dc.b #%11000000
		dc.b #%11100000
		dc.b #%11110000
		dc.b #%11111000
		dc.b #%11111100
		dc.b #%11111110
		dc.b #%11111111
		dc.b #%11111111
		dc.b #%11111111
		dc.b #%11111111
		dc.b #%11111111
		dc.b #%11111111
		dc.b #%11111111
		dc.b #%11111111
		dc.b #%11111111
		dc.b #%11111111
		dc.b #%11111111
		dc.b #%11111111

; things higher or lower than us are farther away than things right in front of us.
; this table indicates how many times the zdelta (distance ahead) needs to be added back to itself to
; compensate for vertical difference, to more accurately compute distance.
; the largest bit indicates that 1/4th of the value needs to be added back in.
; next bit left indicates that 1/8th of the value needs to be added back in to itself.
; the next bit indicates that 1/16th of the value needs to be added back in to itself.

distancemods

     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00000000
     dc.b #%00100000
     dc.b #%00100000
     dc.b #%00100000
     dc.b #%00100000
     dc.b #%00100000
     dc.b #%00100000
     dc.b #%00100000
     dc.b #%00100000
     dc.b #%00100000
     dc.b #%00100000
     dc.b #%01000000
     dc.b #%01000000
     dc.b #%01000000
     dc.b #%01000000
     dc.b #%01000000
     dc.b #%01000000
     dc.b #%01000000
     dc.b #%01000000
     dc.b #%01100000
     dc.b #%01100000
     dc.b #%01100000
     dc.b #%01100000
     dc.b #%01100000
     dc.b #%01100000
     dc.b #%01100000
     dc.b #%10000000
     dc.b #%10000000
     dc.b #%10000000
     dc.b #%10000000
     dc.b #%10000000
     dc.b #%10000000
     dc.b #%10100000
     dc.b #%10100000
     dc.b #%10100000
     dc.b #%10100000
     dc.b #%10100000
     dc.b #%10100000
     dc.b #%11000000
     dc.b #%11000000
     dc.b #%11000000
     dc.b #%11000000

; atan(x/y) normalized to between 0 and 50
; $x and $y are 1..16, print int(rad2deg(atan($x/$y))*0.57)||0, "," 
; here, "25" corresponds to a 45 degree angle, I guess
; XXX I guess that means that half this table is never used... should optimize it better

; increasing Y values go across, increasing Z values go down -- no, backwards... so confused... increasing Z should move lines towards middle of screen though
; lines with more Z than Y never get displayed anyway

;   Y  Z-->
;   |
;   V


arctangent

		; dc.b 25, 15, 10, 8, 6, 5, 4, 4, 3, 3, 2, 2, 2, 2, 2, 2
		dc.b 25, 15, 10, 8, 6, 5, 4, 4, 3, 3, 2, 1, 1, 0, 0, 0 
		; dc.b 36, 25, 19, 15, 12, 10, 9, 8, 7, 6, 5, 5, 4, 4, 4, 4
		dc.b 36, 25, 19, 15, 12, 10, 9, 8, 7, 6, 5, 5, 4, 3, 3, 2
		dc.b 40, 32, 25, 21, 17, 15, 13, 11, 10, 9, 8, 8, 7, 6, 6, 6
		dc.b 43, 36, 30, 25, 22, 19, 16, 15, 13, 12, 11, 10, 9, 9, 8, 8
		dc.b 44, 38, 33, 29, 25, 22, 20, 18, 16, 15, 13, 12, 11, 11, 10, 9
		dc.b 45, 40, 36, 32, 28, 25, 23, 21, 19, 17, 16, 15, 14, 13, 12, 11
		dc.b 46, 42, 38, 34, 31, 28, 25, 23, 21, 19, 18, 17, 16, 15, 14, 13 
		dc.b 47, 43, 39, 36, 33, 30, 27, 25, 23, 22, 20, 19, 18, 16, 16, 15 
		dc.b 47, 44, 40, 37, 34, 32, 29, 27, 25, 23, 22, 21, 19, 18, 17, 16 
		dc.b 48, 44, 41, 38, 36, 33, 31, 29, 27, 25, 24, 22, 21, 20, 19, 18 
		dc.b 48, 45, 42, 39, 37, 34, 32, 30, 28, 27, 25, 24, 22, 21, 20, 19 
		dc.b 48, 45, 43, 40, 38, 36, 34, 32, 30, 28, 27, 25, 24, 23, 22, 21 
		dc.b 48, 46, 43, 41, 39, 37, 35, 33, 31, 29, 28, 26, 25, 24, 23, 22 
		dc.b 48, 46, 44, 42, 40, 38, 36, 34, 32, 31, 29, 28, 26, 25, 24, 23 
		dc.b 49, 46, 44, 42, 40, 38, 37, 35, 33, 32, 30, 29, 27, 26, 25, 24 
		dc.b 49, 47, 45, 43, 41, 39, 37, 36, 34, 33, 31, 30, 29, 27, 26, 25 

;
;
;

perspectivetable

		; computer generated one
		 dc.b 20, 20, 20, 20, 20, 20, 20, 20, 17, 16, 14, 13, 12, 11, 10, 10, 9, 8, 8, 8, 7, 7, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1


level0
        ; platform start point in the level, platform end point, height of the platform, color (index into the colors table shifted left five bits)
        ; eg, this first one starts at 1, is 10 long, is 30 high, and points to the 1th entry in the colors table
		dc.b 1, 11, $1e,  %00100000
		dc.b 20, 20, $14, %01000000
       dc.b 30, 40, $18,  %01100000
;		dc.b 15, 25, $19, %01100000
		dc.b 0, 0, 0, 0 		;       end
		dc.b 0, 0, 0, 0 		;       end

colors

 		dc.b %11111110,  %01011100, %01101100, %10111000
 		dc.b %11111110,  %01011100, %01101100, %10111000		; repeat because highbit gets used as color index but is really a flag!

; %0000... is white/grey/black
; %1000... is blue
; %0100... is red
; %1100... is green
; %1110... golden
; %1111... organge


;
;
;

NUMBERS
    .byte %11101110
    .byte %10101010
    .byte %10101010
    .byte %10101010
    .byte %10101010
    .byte %11101110
	.byte $00, $00
	
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $00, $00
	
	.byte $EE ; |XXX XXX |
	.byte $22 ; |  X   X |
	.byte $EE ; |XXX XXX |
	.byte $88 ; |X   X   |
	.byte $88 ; |X   X   |
	.byte $EE ; |XXX XXX |
	.byte $00, $00
	
	.byte $EE ; |XXX XXX |
	.byte $22 ; |  X   X |
	.byte $66 ; | XX  XX |
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $EE ; |XXX XXX |
	.byte $00, $00
	
	.byte $AA ; |X X X X |
	.byte $AA ; |X X X X |
	.byte $EE ; |XXX XXX |
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $00, $00
	
	.byte $EE ; |XXX XXX |
	.byte $88 ; |X   X   |
	.byte $EE ; |XXX XXX |
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $EE ; |XXX XXX |
	.byte $00, $00
	
	.byte $EE ; |XXX XXX |
	.byte $88 ; |X   X   |
	.byte $EE ; |XXX XXX |
	.byte $AA ; |X X X X |
	.byte $AA ; |X X X X |
	.byte $EE ; |XXX XXX |
	.byte $00, $00
	
	.byte $EE ; |XXX XXX |
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $00, $00
	
	.byte $EE ; |XXX XXX |
	.byte $AA ; |X X X X |
	.byte $EE ; |XXX XXX |
	.byte $AA ; |X X X X |
	.byte $AA ; |X X X X |
	.byte $EE ; |XXX XXX |
	.byte $00, $00
	
	.byte $EE ; |XXX XXX |
	.byte $AA ; |X X X X |
	.byte $EE ; |XXX XXX |
	.byte $22 ; |  X   X |
	.byte $22 ; |  X   X |
	.byte $EE ; |XXX XXX |
	.byte $00, $00, $00

	.byte $EE ; |XXX XXX |
	.byte $AA ; |X X X X |
	.byte $EE ; |XXX XXX |
	.byte $AA ; |X X X X |
	.byte $AA ; |X X X X |
	.byte $AA ; |X X X X |
	.byte $00, $00

	.byte %11001100; |XX  XX  |
	.byte %10101010; |X X X X |
	.byte %11001100; |XX  XX  |
	.byte %10101010; |X X X X |
	.byte %10101010; |X X X X |
	.byte %11001100; |XX  XX  |
	.byte $00, $00

	.byte %01000100; | X   X  |
	.byte %10101010; |X X X X |
	.byte %10001000; |X   X   |
	.byte %10001000; |X   X   |
	.byte %10101010; |X X X X |
	.byte %01000100; | X   X  |
	.byte $00, $00

	.byte %11001100; |XX  XX  |
	.byte %10101010; |X X X X |
	.byte %10101010; |X X X X |
	.byte %10101010; |X X X X |
	.byte %10101010; |X X X X |
	.byte %11001100; |XX  XX  |
	.byte $00, $00

	.byte %11101110;
	.byte %10001000;
	.byte %11101110;
	.byte %10001000;
	.byte %10001000;
	.byte %11101110;
	.byte $00, $00

	.byte %11101110;
	.byte %10001000;
	.byte %11101110;
	.byte %10001000;
	.byte %10001000;
	.byte %10001000;
	.byte $00, $00

;
;
;

		echo "ROM bytes used up to ", *

;
; interrupt vectors
;

		.org $fffa
		.word reset
		.word reset
		.word reset


;
; render pipeline
;

; 1. clear the 100 lines of framebuffer data
; 2. each framebuffer byte encodes line width and color
; 3. loop over the next few platforms from the player's current position
; 4. loop over each point in the platform (each segment it is long)
; 5. compute angle to point in the platform; this indicates which scanline that point is to be drawn at 
; 6. compute distance to platforms using a cheat hypotnuse table
; 7. draw the platform segment into the framebuffer if it is wider than anything that's already there (nearer)

; alternatively, find the angle/distance for the front and back of each platform, and then
; fill in all of the scan lines between the two points interpolating platform width along the way

; projection: projected y = distance_of_"screen" * actual_y / distance_of_point

; finding the angle:
; input is ratio of distance ahead to distance above/below, that is $delta_y / $delta_z
; output is degrees
; how? angle = atan(y_delta/z_delta)... delta_z must be greater than (>) delta_y or it's out of our 45 degree range

;
; todo/done
;

; done: test for delta_z > delta_y before attempting division

; done: perhaps cheat and do a divide table with four bits of zdelta and four bits of ydelta... visibility of 
; 16 each way, but we could ror the least significant bits off of the deltas and get a quick answer 
; as to where on the screen they go

; todo: pointer into the platform list representing where the player is standing (or was last standing if between platforms)

