 
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
playeryspeed ds 1  ; signed Y momentum
playerzspeed ds 1  ; signed Z momentum

; numeric output
num0    ds 1		; number to output, left

; various
tmp1	ds 1
tmp2	ds 1

NUMG0	= tmp2		; pattern buffer temp -- number output (already uses tmp1)
scanline = tmp1 

; level render
; these must be persistent between calls to platresume so the routine can pause and resume
curplat		ds 1		; which platform we are rendering or considering; used as an index into the level0 table; incremented by 4 for each platform
deltaz		ds 1		; how far forward the current platform line is from the player
deltay		ds 1 		; how far above or below the current platform the player is
lastline	ds 1		; last line rendered to the screen for gap filling

; faked jsr/rts
caller		ds 1		; which routine to return to

; framebuffer
view	ds [ $ff - view ]		; 100 or so lines; from $96 goes to $fa, which leaves $fd and $fe for one level of return for the 6502 call stack


;
; constants
;

viewsize	= [ $ff - view ]
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
; game logic
;

		MAC _gamelogic
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
		ENDM

;
; readstick
;

		MAC _readstick
		; bit 3 = right, bit 2 = left, bit 1 = down, bit 0 = up, one stick per nibble
		lda SWCHA
		and #$f0
		eor #$ff
readstick0
        ; bit 0 = up
		tax
		and #%00010000
		beq readsticka
		ldy playery
		cpy #$ff
		beq readsticka ; don't go over
		inc playery ; XXX testing
readsticka
        ; bit 1 = down
		txa
		and #%00100000
		beq readstickb
		ldy playery
		cpy #$00
		beq readstick5 ; don't go over
		dec playery ; XXX testing
readstickb
		lda playery
		; sta num0 ; XXXX
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
		ENDM

;
; _arctan (formerly platlinedelta)
;

; takes deltaz and deltay
; index the arctan table with four bits of each delta
; we use the most significant non-zero four bits of each delta
; the arctangent table is indexed by the ratio of the y and z deltas
; this means we can scale the values up to get more precision as long as we scale them up together
; we shift right up to four times until ( deltay | delta ) <= 0x0f
; deltaz is in tmp1 and deltay is in tmp2 where they get shifted to the right
; this also adds half of the screen height or subtracts from half screen height as appropriate to convert to scan line number

; first stuff case of the deltay = 0 before we look at other stuffed cases; deltay = 0 platforms are drawn dead center of the screen
; we were stuffing that case, but then the arctangent table get fixed to handle it correctly so it got removed, then we started stuff
; deltaz = 0 to return either 0 or viewsize-1 and that broke that, so we started stuffing deltay = 0 again.

; XXXX since we now stuff Y=0, we can start the arctangent table on Y=1 for a little more precision

		MAC _arctan
.platlinedelta
		lda deltay
		beq .platlinedeltastuff2	; stuff the case where deltay = 0

		_absolute
		sta tmp2				; tmp2 has abs(deltay)

		cmp deltaz				; is deltaz = deltay?
		bne .platlinedeltago	; branch on the normal case where deltaz != deltay

.platlinedeltastuff0
		; deltaz = deltay or deltaz = 0; stuff the return value to be the very top or very bottom scanline, depending
		bit deltay
		bpl .platlinedeltastuff1
		; platform is above us
		lda #[viewsize - 1]
        jmp .platarctan9
.platlinedeltastuff1
		; platform is below us
		lda #0
		jmp .platarctan9

.platlinedeltastuff2
		lda #[viewsize/2]
		bne .platarctan9

.platlinedeltago
		lda deltaz
		beq .platlinedeltastuff0	; stuff the case where deltaz = 0; depending on deltay, we either return 0 or viewsize-1 for the scanline
		sec
		sbc tmp2
		sta tmp1				; tmp1 has deltaz - abs(deltay)
		ora tmp2				; combined bits so we only have to test one value to see if all bits are clear of the top nibble
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
		bit deltay				; handle the separate cases of the platform above us and the platform below us
		bpl .platarctan1		; branch if deltay is positive; this means that the platform is lower than us
		lda arctangent,y		; platform is above us; add the arctangent value to the middle of the screen
		clc
		adc #(viewsize/2)		; 'view' is upside down, so adding relative to the middle of it moves things towards the top of the screen
        jmp .platarctan9
.platarctan1
		lda #(viewsize/2)		; platform is below us; subtract the arctangent value from the middle of the screen
		sec
		sbc arctangent,y		; 'view' is upside down, so subtracting relative the middle of it moves things towards the bottom of the screen
		; negative value indicates off screen angle; return the negative value to proprogate the error
.platarctan9
		ENDM

;
; _plathypot
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

		MAC _plathypot
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
; _plotonscreen
;

; Y gets the distance, which we use to figure out which size of line to draw
; X gets the scan line to draw at
; updates view[]
; uses tmp1 during the call to remember the new lastline
; this macro is used in three different places; okay, it's only used in one place with fatlines disabled but we're trying to add gap filling logic
; instead of sticking Y (distance) into tmp2 to remember it, hold the current color in tmp2

		MAC _plotonscreen
.plotonscreen1

		cpx lastline			; are we drawing on top of the last line we drew for this platform?
		; beq .plotonscreen3a	; skip straight to drawing it if we're overwriting a platform of the same color; this won't be safe if multiple platforms on the level are the same color! XXX
		beq .plotonscreen9		; then do nothing

		stx tmp1				; hold the new lastline here until after we're done recursing to fill in the gaps; only do this when we're first called, not when we recurse

		; fetch the current platform color and stash it in tmp2 then put tmp1 back into X
		ldx curplat
		lda level0+3,x
		sta tmp2
		ldx tmp1

.plotonscreen2
		lda view,x				; get the line width of what's there already
		and #%00011111			; mask off the color part and any other data
		cmp perspectivetable,y	; compare to the fatness of line we wanted to draw
		bpl .plotonscreen4		; what we wanted to draw is smaller.  that means it's further away.  skip it.  but still see about filling in gaps.

.plotonscreen3
		; actually plot this line on the screen
.plotonscreen3a
		lda perspectivetable,y	; perspectivetable translates distance to on-screen platform line width; 128 entries starting with 20s, winding down to 1s
		ora tmp2				; add the platform color
		sta view,x				; draw to the framebuffer

.plotonscreen4
		; figure out if we want to fill in the gap from here to the last line we drew
		lda lastline			; make sure that there is a lastline and don't try to fill gaps if not
		beq .plotonscreen8		; skip gap filling if there is no lastline

;		lda SWCHB
;		and #%00000010			; select switch
;		beq .plotonscreen8 		; XXX testing; select switch disables filling in gaps

		lda INTIM
		cmp #8
		bmi .plotonscreen8		; XXX experimental; running low on CPU time remaining also disables filling in gaps; hey, this works great, except for, you know, the gaps

		txa
		sec
		sbc lastline
		cmp #1
		beq .plotonscreen8		; if lastline minus curline is exactly 1 or -1 then our work is done; bail out; don't overwrite a narrow line with a fatter line
		cmp #$ff
		beq .plotonscreen8		; if lastline minus curline is exactly 1 or -1 then our work is done; bail out; don't overwrite a narrow line with a fatter line

		cmp #0
		bmi .plotonscreen6		; branch if we're now drawing upwards relative the last plot; else we're drawing downwards relative the last plot
.plotonscreen5
		inc num0				; XXXX count how many lines we fill in 
		dex						; drawing downwards relative last plot; step back up one line and draw there
		jmp .plotonscreen2		; recurse back in
.plotonscreen6
		inc num0				; XXXX count how many lines we fill in
		inx						; drawing upwards relative last plot
		jmp .plotonscreen2		; recurse back in

.plotonscreen8
		ldx tmp1				; after we're done recursing to fill in the gaps, update lastline
		stx lastline
.plotonscreen9
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

		; initialize ram to 0
		lda #0
		ldx #$80
reset0  sta $80,x
		dex
		bne reset0

		; hardware
		sta $281				; all joystick pins for input

		; player location on map
		ldy #2
		sty playerz
		ldy #32
		sty playery



;
; platform graphics
;


startofframe

		lda #$00
		sta COLUBK

		lda #%00000101		; reflected playfield with priority over players
		sta CTRLPF

		lda #0
		sta VBLANK
		; sta PF0
		; sta PF1
		; sta PF2

		ldy #viewsize			; indexes the view table and is our scanline counter
		sty scanline

		lda playery				; 1/4th playery for picking background color for shaded sky/earth
		lsr
		lsr
		sta tmp2

		sta WSYNC			
		bne renderpump			; always

platforms

; high bit or something should indicate a bit of sprite
; we get 22 cycles before drawing starts, and then 76 total for the scan line
; 69 cycles; if we take out the wsync, we have 7 cycles left; that's enough to copy sprite data from a pre-computed table, but we already use all of our RAM.  argh.
; 62 cycles!  14 cycles to spare.

		sta WSYNC			; +3   62
		sty COLUPF			; +3    3

		tay					; +2    5
		lda background,y	; +4    9
		sta COLUBK			; +3   12

		lda pf0lookup,x		; +4   16
		sta PF0				; +3   19 ... this needs to happen sometime on or before cycle 22
		lda pf1lookup,x		; +4   23
		sta PF1				; +3   26 ... this needs to happen some time before cycle 28; cycle 24 is working
		lda pf2lookup,x		; +4   30
		sta PF2				; +3   33
renderpump

		; get COLUPF, COLUBK, and scanline values ready to roll

; to shave a few cycles and minimize moving things around, try to put the pf*lookup index into S instead, and COLUPF in A?  nope.
; would it be faster to put scanline back into RAM rather than trying to use S?

		ldy scanline		; +3   36

		; get value for COLUPF ready in Y
		lax view,y			; +4   40 
		ldy platformcolors,x; +4   44

		; get the pf*lookup index ready in X
		and #%00011111		; +2   46
		tax					; +2   48

        ; get value for COLUBK somewhat setup in A
		lda scanline		; +3   51
		adc tmp2			; +3   54

		dec scanline		; +5    59
        bne platforms		; +3    62

renderdone


;
; debugging output (a.k.a. score)
;

		; re-adjust after platform rendering
		sta WSYNC			; don't start changing colors and pattern data until after we're done drawing the plast platform line
		ldx #$ff			; we use the S register as a temp so restore it to the good stack pointer value of top level execution
		txs
		; black to a back background
		lda #$00
		sta COLUBK

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

		_readstick
		_gamelogic
		lda #0					; return to return0
		sta caller
		jmp platlevelclear		; start at the beginning
return0
		sta WSYNC


; 30 lines of overscan, which means a timer for 29 frames plus a WSYNC

		lda #34					; 29*76/64 = 34.4375, plus the WSYNC at the end
		sta TIM64T
		lda #1					; return to return1
		sta caller
		jmp platresume
return1
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
		lda #2					; return to return2
		sta caller
		jmp platresume
return2
		lda #%01000000 ; turn VBLANK off and the joystick latch on; joystick triggers will now right read 1 until trigger is pressed, then it will stick as 0
		sta VBLANK
		sta WSYNC

; and back to drawing the screen
;

		jmp startofframe

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
		sty num0	; XXXX counting how many platform lines we render, or other platlevelclear+platresume specific metrics
		sty deltaz
		sty curplat
		sty lastline

		; zero out the framebuffer
		ldy #viewsize-1
		ldx #0
platlevelclear2
		stx view,y
		dey
		bne platlevelclear2
		jmp platnext0
		
platresume
		ldy curplat			; where we in middle of a platform (other than the 0th one)?
		beq platnext0		; starting at zero, so go seek to the first platform the player can actually see
		lda level0,y		; did we already hit the end of the level last call this frame?
		beq platnext0vblanktimer ;  if so, just go busy spin
        bne platrenderline     ; yeah?  continue rendering the current platform

platnext0
		; is there a current platform?  if not, go busy spin on the timer.  if so, figure out if any of it is still in front of the player.
		; this condition is reset when platlevelclear is called.
		ldy curplat				; offset into the level0 table
		lda level0,y			; load the first byte, the Z start position, of the current platform
		bne platnext1			; not 0 yet, so we have a platform to evaluate and possibily render if it proves visible
platnext0vblanktimer
		jmp vblanktimerendalmost	; no more platforms; just burn time until the timer expires

platnext1
		; skip to the next platform again unless one ends somewhere in front of us
		lda level0+1,y			; get the end point of the platform, since the end is the interesting part to test for to see if we can see any of this platform
		cmp playerz				; compare to where the player is
		beq platnext			; skip rendering this one if the end is exactly where the player is at; only render stuff forward of us; mostly, we don't want to fall into platrenderline from here starting with a 0 deltaz
		bpl platfound			; playerz <= end-of-this-platform, so show the platform
		; otherwise, fall through to trying the next platform

platnext
		; seek to the next platform and take a look at doing it
		ldy curplat
		iny
		iny
		iny
		iny
		sty curplat
		jmp platnext0

platfound
		; a platform was found that ends in front of us; initialize deltay, deltaz and start doing lines from a platform
		lda #0					; blank out the lastline so we don't try to gapfill to it when we start rendering the next platform
		sta lastline
		lda level0+1,y			; get platform end
		sec
		sbc playerz
		sta deltaz				; end of the platform minus playerz is deltaz
		lda playery
		sec
		sbc level0+2,y			; subtract the 3rd byte, the platform height
		sta deltay				; deltay is the difference between the player and the platform, signed

; work backwards from the last visible line using deltaz as the counter

platrenderline

		; if deltay > deltaz, this bit of the platform isn't visible
		; eg, for the first platform, deltaz does 8,7,6,5,4,3,2 and deltay is 2
		; since we render back to front, we know the rest of the platform isn't visible either, so stop rendering this one and go to the next
		; this logic avoids the relatively expensive call to arctan
		; update, if deltay = deltaz, we want to render one last bit of platform at the very top/bottom of the screen in this case
		; previous platform lines plotted to the screen will get connected to it
		lda deltay
		_absolute
		cmp deltaz
		bmi platrenderline1
		beq platrenderline1
		jmp platnext			; do this if deltay > deltaz
platrenderline1

		_arctan					; takes deltaz and deltay; uses tmp1 and tmp2 for scratch; returns an arctangent value in the accumulator from a table which we use as a scanline to draw too
		; bmi platnext			; negative return value indicates that the angle is steeper than our field of view; since we're working backwards from the end of the platform towards ourselves, we know we won't be able to see any lines closer to us if we can't see this one, so just skip to platnext; we seem to be avoiding this situation currently so commenting this check out for now
		tax
		txs						; using the S register to store our value for curlineoffset

		_plathypot				; reads deltay and deltaz directly, returns the size aka distance of the line in the accumulator

		tay						; Y gets the distance, fresh back from plathypot, which we use to figure out which size of line to draw
		tsx						; X gets the scanline to draw at; value for curlineoffset is hidden in the S register

		_plotonscreen			; Y gets the distance away/platform line width, X gets the scanline to draw at

platnextline

		lda INTIM
		; at least 5*64 cycles left?  have to keep fudging this.  last observed was 5, so one for safety.  then did gap filling since then.
		; cmp #6
		bpl platnextline0a		; the largest vaglue this will ever be loaded with is 96, which is less than 127; if we observe the 7 bit set, it means that we ran out of time on the timer
ranoutoftime		; XXXX set a debugger breakpoint here; yup, it runs out of time a lot, and 04_runtime.pl agrees that stuff is taking way too much time
		nop
platnextline0a	; XXX testing
		cmp #8
		bmi vblanktimerendalmost
platnextline0

		; inc num0				; XXX counting how many platform lines we render in a frame

		dec deltaz				; deltaz goes down to zero; doing this after the timer test instead of before probably means that when we come back, we redo the same line that we just did, but the alternative is mindly jumping into doing the line when we come back without first doing the (below) check to see if we should be doing it.

		; deltaz = 0 is okay; stop on deltaz < 0
		bmi platnextline3		; on deltaz < 1, branch to platnext to start in on the next platform; we've walked backwards past the players position for this platform
		; for platforms that start in front of us, only draw them back to the point where they start
		; if the start of the platform is behind us, then the deltaz check above will catch that so don't worry about that case
		lda deltaz
		ldy curplat
		lda level0,y			; don't take deltaz below level0+0,y - playerz; aka, stop when deltaz reaches the start of the platform
		sec
		sbc playerz
		beq platnextline2		; we're exactly standing on the start of this platform so we don't need to do this deltaz test
		bmi platnextline2		; this test doesn't make sense if the platform starts behind us; if deltaz went < 0, the above test would have caught it, so just continue rendering
		; platform is in front of us; A has how many units ahead the platform is; we want to make sure that deltaz >= that
		; start of the platform is somewhere in front of us; deltaz should not count down to closer then the relative platform start; we want deltaz to be larger
		sec
		sbc deltaz				
		bmi platnextline2		; branch to do the next line if deltaz > ( level0,y - playerz )
		beq platnextline2		; branch to do the next line if deltaz = ( level0,y - playerz )
		; don't walk off of the beginning of the platform; fall through to platnext if deltaz < ( level0,y - playerz )

platnextline3
        jmp platnext

platnextline2
		jmp platrenderline		; if deltaz is larger than level0,y - playerz - 1, then go on to render the next line with our newly dec'd deltaz; too far for a relative branch

;
; timer
;

; execution is sent here when there isn't enough time left on the clock to render a line or do whatever other operation
; nothing to do but wait for the timer to actually go off

vblanktimerendalmost
		; lda #0				; XX testing
		; sta tmp1
vblanktimerendalmost1
		lda	INTIM
		beq vblanktimerendalmost2
		; inc tmp1			; XX testing -- how much time do we have to burn before the timer actually expires?
        jmp vblanktimerendalmost1
vblanktimerendalmost2
		; lda tmp1
		; sta num0 ; XX diagnostics to figure out how much time is left on the timer when platresume gives up
		; ldx #$fd			; we use the S register as a temp so restore it to the good stack pointer value; we only ever call one level deep so we can hard code this
		; fall through to computedreturn

computedreturn
		; this stupidity saves one byte of RAM
		lda caller
		asl
		tax
		lda returntable,x
		sta tmp1
		lda returntable+1,x
		sta tmp2
		jmp (tmp1)

;
;		return address table
;

returntable
		dc.w return0, return1, return2


;
;
; small subroutines
;
;

;
; collisions
;

; XXX so far, only called from unit tests

collisions
		; detect collisions XXXXXXXXXXXXX

		ldy #$ff
		sty tmp2				; use tmp2 to record which, if any, platform we're standing on; maybe platforms do something magical when we're standing on them so we want to know which one it is
		ldy #0
		sty tmp1				; use tmp1 for collision bits; bit 0 means we can't go up; bit 1 means we can't go forward
		; sty deltaz	; don't think we need this
		; sty curplat	; just keep it in Y?
collisions1
		lda level0,y			; load the first byte, the Z start position, of the current platform
		beq collisions9			; stop when there's no more platform data

		; make sure we're >= the start of it and <= the end of it
		lda playerz
		cmp level0+0,y
		bmi collisions3			; if playerz - platstart < 0, the platform hasn't started yet
		lda level0+1,y
		cmp playerz
		bmi	collisions3			; if platend - playerz < 0, we're off the end of the platform

		; are we standing on this platform?
		lda playery
		clc						; our feet are one unit above our head, I guess, so subtract one extra
		sbc level0+2,y			; subtract the 3rd byte, the platform height
		bne collisions2			; branch if not exactly one unit above the platform height
collisions1a					; label just here for the unit tests
		; okay, we're standing on this platform!
		sty tmp2
		; XX maybe jmp out?  or are we going to start checking for other kinds of collisions?
; XXXX should probably have done the z checks first and then we could both check if we're standing on the platform or if we've hit our head
		nop
		jmp collisions8			; go on to the next platform

collisions2
		; okay, are we hitting our head?
		lda playery
		sec						; is our head exactly at the platform level?  bump.
		sbc level0+2,y
		bne collisions3			; not exactly so not hitting our head
collisions2a
		; we're hitting our head on this platform
		lda tmp1
		and #%00000001
		sta tmp1
		jmp collisions8			; go on to the next platform

collisions3
		; are we walking in to this platform?
		lda level0,y
		clc
		sbc playerz
		bne collisions8
		lda level0+2,y
		cmp playery
		bne collisions8
collisions3a
		; we're walking in to this platform
		lda tmp1
		and #%00000010
		sta tmp1
		jmp collisions8			; go on to the next platform

collisions8
		; try another platform?
		iny
		iny
		iny
		iny
		; sty curplat
		jmp collisions1

collisions9
		rts ; XXXX make this a macro



;
;
; tables
;
;

		align 256

; playfield data for each register, indexed by width of the platform to draw
; 20 entries
; furtherest away platform (no data) first then full width platform at pf*lookup[19]
; pf0 is 4 bits wide and is the first four bits drawn on the left edge of the screen (and right edge since we're mirrored); bits are stored in the high nibble; bits are also stored in reverse order than how drawn
; pf1 is the next 8 bits and pf2 the last 8 bits ending at the center of the screen
; currently, the zero index of these tables are never used; we never render zero width chunks of platform, no matter how far away the platform bit is

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

;
; distancemods
;

; used in the plathypot routine

; things higher or lower than us are farther away than things right in front of us.
; this table indicates how many times the zdelta (distance ahead) needs to be added back to itself to
; compensate for vertical difference, to more accurately compute distance.
; the largest bit indicates that 1/4th of the value needs to be added back in.
; next bit left indicates that 1/8th of the value needs to be added back in to itself.
; the next bit indicates that 1/16th of the value needs to be added back in to itself.

; a table of multipliers like this is more space effecient than eg a 32x32 table (1k) of results and has better numeric range.

; # flag: flag:  shift it right twice (/4) and add it back in.  another: (/8) and add it back in.
; # go from +0 modification to distance (point straight ahead) to a 45 degree hypot (~ 1.4 times longer)
; # straight ahead is $yd 0 and $zd maybe 127 or something.
; # 45 degrees is $yd 127, $zd 127, for instance.

; my $zd = 127;
; for my $yd (1..127) {
;     next if $yd % 2;
;     my $n = (sqrt($zd**2 + $yd**2)/127)-1;
;     my $byfour = 0;
;     my $byeight = 0;
;     my $bysixteen = 0;
;     if($n >= 1/4) {
;         $byfour = 1;
;         $n -= 1/4;
;     }
;     if($n >= 1/8) {
;         $byeight = 1;
;         $n -= 1/8;
;     }
;     if($n >= 1/16) {
;         $bysixteen = 1;
;         $n -= 1/16;
;     }
;     print "     dc.b #%$byfour$byeight$bysixteen" . "00000\n";
; }

		align 256

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

;
; arctangent
;

; atan(x/y) normalized to between 0 and #(viewsize/2)
; $x and $y are 0..15

; this table is indexed by two nibbles.
; the high nibble is indexed by deltay.
; the low nibble is indexed by deltaz-deltay.
; since deltay is never (much) larger than deltaz, we don't need to store cases in the table where deltay > deltaz.
; subtracting deltaz out of deltaz first gives us more precision for things off in the distance.

;   Y  Z-->
;   |
;   V

; build a 256 byte table of possible inputs for deltax and deltay translated to angle
; output is scaled so it fits in 0-55 to allow for a 110 line tall window
; with a 45 degree up and down viewing angle, we'd get a 90 scanline high screen
; given a 110 line display instead of 90, perl -e 'print 110/90;', we have to multiply the angles by 1.2 to get scanlines
; okay, these get added to/subtracted from scanline 55 (given 110 scan lines) and we can't hit 110, only 109, so clamp it to 54
;
; use Math::Trig;
; my $scanlines = 112;
; my $max = int($scanlines / 2); $max-- if(( $scalines & 0b01) == 0);
; my $field_of_view_in_angles = 90;
; my $multiplier = $scanlines / $field_of_view_in_angles;
; for my $y (0..15) {
;    my @z = $y+1 .. $y+16;
;    # my @z = $y .. $y+15; # we lose a significant amount of detail this way
;    print "\t\t; z = @{[ join ', ', @z ]}\n";
;    print "\t\tdc.b ";
;    for my $z (@z) {
;          if( $z == 0 ) { print '%0000000, '; next; }
;          my $angle = int(rad2deg(atan($y/$z))*$multiplier);
;          $angle = $max if $angle > $max; # with the above, this never happens; we're about 3 away from either bound!
;          print $angle;
;          print ", " if $z != $z[-1];
;    }
;    print "; y = $y\n";
; }
; print "\n";

; we will never see anything with a Y > Z because it's outside of our 45 degree field of view.  wait, not exactly.
; our field of view is bounded by this table.  in the most extreme case case, Z=1 and Y=15, or else Z=15 and Y=1.
; this table essentially gives the scan lines to draw those at, relative the center of the screen #(viewsize/2).


		align 256

arctangent

                ; z = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16
                dc.b 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0; y = 0
                ; z = 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17
                dc.b 32, 22, 17, 13, 11, 9, 8, 7, 6, 6, 5, 5, 4, 4, 4, 4; y = 1
                ; z = 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18
                dc.b 41, 32, 26, 22, 19, 17, 15, 13, 12, 11, 10, 9, 9, 8, 8, 7; y = 2
                ; z = 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19
                dc.b 45, 37, 32, 28, 25, 22, 20, 18, 17, 15, 14, 13, 12, 12, 11, 10; y = 3
                ; z = 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
                dc.b 47, 41, 36, 32, 29, 26, 24, 22, 20, 19, 18, 17, 16, 15, 14, 13; y = 4
                ; z = 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21
                dc.b 48, 43, 39, 35, 32, 29, 27, 25, 24, 22, 21, 20, 18, 18, 17, 16; y = 5
                ; z = 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22
                dc.b 49, 45, 41, 37, 34, 32, 30, 28, 26, 25, 23, 22, 21, 20, 19, 18; y = 6
                ; z = 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23
                dc.b 50, 46, 42, 39, 36, 34, 32, 30, 28, 27, 25, 24, 23, 22, 21, 20; y = 7
                ; z = 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24
                dc.b 50, 47, 44, 41, 38, 36, 34, 32, 30, 29, 27, 26, 25, 24, 23, 22; y = 8
                ; z = 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25
                dc.b 51, 48, 45, 42, 40, 37, 35, 34, 32, 30, 29, 28, 27, 26, 25, 24; y = 9
                ; z = 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26
                dc.b 51, 48, 45, 43, 41, 39, 37, 35, 33, 32, 31, 29, 28, 27, 26, 25; y = 10
                ; z = 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27
                dc.b 51, 49, 46, 44, 42, 40, 38, 36, 35, 33, 32, 31, 30, 29, 28, 27; y = 11
                ; z = 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28
                dc.b 52, 49, 47, 45, 43, 41, 39, 37, 36, 34, 33, 32, 31, 30, 29, 28; y = 12
                ; z = 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29
                dc.b 52, 50, 47, 45, 43, 42, 40, 38, 37, 36, 34, 33, 32, 31, 30, 29; y = 13
                ; z = 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30
                dc.b 52, 50, 48, 46, 44, 42, 41, 39, 38, 36, 35, 34, 33, 32, 31, 30; y = 14
                ; z = 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31
                dc.b 52, 50, 48, 46, 45, 43, 41, 40, 39, 37, 36, 35, 34, 33, 32, 31; y = 15

;
; platformcolors
;

platformcolors

; in the framebuffer, the top three bits are the color and the bottom five bits (but only up to dec 29) are the line width;
; this is a quick translation to the top four bits being color and the bottom four being the scaled brightness (based on line width which implies distance)
; 8 lines with 32 entries on each (but only the first 29 items on each line will ever be used)
; we want a log drop off
; not quite right; still dicking with this


; use strict;
; use warnings;
; use Math::Trig;
; # 30 datapoints (0-29)
; # each in the range of 0-15
; # so convert 0-29 to 0-90 and then scale that to 0-15
; my @brightnesses;
; for(my $i = 0; $i < 32; $i++ ) {
;     my $width = $i > 29 ? 29 : $i;
;     # push @brightnesses, int( rad2deg( cos( deg2rad($stop) ) ) );
;     my $multiplier = 29 / sqrt(29);
;     push @brightnesses, $multiplier * sqrt $width;
; }
; warn "brightnesses: @brightnesses\n";
; @brightnesses = reverse @brightnesses;
; for my $b (@brightnesses ) {
;     print 'X' x $b , "\n";
; }
; for( my $color = 0; $color <= 0b11100000; $color += 0b00100000 ) {
;     print "\t\t.byte ";
;     print join ', ', map { sprintf "%%%08b", $_ } map { $color | int($_) } @brightnesses;
;     print "\n";
; }

		.byte %00011101, %00011101, %00011101, %00011100, %00011011, %00011011, %00011010, %00011010, %00011001, %00011001, %00011000, %00011000, %00010111, %00010110, %00010110, %00010101, %00010100, %00010100, %00010011, %00010010, %00010001, %00010001, %00010000, %00001111, %00001110, %00001101, %00001100, %00001010, %00001001, %00000111, %00000101, %00000000
		.byte %00111101, %00111101, %00111101, %00111100, %00111011, %00111011, %00111010, %00111010, %00111001, %00111001, %00111000, %00111000, %00110111, %00110110, %00110110, %00110101, %00110100, %00110100, %00110011, %00110010, %00110001, %00110001, %00110000, %00101111, %00101110, %00101101, %00101100, %00101010, %00101001, %00100111, %00100101, %00100000
		.byte %01011101, %01011101, %01011101, %01011100, %01011011, %01011011, %01011010, %01011010, %01011001, %01011001, %01011000, %01011000, %01010111, %01010110, %01010110, %01010101, %01010100, %01010100, %01010011, %01010010, %01010001, %01010001, %01010000, %01001111, %01001110, %01001101, %01001100, %01001010, %01001001, %01000111, %01000101, %01000000
		.byte %01111101, %01111101, %01111101, %01111100, %01111011, %01111011, %01111010, %01111010, %01111001, %01111001, %01111000, %01111000, %01110111, %01110110, %01110110, %01110101, %01110100, %01110100, %01110011, %01110010, %01110001, %01110001, %01110000, %01101111, %01101110, %01101101, %01101100, %01101010, %01101001, %01100111, %01100101, %01100000
		.byte %10011101, %10011101, %10011101, %10011100, %10011011, %10011011, %10011010, %10011010, %10011001, %10011001, %10011000, %10011000, %10010111, %10010110, %10010110, %10010101, %10010100, %10010100, %10010011, %10010010, %10010001, %10010001, %10010000, %10001111, %10001110, %10001101, %10001100, %10001010, %10001001, %10000111, %10000101, %10000000
		.byte %10111101, %10111101, %10111101, %10111100, %10111011, %10111011, %10111010, %10111010, %10111001, %10111001, %10111000, %10111000, %10110111, %10110110, %10110110, %10110101, %10110100, %10110100, %10110011, %10110010, %10110001, %10110001, %10110000, %10101111, %10101110, %10101101, %10101100, %10101010, %10101001, %10100111, %10100101, %10100000
		.byte %11011101, %11011101, %11011101, %11011100, %11011011, %11011011, %11011010, %11011010, %11011001, %11011001, %11011000, %11011000, %11010111, %11010110, %11010110, %11010101, %11010100, %11010100, %11010011, %11010010, %11010001, %11010001, %11010000, %11001111, %11001110, %11001101, %11001100, %11001010, %11001001, %11000111, %11000101, %11000000
		.byte %11111101, %11111101, %11111101, %11111100, %11111011, %11111011, %11111010, %11111010, %11111001, %11111001, %11111000, %11111000, %11110111, %11110110, %11110110, %11110101, %11110100, %11110100, %11110011, %11110010, %11110001, %11110001, %11110000, %11101111, %11101110, %11101101, %11101100, %11101010, %11101001, %11100111, %11100101, %11100000

; upside down, to match the view buffer
; 22+ sets of 5 to match the 110 lines of frame buffer

background

        .byte $26, $26, $26, $26, $26
        .byte $26, $26, $26, $26, $26  ; lightest dirt -- this winds up staying on the screen past the end of the framebuffer

        .byte $24, $24, $24, $24, $24
        .byte $24, $24, $24, $24, $24
        .byte $24, $24, $24, $24, $24   ; lighter dirt

        .byte $22, $22, $22, $22, $22   ; dirt

		.byte $9a, $9a, $9a, $9a, $9a	; lighest sky
		.byte $9a, $9a, $9a, $9a, $9a
		.byte $9a, $9a, $9a, $9a, $9a

		.byte $98, $98, $98, $98, $98	; middle sky
		.byte $98, $98, $98, $98, $98
		.byte $98, $98, $98, $98, $98
		.byte $98, $98, $98, $98, $98
		.byte $98, $98, $98, $98, $98
		.byte $98, $98, $98, $98, $98
		.byte $98, $98, $98, $98, $98
		.byte $98, $98, $98, $98, $98
		.byte $98, $98, $98, $98, $98
		.byte $98, $98, $98, $98, $98
		.byte $98, $98, $98, $98, $98
		.byte $98, $98, $98, $98, $98

		.byte $96, $96, $96, $96, $96	; less dark sky
		.byte $96, $96, $96, $96, $96
		.byte $96, $96, $96, $96, $96

		.byte $84, $84, $84, $84, $84	; dark sky
		.byte $84, $84, $84, $84, $84
		.byte $84, $84, $84, $84, $84
		.byte $84, $84, $84, $84, $84
		.byte $84, $84, $84, $84, $84
		.byte $84, $84, $84, $84, $84
		.byte $84, $84, $84, $84, $84
		.byte $84, $84, $84, $84, $84
		.byte $84, $84, $84, $84, $84

		.byte $82, $82, $82, $82, $82	; even darker sky
		.byte $82, $82, $82, $82, $82
		.byte $82, $82, $82, $82, $82
		.byte $82, $82, $82, $82, $82

		.byte $80, $80, $80, $80, $80	; darkest sky
		.byte $80, $80, $80, $80, $80
		.byte $80, $80, $80, $80, $80
		.byte $80, $80, $80, $80, $80

		.byte $00, $00, $00, $00, $00
		.byte $00, $00, $00, $00, $00
		.byte $00, $00, $00, $00, $00
		.byte $00, $00, $00, $00, $00

		.byte $00, $00, $00, $00, $00
		.byte $00, $00, $00, $00, $00
		.byte $00, $00, $00, $00, $00
		.byte $00, $00, $00, $00, $00

;
; perspectivetable
;

perspectivetable

; line widths
; let's say all platforms are the same width... perhaps 20 to start with (since that's how many pixels
; wide we can physically draw them)
; projection for X is like Y:
; projection: projected y = distance_of_"screen" * actual_y / distance_of_point
; projected x (0-20) = distance_to_screen (eg, 4) * actual_x (always 20) / distance_of_point (0..127 or so, re-use distance from Y calc)
; okay, tweaking variables to draw out the tail, even if it overflows early on

; for my $x (1..128) {
;     my $px = 4 * 40 / $x;
;     $px = 20 if $px > 20;
;     print int($px), ', ';
; }

		 dc.b 19, 19, 19, 19, 19, 19, 19, 19, 17, 16, 14, 13, 12, 11, 10, 10, 9, 8, 8, 8, 7, 7, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1

;
; level0
;

level0
        ; platform start point in the level, platform end point, height of the platform, color (3 bits only, so only even numbers)
        ; eg, this first one starts at 1, is 10 long, is 30 high, and is green
		dc.b 1, 11, $1e,  $e0
		dc.b 20, 25, $14, $60
		dc.b 30, 40, $18, $20
		dc.b 0, 0, 0, 0 		;       end
		dc.b 0, 0, 0, 0 		;       end


 		dc.b $fe  ; light green
		dc.b $5c  ; pink
		dc.b $6c  ; light purple
		dc.b $b8  ; blue-green




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

