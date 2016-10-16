;----------------------------------------------;
;                                              ;
; Basic Atari 2600 kernel                      ;
;                                              ;
; Philip Bull 2016                             ;
;                                              ;
; Based on Kirk Israel's "thin red line" demo  ;
;                                              ;
;----------------------------------------------;


;
; First inform the assembler which processor we are coding for
; The 2600 uses a 6507, which is basically a 6502, with fewer address lines and
; interrupts disabled
;
	PROCESSOR 6502

;
; Now include all the Atari support stuff
;
	INCLUDE "vcs.h"
	INCLUDE "macro.h"

;
; The ORG directive tells the assembler where in memory to place
; all the code that follows.
; $F000 is the preferred address for Atari programs
;
	ORG $F000

;
; Start of program code
;
Start

;
; When the Atari starts up, all its memory is uninitialized. 
; Do the following to tidy everything up
;
	SEI				; Disable interrupts
	CLD  	 		; Clear BCD math bit.
	LDX #$FF 		; set X to the top of the memory we have,
	TXS	 			; ...and set the stack pointer to what X is...i.e. #$FF aka 255
	
;
; Walk through our memory and zero it all out
;
	LDA #0			; Put Zero into A (X is at $FF)
ClearMem
	STA 0,X			; Store accumulator into location X (offset by 0)
	DEX				; Decrement X
	BNE ClearMem	; If the last command resulted in something
					; non-zero, branch to "ClearMem"

; --- End of initialization


; Game setup
	LDA #$00		; $00 = black
	STA COLUBK		; Store color into background color register

	LDA #$45
	STA COLUP0		; P1/P1 missile color

	LDA #%00000010	; First time through, blank the screen. Once we are in the main loop
	STA VBLANK		; this will be done in our overscan section

; -----------------------------------------------------------------------------


;
; Start of main loop (one game frame occurs here)
;
; We have four distinct sections:
;   Vertical Sync - starts up new frame
;	Vertical Blank - 37 scanlines during we which we can do game logic
;	Horizontal Blank/Scanline drawing (we have some hblank time for game code here,
;										but it's mainly for rendering)
;	Overscan - 30 scanlines for game logic
;

MainLoop

;
; Vertical Sync
; -------------
; 
; According to SPG, bit D1 of VSYNC needs to be set to 1 for at least two scanlines
; and then set back to 0 to trigger a new frame
;
	LDA #%00000010
	STA VSYNC

	STA WSYNC	; Hold it for two scanlines...we could do something in this period
	STA WSYNC	; but for now just hold.

	LDA #%00000000
	STA VSYNC	; Turn off VSYNC

; -----------------------------------------------------------------------------


;
; Vertical Blank (37 scanlines)
; -----------------------------
;
; We will probably want to do some game logic execution during the 37 scanline period
; we have available here. The best way to do this is to fire off a timer which will run
; in the background and then execute our code. The code we execute should take no longer
; than 37 scanlines. Once we have done everything we want to do we sit in a loop waiting
; for the timer to complete.
;
; Each scanline takes 76 cycles (which are the same thing our clock is geared to)
; The VBLANK period therefore amounts to 2812 cycles (37 x 76)
; However, we need to account for the 6 cycles it takes to set the timer. Plus the 
; checking loop is only accurate to 6 cycles, which gives us 12 cycles subtracted
; from the 2812.
; (2812 - 12) = 2800.
;
; The timer we are going to use is TIM64T, which performs one tick every 64 cycles. 
; Therefore we require (2800 / 64) = 43.75 ticks (but we round down to 43)
;
	LDA #43		; Load "43" into the accumulator
	STA TIM64T	; Store the accumulated value in the TIM64T register

;
; This is now the area in which we can execute our code, provided it doesn't take any 
; longer than 37 scanlines
;

;
; 		-- DO STUFF HERE --
;

;
; Once we've done our stuff, we are going to sit in a tight loop and wait for the timer
; to end
;
VBlankLoop
	LDA INTIM			; Load timer value into accumulator
	BNE VBlankLoop		; Loop back if the timer value is not zero
	
	; Timer has expired but there's a good chance we'll be some way through a scanline
	; here, so sit tight until we get to the end and then turn off the VBLANK
	STA WSYNC
	STA VBLANK			; End VBLANK period with the zero we have in the accumulator
	
; -----------------------------------------------------------------------------
	

;
; Horizontal Blank/Scanline draw
; ------------------------------
;
; Here we are going to manually count down each scanline. The Y register will hold
; the current scanline value
;	

	LDY #192		; Using Y register as scanline counter and counting off 192 scanlines
	
	LDA #2			; Storing a 2 in ENAM0 (i.e. bit D1) will enable Missile 0
	STA ENAM0

	LDA #$F0
	STA HMM0		; stick that in the missile mover



ScanLoop
	; After 68 clock cycles in this loop the TIA will begin drawing the scanline

	;
	; 		-- DO SCANLINE STUFF HERE --
	; 

	DEY				; Decrement the scanline counter
	STA WSYNC 		; Wait for the line to finish
	BNE ScanLoop	; Loop if there are remaining scanlines

	STA HMOVE

; -----------------------------------------------------------------------------


;
; Overscan
; --------
;
; Same deal as the VBLANK timer, except we have 30 scanlines to work with.
; We will set up the timer accordingly. Our two loads (one for the VBLANK and
; one to set the timer value) take up 5 and 6 clock cycles respectively (11 in total) and the
; timer check loop requires 6.
;
;   (30 * 76) = 2280 clock cycles
; (2280 - 17) = 2263
; (2263 / 64) = 35.359
; ~35 timer ticks 
;
	LDA #2			; Write "2" for the VBLANK
	STA VBLANK 		; Make TIA output invisible for the overscan,
					; (and keep it that way for the vsync and vblank)

	LDA #35			; Load "35" into the accumulator
	STA TIM64T		; Store the accumulated value in the TIM64T register


;
; This is now the area in which we can execute our code, provided it doesn't take any 
; longer than 30 scanlines
;

;
; 		-- DO STUFF HERE --
;

;
; Once we've done our stuff, we are going to sit in a tight loop and wait for the timer
; to end
;
OverscanLoop
	LDA INTIM			; Load timer value into accumulator
	BNE OverscanLoop	; Loop back if the timer value is not zero

	STA WSYNC			; Again, we're probably in the middle of a scanline here. Wait
						; for it to complete
	JMP MainLoop		; END OF FRAME - jump back to main game loop

; -----------------------------------------------------------------------------


;
; Final housekeeping
;
; There are two special memory locations, $FFFC and $FFFE.
; When the atari starts up, a "reset" is done. When this happens, the 6502 looks at
; memory location defined by the bytes $FFFC and $FFFD and then goes to that 
; location. The first .word Start tells DASM to put the binary data that we labeled 
; "Start" at the location we established with org. 
;
; This is repeated for $FFFE/$FFFF, which is for a special event called a BRK
;
	ORG $FFFC
	.word Start
	.word Start