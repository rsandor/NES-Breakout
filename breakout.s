;;;;;;;;;;;;;; Header / Startup Code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.segment "HEADER"    
	.byte   "NES", $1A      ; iNES header identifier
	.byte   2               ; 2x 16KB PRG code
	.byte   1               ; 1x  8KB CHR data
	.byte   $01, $00        ; mapper 0, vertical mirroring

.segment "STARTUP"

.segment "CODE"

reset:
.include "include/reset.s"

;;;;;;;;;;;;;; Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.macro vram hi, lo
	lda hi
	sta $2006
	lda lo
	sta $2006
.endmacro

.macro strobe
	lda #$01
	sta $4016
	lda #$00
	sta $4016
.endmacro

;;;;;;;;;;;;;; Main Program ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
main:
	; Palette Cycle Timer
	palette_timer = $0300
	palette_delay = $0b
	lda #$00
	sta palette_timer

	; Paddle's Palette State
	paddle_state = $0301
	lda #$00
	sta paddle_state

	; Game state
	game_state = $0302
	
init:
	; Load the default palette
	jsr load_palette

	; Set the game state to the title screen
	lda #0
	sta $00
	jsr change_state

	; Reset VRAM address
	vram #0, #0

forever:
	jmp forever


;;;;;;;;;;;;;; Game Loop (NMI) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
game_loop:

	lda game_state
	bne @play

@title:	jsr title_loop
	jmp draw_sprites

@play:	jsr play_loop


draw_sprites:
	lda #$00
	sta $2003
	lda #$02
	sta $4014

cleanup:
	vram #0, #0
	rti

;;;;;;;;;;;;;; Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Game loop code for the title screen
;
title_loop:
	; Strobe the controller
	strobe

	; Proceed to the start button
	lda $4016
	lda $4016
	lda $4016

	; Read the start button
	lda #$01
	and $4016
	beq @done

	; Change to the play state if they pressed start
	lda #1
	sta $00
	jsr change_state

@done:	rts
	
;
; Game loop code for the main game
;
play_loop:
	; Strobe the controller
	strobe

	; Move up to the left button
	ldx #$06
@loop:	lda $4016
	dex
	bne @loop

check_left:
	lda #$01
	and $4016
	beq check_right

	lda $0203
	beq check_palette_timer

	ldx #$02
@loop:	dec $0203
	dec $0207
	dec $020b
	dec $020f
	dex
	bne @loop

	jmp check_palette_timer

check_right:
	lda #$01
	and $4016
	beq check_palette_timer

	lda $020f
	cmp #$f6
	beq check_palette_timer

	ldx #$02
@loop:	inc $0203
	inc $0207
	inc $020b
	inc $020f
	dex
	bne @loop

check_palette_timer:
	inc palette_timer
	ldx palette_timer
	cpx #palette_delay
	beq @cycle_palette
	jmp @done
	
@cycle_palette:
	ldx #$00
	stx palette_timer

	inc paddle_state
	lda paddle_state
	and #$07
	sta paddle_state
	tax
	vram #$3f, #$12
	lda paddle_cycle, x
	sta $2007

@done:
	rts


;
; Sets the game state
;
; Params:
;	$00 - The state to set
;
change_state:
	; Disable NMI, sprites, and background
	lda #$00
	sta $2000
	sta $2001

	; Store the new game state
	lda $00
	sta game_state

@title: bne @play

	; Load the title screen
	jsr clear_sprites
	jsr title_screen

	; Enable NMI
	lda #%10000000
	sta $2000

	; Enable background
	lda #%00001000
	sta $2001

	jmp @done

@play:
	; Load sprites for main game play
	jsr clear_sprites
	jsr load_sprites

	; Enable NMI
	lda #%10000000
	sta $2000
	
	; Disable Background
	lda #%00010110
	sta $2001

@done:	rts



;
; Clears sprite memory
;
clear_sprites:
	lda #$fe
	ldx #$00
@clear:	sta $0200, x
	inx
	bne @clear
	rts

;
; Loads sprites into sprite memory
;
load_sprites:
	; Draw the paddle
	ldx #$00
@loop:	lda paddle, x
	sta $0200, x
	inx
	cpx #$10
	bne @loop
	rts


;
; loads the game's master palette
; 
load_palette:
	ldx #$00
	vram #$3f, #$00
@loop:	lda palette, x
	sta $2007
	inx
	cpx #$20
	bne @loop
	rts

;
; Draws the game's main title screen to VRAM
;
title_screen:
	ldx #$40
	vram #$23, #$c0
	lda #0
@loop:	sta $2007
	dex
	bne @loop

draw_logo:
	lo = $20
	hi = $21

	lda #$21
	sta hi
	lda #$88
	sta lo

	ldy #$00
@row:	vram hi, lo
	ldx #$10
@col:	sty $2007
	iny
	dex
	bne @col
	
	clc
	lda lo
	adc #$20
	sta lo
	lda #$00
	adc hi
	sta hi

	cpy #$40
	bne @row
	rts


;;;;;;;;;;;;;; Palettes, Nametables, etc. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

palette:
	; Background
	.byte $0f, $03, $19, $00
	.byte $0f, $00, $00, $00
	.byte $0f, $00, $00, $00
	.byte $0f, $00, $00, $00

	; Sprites
	.byte $0f, $00, $08, $10
	.byte $0f, $00, $00, $00
	.byte $0f, $00, $00, $00
	.byte $0f, $00, $00, $00

paddle:
	.byte $d8, $40, %00000000, $10
	.byte $d8, $41, %00000000, $18
	.byte $d8, $41, %01000000, $20
	.byte $d8, $40, %01000000, $28

paddle_cycle:
	.byte $08, $18, $28, $38
	.byte $28, $18, $08, $0f


;;;;;;;;;;;;;; Pattern Table (CHR-ROM) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "CHARS"
.include "include/logo.s"	; $00 - $3f
.include "include/paddle.s"	; $40 - $41
.include "include/blocks.s"	; $42 - $49
.include "include/ball.s"	; $4A
;.include "include/wall.s"	; $4B - $53

;;;;;;;;;;;;;; Vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "VECTORS"
.word 0, 0, 0, game_loop, reset, 0
