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

;;;;;;;;;;;;;; Main Program ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
main:
	counter = $0300
	lda #$00
	sta counter

	paddle_state = $0301
	lda #$00
	sta paddle_state


load_palette:
	ldx #$00
	vram #$3f, #$00
@loop:	lda palette, x
	sta $2007
	inx
	cpx #$20
	bne @loop

set_attributes:
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

enable_rendering:
	lda #%10000000	; Enable NMI
	sta $2000
	lda #%00011110	; Enable Sprites & Background
	sta $2001
	lda #$00 	; Reset the VRAM address
	sta $2006
	sta $2006

clear_sprites:
	lda #$fe
	ldx #$00
@clear:	sta $0200, x
	inx
	bne @clear

load_sprites:
	; Draw the paddle
	ldx #$00
@loop:	lda paddle, x
	sta $0200, x
	inx
	cpx #$10
	bne @loop



forever:
	jmp forever


;;;;;;;;;;;;;; Game Loop (NMI) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
game_loop:

draw_sprites:
	lda #$00
	sta $2003
	lda #$02
	sta $4014

check_counter:
	inc counter
	ldx counter
	cpx #$08
	beq cycle_palette
	jmp cleanup
	
cycle_palette:
	ldx #$00
	stx counter

	inc paddle_state
	lda paddle_state
	and #$03
	sta paddle_state
	tax
	vram #$3f, #$12
	lda paddle_cycle, x
	sta $2007

	vram #$3f, #$01
	lda bg_cycle, x
	sta $2007





cleanup:
	vram #$00, #$00
	rti

;;;;;;;;;;;;;; Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
	.byte $08, $40, %00000000, $10
	.byte $08, $41, %00000000, $18
	.byte $08, $41, %01000000, $20
	.byte $08, $40, %01000000, $28

paddle_cycle:
	.byte $08, $18, $28, $38


bg_cycle:
	.byte $03, $13, $23, $33


;;;;;;;;;;;;;; Pattern Table (CHR-ROM) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "CHARS"
.include "include/logo.s"
.include "include/paddle.s"

;;;;;;;;;;;;;; Vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "VECTORS"
.word 0, 0, 0, game_loop, reset, 0
