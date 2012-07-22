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

	; Ball position
	ball_x = $0203
	ball_y = $0200

	; Ball direction
	ball_dx = $0303
	ball_dy = $0304

	; Whether or not the ball is moving
	ball_moving = $0305

	; Flag that determines if start was held last frame
	start_down = $0306

	; Whether or not the game is paused
	game_paused = $0307

	; Game states
	STATE_TITLE = 0
	STATE_NEW = 1
	STATE_PLAYING = 2
	STATE_PAUSED = 3
	STATE_GAMEOVER = 4
	
	; Load the default palette
	jsr load_palette

	; Set the game state to the title screen
	lda #STATE_TITLE
	sta $00
	jsr change_state

	; Reset VRAM address
	vram #0, #0

forever:
	jmp forever


;;;;;;;;;;;;;; Game Loop (NMI) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
game_loop:
	lda game_state
	
@title:	bne @play
	jsr title_loop
	jmp cleanup

@play:	cmp #STATE_PLAYING
	bne @pause
	jsr play_loop
	jmp cleanup

@pause:	cmp #STATE_PAUSED
	bne @over
	jsr pause_loop
	jmp cleanup

@over:  ; TODO Implement me

cleanup:
	lda #$00 	; Draw sprites
	sta $2003
	lda #$02
	sta $4014
	vram #0, #0 	; Clear VRAM Address
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

	; Indicate that the start button is being pressed
	lda #1
	sta start_down

	; Change to the new game state if they pressed start
	lda #STATE_NEW
	sta $00
	jsr change_state

@done:	rts
	
;
; Game loop code for the main game
;
play_loop:
	; Strobe the controller
	strobe


	; A - Gets the ball moving at the start of the game
check_a:
	lda #$01
	and $4016
	beq check_start

	lda ball_moving
	bne check_start

	lda #$01
	sta ball_moving


	; Start - Pauses the game
check_start:
	lda $4016 ; Skip B
	lda $4016 ; Skip Select
	
	lda start_down
	bne @ignore

	lda #$01
	and $4016
	beq check_left

	lda #1
	sta start_down

	lda #STATE_PAUSED
	sta $00
	jsr change_state
	rts

@ignore:
	lda #$01
	and $4016
	sta start_down

	
check_left:
	lda $4016 ; Skip Up
	lda $4016 ; Skip Down

	lda #$01
	and $4016
	beq check_right

	lda $0207
	beq check_palette_timer

	ldx #$02
	lda ball_moving
	beq @move_with_ball

@move:
	dec $0207
	dec $020b
	dec $020f
	dec $0213
	dex
	bne @move
	jmp @done_left

@move_with_ball:
	dec $0207
	dec $020b
	dec $020f
	dec $0213
	dec $0203
	dex
	bne @move_with_ball

@done_left:
	jmp check_palette_timer

check_right:
	lda #$01
	and $4016
	beq check_palette_timer

	lda $0213
	cmp #$f6
	beq check_palette_timer

	ldx #$02
	lda ball_moving
	beq @move_with_ball

@move:
	inc $0207
	inc $020b
	inc $020f
	inc $0213
	dex
	bne @move
	jmp @done_right

@move_with_ball:
	inc $0207
	inc $020b
	inc $020f
	inc $0213
	inc $0203
	dex
	bne @move_with_ball

@done_right:


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


bound_ball:


move_ball:
	lda ball_moving
	beq @done_y

	; Move the ball in the x-coordinate
	lda ball_dx
	bne @move_right
	dec $0203
	jmp @done_x
@move_right:
	inc $0203
@done_x:
	
	; Move the ball in the y-coordinate
	lda ball_dy
	bne @move_down
	dec $0200
	jmp @done_y
@move_down:
	inc $0200
@done_y:

	rts


;
; Game loop for the paused state
;
pause_loop:
	strobe
	lda $4016
	lda $4016
	lda $4016

	lda start_down
	bne @skip

	lda #$01
	and $4016
	beq @done

	sta start_down
	lda #STATE_PLAYING
	sta $00
	jsr change_state
	rts

@skip:	lda #$01
	and $4016
	sta start_down

@done:	rts





;
; Sets the game state
;
; Params:
;	$00 - The state to set
;
change_state:
	; Store the new game state
	lda $00
	sta game_state

@title: 
	cmp #STATE_TITLE
	bne @new_game

	; Disable NMI, sprites, and background
	lda #$00
	sta $2000
	sta $2001

	; Load the title screen
	jsr clear_sprites
	jsr title_screen

	; Enable NMI
	lda #%10000000
	sta $2000

	; Enable background
	lda #%00001000
	sta $2001

	jmp @return

@new_game:
	cmp #STATE_NEW
	bne @playing

	; Disable NMI, sprites, and background
	lda #$00
	sta $2000
	sta $2001

	; Load sprites for main game play
	jsr clear_sprites
	jsr load_sprites

	; Reset the ball dx, dy
	lda #$00
	sta ball_dx
	sta ball_dy

	; Reset ball moving and game paused
	sta ball_moving
	sta game_paused

	; Set the game state to "playing"
	lda #STATE_PLAYING
	sta game_state
	
	; Enable NMI
	lda #%10000000
	sta $2000
	
	; Disable Background
	lda #%00010110
	sta $2001

	jmp @return

@playing:
	cmp #STATE_PLAYING
	bne @paused

	; Swtich to color mode
	lda #%00010110
	sta $2001

	jmp @return

@paused:
	cmp #STATE_PAUSED
	bne @game_over

	; Switch to monochrome mode
	lda #%00010111
	sta $2001

	jmp @return

@game_over:
	; TODO Implement me


@return:
	rts



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
	; Load the paddle and ball
	ldx #$00
@loop:	lda sprites, x
	sta $0200, x
	inx
	cpx #$14
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


draw_board:
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
	.byte $0f, $06, $16, $27
	.byte $0f, $00, $00, $00
	.byte $0f, $00, $00, $00


sprites:
	; Ball (sprite 0)
	.byte $c0, $4a, %00000001, $7c

	; Paddle
	.byte $c8, $40, %00000000, $70
	.byte $c8, $41, %00000000, $78
	.byte $c8, $41, %01000000, $80
	.byte $c8, $40, %01000000, $88


paddle_cycle:
	.byte $08, $18, $28, $38
	.byte $28, $18, $08, $0f


;;;;;;;;;;;;;; Pattern Table (CHR-ROM) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "CHARS"
.include "include/logo.s"	; $00 - $3f
.include "include/paddle.s"	; $40 - $41
.include "include/blocks.s"	; $42 - $49
.include "include/ball.s"	; $4A
.include "include/wall.s"	; $4B - $53

;;;;;;;;;;;;;; Vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "VECTORS"
.word 0, 0, 0, game_loop, reset, 0
