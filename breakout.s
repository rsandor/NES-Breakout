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
	pha
	lda hi
	sta $2006
	lda lo
	sta $2006
	pla
.endmacro

.macro vram_addr address
	pha
	lda #.HIBYTE(address)
	sta $2006
	lda #.LOBYTE(address)
	sta $2006
	pla
.endmacro

.macro strobe
	pha
	lda #$01
	sta $4016
	lda #$00
	sta $4016
	pla
.endmacro

.macro tile add_x, add_y
	lda ball_x
	adc add_x
	sta $00
	lda ball_y
	adc add_y
	sta $01
	jsr get_tile
.endmacro

.macro addr label
	pha
	lda #.LOBYTE(label)
	sta $00
	lda #.HIBYTE(label)
	sta $01
	pla
.endmacro

.macro addr2 l1, l2
	pha
	lda #.LOBYTE(l1)
	sta $00
	lda #.HIBYTE(l1)
	sta $01
	lda #.LOBYTE(l2)
	sta $02
	lda #.HIBYTE(l2)
	sta $03
	pla
.endmacro

.macro load_attrs label
.scope
	vram #$23, #$c0
	ldx #$00
@__load_attrs_loop:	
	lda label, x
	sta $2007
	inx
	cpx #$40
	bne @__load_attrs_loop
.endscope
.endmacro

.macro block_row hi, lo
.scope
	vram hi, lo
	ldx #$0e
@__block_row_loop:
	lda #$42
	sta $2007
	lda #$43
	sta $2007
	dex
	bne @__block_row_loop
.endscope
.endmacro


;;;;;;;;;;;;;; Global Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

palette_timer 	= $0300 ; Pallete cycle timer and delay
paddle_state 	= $0301 ; Paddle's palette state
game_state 	= $0302 ; Master game states

ball_dx 	= $0303 ; Ball's X direction
ball_dy 	= $0304 ; Ball's Y direction

ball_moving 	= $0305 ; Whether or not the ball is moving
start_down 	= $0306 ; Whether or not start was down last frame

lives 		= $0307 ; Player Lives
score 		= $0308 ; Score in BCD form (8-bytes)

; Ball position
ball_x = $0203
ball_y = $0200

; Paddle position
paddle_x = $0207

; Constants
PALETTE_DELAY 	= $0b
PADDLE_Y 	= $d8
NUMBER_OFFSET	= $64
SPACE_OFFSET	= $54

; Game States Enumeration
.enum GameState
	TITLE
	NEW
	PLAYING
	LOSE_LIFE
	PAUSED
	GAMEOVER
.endenum


;;;;;;;;;;;;;; Main Program ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
main:
	; Load the default palette
	jsr load_palette

	; Set the game state to the title screen
	lda #GameState::TITLE
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

@play:	cmp #GameState::PLAYING
	bne @pause
	jsr play_loop
	jmp cleanup

@pause:	cmp #GameState::PAUSED
	bne @over
	jsr pause_loop
	jmp cleanup

@over:  cmp #GameState::GAMEOVER
	bne cleanup
	jsr game_over_loop

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

	; Check to see if the start button is still down
	lda start_down
	bne @skip

	; Read the start button
	lda #$01
	and $4016
	sta start_down
	beq @done

	; Indicate that the start button is being pressed
	lda #1
	sta start_down

	; Change to the new game state if they pressed start
	lda #GameState::NEW
	sta $00
	jsr change_state
	jmp @done

@skip:	lda #$01
	and $4016
	sta start_down

@done:	rts


;
; Game loop code for the main game
;
play_loop:
	; Strobe the controller
	strobe


	; A - Gets the ball moving at the start of the game
button_a:
	lda #$01
	and $4016
	beq button_start

	lda ball_moving
	bne button_start

	lda #$01
	sta ball_moving


	; Start - Pauses the game
button_start:
	lda $4016 ; Skip B
	lda $4016 ; Skip Select
	
	lda start_down
	bne @ignore

	lda #$01
	and $4016
	beq button_left

	lda #1
	sta start_down

	lda #GameState::PAUSED
	sta $00
	jsr change_state
	rts

@ignore:
	lda #$01
	and $4016
	sta start_down

	
button_left:
	lda $4016 ; Skip Up
	lda $4016 ; Skip Down

	lda #$01
	and $4016
	beq button_right

	lda $0207
	cmp #$10
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

button_right:
	lda #$01
	and $4016
	beq check_palette_timer

	lda $0213
	cmp #$e6
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
	cpx #PALETTE_DELAY
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


check_hit:
	bit $2002
	bvs check_x
	jmp check_paddle

check_x:
	lda ball_dx
	bne check_right

check_left:
	; (x, y+4)
	tile #0, #4
	cmp #$ff
	beq check_y
	jsr block_hit
	lda #1
	sta ball_dx
	jmp check_y

check_right:
	; (x+7, y+3)
	tile #7, #3
	cmp #$ff
	beq check_y
	jsr block_hit
	lda #0
	sta ball_dx

check_y:
	lda ball_dy
	bne check_down

check_up:
	; (x+3, y)
	tile #3, #0
	cmp #$ff
	beq check_paddle
	jsr block_hit
	lda #1
	sta ball_dy
	jmp check_paddle

check_down:
	; (x+4, y+7)
	tile #4, #7
	cmp #$ff
	beq check_paddle
	jsr block_hit
	lda #0
	sta ball_dy

check_paddle:
	lda ball_y
	cmp #(PADDLE_Y - $08)
	bne check_lose

	; ball_x >= paddle_x
	clc
	lda ball_x
	adc #4
	cmp paddle_x
	bcc check_lose

	; paddle_x + 35 >= ball_x
	clc
	lda paddle_x
	adc #35
	cmp ball_x
	bcc check_lose

	; The paddle is in the right spot!
	lda #0
	sta ball_dy

check_lose:
	lda ball_y
	cmp #$f0
	bcc move_ball

	lda #GameState::LOSE_LIFE
	sta $00
	jsr change_state
	rts

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
	lda #GameState::PLAYING
	sta $00
	jsr change_state
	rts

@skip:	lda #$01
	and $4016
	sta start_down

@done:	rts


;
; Loop for the game over screen
;
game_over_loop:
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
	lda #GameState::TITLE
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
	cmp #GameState::TITLE
	bne @new_game

	; Disable NMI, sprites, and background
	lda #$00
	sta $2000
	sta $2001

	; Load the title screen
	jsr clear_sprites
	jsr draw_title

	; Wait for VBLANK
@wait:	bit $2002
	bpl @wait

	; Enable NMI
	lda #%10000000
	sta $2000

	; Enable background
	lda #%00001000
	sta $2001

	jmp @return

@new_game:
	cmp #GameState::NEW
	bne @lose_life

	; Disable NMI, sprites, and background
	lda #$00
	sta $2000
	sta $2001

	; Load sprites for main game play
	jsr clear_sprites
	jsr load_sprites
	
	; Reset the palette timer and paddle palette state
	lda #$00
	sta palette_timer
	sta paddle_state

	; Reset the ball dx, dy
	sta ball_dx
	sta ball_dy

	; Reset ball moving and game paused
	sta ball_moving

	; Reset lives to 3
	lda #3
	sta lives

	; Reset score to 0
	ldx #0
	lda #0
@score:	sta score, x
	inx
	cpx #8
	bne @score

	; Set the game state to "playing"
	lda #GameState::PLAYING
	sta game_state
	
	; Draw the game board
	jsr draw_board

	; Wait for VBLANK
@wait2:	bit $2002
	bpl @wait2

	; Enable NMI, sprites and background
	lda #%10000000
	sta $2000
	lda #%00011110
	sta $2001

	jmp @return

@lose_life:
	cmp #GameState::LOSE_LIFE
	bne @playing

	; Disable NMI
	lda #$00
	sta $2000

	; Decrement Lives
	dec lives
	ldx lives
	bne @game_continue
	
	; Lives == 0, game is now over
	lda #GameState::GAMEOVER
	sta game_state
	jmp @game_over

@game_continue:
	; Draw the update lives to the board
	jsr draw_lives

	; Reset ball and paddle position
	lda #$00
	sta ball_dx
	sta ball_dy
	sta ball_moving
	jsr load_sprites

	; Jump into the "playing state"
	lda #GameState::PLAYING
	sta game_state

	; Enable NMI
	lda #%10000000
	sta $2000

	jmp @return

@playing:
	cmp #GameState::PLAYING
	bne @paused

	; Swtich to color mode
	lda #%00011110
	sta $2001

	jmp @return

@paused:
	cmp #GameState::PAUSED
	bne @game_over

	; Switch to monochrome mode
	lda #%00011111
	sta $2001

	jmp @return

@game_over:
	; Disable NMI, sprites, and background
	lda #$00
	sta $2000
	sta $2001

	; Draw the game over screen
	jsr draw_game_over

	; Wait for vblank
@wait3:	bit $2002
	bpl @wait3

	; Enable the background and NMI
	lda #%10000000
	sta $2000
	lda #%00001000
	sta $2001

@return:
	rts


;;;;;;;;;;;;;; Drawing Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
; Clears sprite memory
;
clear_sprites:
	lda #$ff
	ldx #$00
@clear:	sta $0200, x
	inx
	bne @clear
	rts


;
; Clears nametable memory
;
clear_nametable:
	ldx #$00
	ldy #$04
	lda #$FF
	vram #$20, #$00
@loop:	sta $2007
	inx
	bne @loop
	dey
	bne @loop
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
	cpx #$18
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
draw_title:
	jsr clear_nametable

	; Load the attribute table
	load_attrs title_attr
	
.scope	; Draw the logo into the nametable
	col = 8
	row = 10
	address = $2000 + col + (row * $20)
	
	lo = $20
	hi = $21
	
	lda #.HIBYTE(address)
	sta hi
	lda #.LOBYTE(address)
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
.endscope

	
.scope	; print "Press Start"
	col = 10
	row = 17
	address = $2000 + col + (row * $20)
	vram #.HIBYTE(address), #.LOBYTE(address)
	addr press_start
	jsr prints
.endscope

	rts

;
; Draws the main game board to the nametable
;
draw_board:
	jsr clear_nametable

	; Load the attribute table
	load_attrs board_attr

.scope
	top = 3
	left = 1
	bottom = 27
	right = 30

	top_left = $2000 + left + ($20 * top)
	top_right = $2000 + right + ($20 * top)
	bottom_left = $2000 + left + ($20 * bottom)
	bottom_right = $2000 + right + ($20 * bottom)

	; Top left corner
	vram_addr top_left
	lda #$4b
	sta $2007

	; Top right corner
	vram_addr top_right
	lda #$4d
	sta $2007

	; Bottom left corner
	vram_addr bottom_left
	lda #$51
	sta $2007

	; Bottom right corner
	vram_addr bottom_right
	lda #$53
	sta $2007

	; Top Border
	vram_addr (top_left + 1)
	ldx #(right - left - 1)
	lda #$4c
@loop:	sta $2007
	dex
	bne @loop

	; Set write increments to 32
	lda #%00000100
	sta $2000
	
	; Left Border
	vram_addr (top_left + 32)
	lda #$4e
	ldx #(bottom-top-1)
@loop2:	sta $2007
	dex
	bne @loop2

	; Right Border
	vram_addr (top_right + 32)
	lda #$50
	ldx #(bottom-top-1)
@loop3:	sta $2007
	dex
	bne @loop3

	; Set write increments back to 1
	lda #%00000000
	sta $2000
.endscope


	; Draw score and lives
	jsr draw_score
	jsr draw_lives


	; Setup the blocks
	;block_row #$20, #$82
	;block_row #$20, #$a2
	block_row #$20, #$c2
	block_row #$20, #$e2
	block_row #$21, #$02
	block_row #$21, #$22
	block_row #$21, #$42
	block_row #$21, #$62


	rts


;
; Draws lives remaining text
;
draw_lives:
.scope
	vram_addr ($2000 + 3 + (1 * $20))
	lda lives
	clc
	adc #NUMBER_OFFSET
	sta $2007
.endscope
	rts


;
; Draws score text
;
draw_score:
.scope
	vram_addr ($2000 + 16 + (1 * $20))
	addr score_text
	jsr prints
	addr score
	jsr print_bcd
.endscope
	; TODO Implement me
	rts


;
; Draws the black "game over" doom screen :D
;
draw_game_over:
	jsr clear_nametable
	load_attrs game_over_attr
.scope
	text_addr = $2000 + 11 + (13 * $20)
	vram #.HIBYTE(text_addr), #.LOBYTE(text_addr)
	addr game_over_text
	jsr prints
.endscope
	rts


;
; Prints a null terminated string into VRAM. Strings are
; limited to 256 characters in length. 
;
; Note: Caller is responsible for setting the appropriate 
;       VRAM address.
;
; Params:
;	$00 - Low byte of the memory address of the string.
;	$01 - High byte of the memory address of the string.
;
prints:
	ldy #$00
@loop:	lda ($00), y
	beq @break
	clc
	adc #$34
	sta $2007
	iny
	bne @loop
@break:	rts

;
; Prints an 8-byte BCD number into VRAM
;
; Params:
;	$00 - Low byte of the memory address of the bcd
;	$01 - High byte of the memory address of the bcd
;
print_bcd:
	ldy #8

	; Skip leading zeros
@loop1:	dey
	lda ($00), y
	bne @break
	lda #$ff
	sta $2007
	cpy #0
	bne @loop1
@break: 

	; Print the remaining digits
	clc
@loop:	lda ($00), y
	adc #NUMBER_OFFSET
	sta $2007
	dey
	cpy #$ff
	bne @loop
	rts


;;;;;;;;;;;;;; Lookup & Math Subroutines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Performs a 16-bit arithmetic shift left.
; 
; Params:
; 	$00 - Low byte of the 16-bit value operand
; 	$01 - High byte of the 16-bit value operand
; 	$02 - Shift operand
;
; Return:
; 	$00 - The low byte of the result
;	$01 - The high byte of the result
asl16:
	ldx $02
@loop:	asl $01
	asl $00
	bcc @cont
	inc $01
@cont:	dex
	bne @loop
	rts


; Performs an add with two 16-bit operands storing
; the result in the first operand.
;
; Params:
; 	$00 - Low byte of the first operand
; 	$01 - High byte of the first operand
; 	$02 - Low byte of the second operand
; 	$03 - High byte of the second operand
;
; Return:
; 	$00 - The low byte of the result
;	$01 - The high byte of the result
add16:
	clc
	lda $02
	adc $00
	sta $00
	lda $03
	adc $01
	sta $01
	rts


;
; Adds two 8-byte BCD values and stores the result in the first.
;
; Params:
;	$00 - Low byte to address of first operand
;	$01 - High byte to address of the first operand
;	$02 - Low byte to address of second operand
;	$03 - High byte to the address of the second operand
;
bcd_add:
	clc
	ldy #0
@loop:	lda ($00), y
	adc ($02), y
	cmp #10
	bne @skip
	adc #5
	and #$0f
	sta ($00), y
	iny
	cpy #8
	sec
	bne @loop
@skip:	sta ($00), y
	iny
	cpy #8
	bne @loop
	rts


; Find the tile in the nametable at the point (x, y).
;
; Params:
; 	$00 - x-coordinate
;	$01 - y-coordinate
;
; Return:
; 	A   - The value of the tile at that address
;	$00 - The low byte of the address
; 	$01 - The high byte of the address
get_tile:
	; Nab the x value and hold onto it
	ldy $00 

	; Calculate the offset into VRAM
	; Tile(x, y) = ($00, $01) = (y / 8) * 32 + (x / 8)

	; (y / 8) * 32 = (y & #$f8) << 2
	lda $01
	and #$f8
	sta $00
	lda #0
	sta $01
	lda #2
	sta $02
	jsr asl16

	; (x / 8)
	tya
	lsr
	lsr
	lsr

	; [(y/8) * 32] + (x/8)
	sta $02
	lda #0
	sta $03
	jsr add16


	; Find that tile in VRAM
	lda $01
	adc #$20
	sta $2006
	sta $01

	lda $00
	sta $2006

	lda $2007
	lda $2007

	rts

;
; Determines if the given vram address represents a block
; and causes a game "collision" to occur if it is.
;
; If a collision did occur this also increments and redraws
; the score.
;
; Params:
;	$00 - Low byte of the vram address
;	$01 - High byte of the vram addres
;
block_hit:
.scope
	; For keeping tabs on whether or not we need to add
	; points to score on a successful hit or clear
	points_scored = $20
	lda #0
	sta points_scored

	; Check the tile to see if it's a block
	vram $01, $00
	lda $2007
	lda $2007

	; t >= $42
	clc
	cmp #$42
	bcs @check_high
	rts

@check_high:
	; t < $4A
	clc
	cmp #$4A
	bcc @check_side
	rts

@check_side:
	; Check to see if the tile is the left or right side
	; of the block
	tax
	and #$01
	bne @right

@left:
	txa
	clc
	adc #$02
	cmp #$44
	beq @clear_left

	vram $01, $00
	sta $2007
	tax
	inx
	stx $2007
	jmp @return

@clear_left:
	lda #$ff
	vram $01, $00
	sta $2007
	sta $2007

	; Assign the "break points"
	lda #(8 * 4)
	sta points_scored

	jmp @add_points

@right:
	txa
	clc
	adc #$01
	cmp #$44
	beq @clear_right

	dec $00
	vram $01, $00
	sta $2007
	tax
	inx
	stx $2007
	jmp @return


@clear_right:
	lda #$ff
	dec $00
	vram $01, $00
	sta $2007
	sta $2007

@add_points:
	addr2 score, hit4
	jsr bcd_add
	jsr draw_score

@return:
	rts
.endscope
	



;;;;;;;;;;;;;; Palettes, Nametables, etc. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

palette:
	; Background
	.byte $0f, $03, $19, $00
	.byte $0f, $00, $10, $20
	.byte $0f, $09, $19, $29
	.byte $0f, $20, $10, $00

	; Sprites
	.byte $0f, $00, $08, $10
	.byte $0f, $06, $16, $27
	.byte $0f, $00, $00, $00
	.byte $0f, $00, $00, $00


sprites:
	; Ball (sprite 0)
	.byte (PADDLE_Y - $08), $4a, %00000001, $7c

	; Paddle
	.byte PADDLE_Y, $40, %00000000, $70
	.byte PADDLE_Y, $41, %00000000, $78
	.byte PADDLE_Y, $41, %01000000, $80
	.byte PADDLE_Y, $40, %01000000, $88

	; Lives Row Ball
	.byte $07, $4a, %00000001, $0e


paddle_cycle:
	.byte $08, $18, $28, $38
	.byte $28, $18, $08, $0f


title_attr:
	.byte $00, $00, $00, $00, $00, $00, $00, $00
	.byte $00, $00, $00, $00, $00, $00, $00, $00
	.byte $00, $00, $00, $00, $00, $00, $00, $00
	.byte $00, $00, $00, $00, $00, $00, $00, $00
	.byte $00, $00, $55, $55, $55, $55, $00, $00
	.byte $00, $00, $00, $00, $00, $00, $00, $00
	.byte $00, $00, $00, $00, $00, $00, $00, $00
	.byte $00, $00, $00, $00, $00, $00, $00, $00

game_over_attr:
	.byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
	.byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
	.byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
	.byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
	.byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
	.byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
	.byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
	.byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff

board_attr:
	.byte $0f, $0f, $0f, $0f, $0f, $0f, $0f, $0f
	.byte $84, $a5, $a5, $a5, $a5, $a5, $a5, $21
	.byte $84, $a5, $a5, $a5, $a5, $a5, $a5, $21
	.byte $84, $a5, $a5, $a5, $a5, $a5, $a5, $21
	.byte $84, $a5, $a5, $a5, $a5, $a5, $a5, $21
	.byte $84, $a5, $a5, $a5, $a5, $a5, $a5, $21
	.byte $84, $a5, $a5, $a5, $a5, $a5, $a5, $21
	.byte $84, $a5, $a5, $a5, $a5, $a5, $a5, $21



;;;;;;;;;;;;;; Strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

press_start:	.asciiz "PRESS START"
score_text:	.asciiz "SCORE:"
game_over_text:	.asciiz "GAME OVER"


;;;;;;;;;;;;;; BCD Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

bcd_zero:	.byte 0,0,0,0,0,0,0,0
hit1:		.byte 0,5,0,0,0,0,0,0
hit2:		.byte 0,0,1,0,0,0,0,0
hit3:		.byte 0,5,2,0,0,0,0,0
hit4:		.byte 0,0,5,0,0,0,0,0

;;;;;;;;;;;;;; Pattern Table (CHR-ROM) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "CHARS"
.include "include/logo.s"	; $00 - $3f
.include "include/paddle.s"	; $40 - $41
.include "include/blocks.s"	; $42 - $49
.include "include/ball.s"	; $4a
.include "include/wall.s"	; $4b - $53
.include "include/font.s"	; $54 - $b9

;;;;;;;;;;;;;; Vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.segment "VECTORS"
.word 0, 0, 0, game_loop, reset, 0
