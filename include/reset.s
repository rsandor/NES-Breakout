;; 6502 (NES) General purpose machine reset
;; By Ryan Sandor Richards (@rsandor)

;; USAGE
;;   .segment "CODE"
;;   reset:
;;   .include "reset.s"
;;
;;   .segment "VECTORS"
;;   .word 0, 0, 0, nmi, reset, 0

	sei		; disable IRQs
	cld		; disable decimal mode
	ldx #$40
	stx $4017	; disable APU frame IRQ
	ldx #$ff 	; Set up stack
	txs		;  .
	inx		; now X = 0
	stx $2000	; disable NMI
	stx $2001 	; disable rendering
	stx $4010 	; disable DMC IRQs

@wait:	bit $2002	; Wait for V-Blank
	bpl @wait

@clear:	lda #$00 	; Clear RAM
	sta $0000, x
	sta $0100, x
	sta $0200, x
	sta $0300, x
	sta $0400, x
	sta $0500, x
	sta $0600, x
	sta $0700, x
	inx
	bne @clear

@wait2:	bit $2002 	; Wait for V-Blank
	bpl @wait2

