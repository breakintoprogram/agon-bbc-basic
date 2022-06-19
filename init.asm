;
; Title:	BBC Basic for AGON - Initialisation Code
;		Initialisation Code
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	03/05/2022
;
; Modinfo:

			SEGMENT __VECTORS
		
			XREF	INIT_UART
			XREF	RX
			XREF	INIT_IM2
			XREF	GPIOB_SETMODE
			XREF	TELL
			XREF	COLD
			XREF	ESCSET
			XREF	RAM_START
			XREF	RAM_END
			XREF	INV_START
			XREF	KEY_CODE
			XREF	TIME
		
			.ASSUME	ADL = 1	
				
			INCLUDE	"equs.inc"
			INCLUDE "macros.inc"
		
			DI
			STMIX				; Using mixed=mode (Z80 and eZ80) - interrupts will start in ADL mode
			JP		_START		; Jump to start
			NOP
				
RST_08:			DS		8
RST_10:			DS		8
RST_18:			DS		8
RST_20:			DS		8
RST_28:			DS		8
RST_30:			DS		8
	
; The NMI interrupt vector (not currently used by AGON)
;
RST_38:			EI
			RETI.L		
	
_START:			LD.SIS	SP, 000000h	; 16 bit SP
			LD	SP,080000h	; 24 bit SP
			CALL.IS	_INIT		; Switch to 16 bit mode at this point
_END:			JR	_END		; If we've got here, something's gone wrong

; VBLANK Interrupt 
; ADL Mode
;
_PB1_INT:		DI
			PUSH		AF
			SET_GPIO 	PB_DR, 2	; Need to set this to 1 for the interrupt to work correctly
			PUSH		BC
			PUSH		DE
			PUSH		HL
			CALL.IS		_VBLANK
			POP		HL
			POP		DE
			POP		BC
			POP		AF
			EI
			RETI.L 

			.ASSUME	ADL = 0			; Switch assembler to Z80

_INIT:			LD	HL,BAUD_250000		; ESP32 baud rate
			LD	A, 3			; 8N1
			CALL	INIT_UART		; Initialise the UART
			CALL	INIT_RAM		; Initialise the RAM
			LD	HL, INV_START		; Setup the vectored interrupts
			CALL	INIT_IM2
			LD	HL, _PB1_INT		; Initialise the VBLANK interrupt
			LD	(INV_START + PB1_IVECT), HL
			LD	A, GPIOMODE_INTRE
			LD	B, 2
			CALL	GPIOB_SETMODE
			EI			
			CALL    TELL			; Custom BOOT message
			DB		"AGON Single Board Computer\n\r\n\r"
			DB    	0
			CALL	COLD			; Start BBC Basic for Z80
			RET.L
				
; The VBLANK Interrupt for BBC Basic
; 16-bit mode
;
_VBLANK:		LD	DE, 2			; Increment the clock
			LD	HL, (TIME + 0)
			ADD	HL, DE
			LD	(TIME + 0), HL
			JR	NC, $F
			LD	HL, (TIME + 2)
			INC	HL
			LD	(TIME + 2), HL

$$:			CALL	RX			; Read the keyboard input
			LD	(KEY_CODE), A		; Store the keycode
			CP	1Bh			; Check for ESC
			CALL	Z, ESCSET
				
			RET.L				; We're returning to a 24-bit function
				
;
; Initialise the RAM
;
INIT_RAM:		LD	HL, RAM_START
			LD	BC, RAM_END-RAM_START
			LD	E, 0
$$:			LD	(HL), E
			INC	HL
			DEC	BC
			LD	A, B 
			OR	C 
			JR	NZ, $B
			RET				


