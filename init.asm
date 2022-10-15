;
; Title:	BBC Basic for AGON - Initialisation Code
;		Initialisation Code
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	15/10/2022
;
; Modinfo:
; 14/07/2022:	Modified to run in MOS
; 15/10/2022:	Added RST_08 and RST_10 handlers

			SEGMENT __VECTORS
		
			XREF	INIT_UART
;			XREF	RX
			XREF	INIT_IM2
			XREF	GPIOB_SETMODE
			XREF	TELL
			XREF	COLD
			XREF	ESCSET
			XREF	RAM_START
			XREF	RAM_END
			XREF	INV_START
;			XREF	KEY_CODE
;			XREF	TIME
		
			.ASSUME	ADL = 0
				
			INCLUDE	"equs.inc"
			INCLUDE "macros.inc"
			
;
; Start in mixed mode. Assumes MBASE is set to correct segment
;
			DI
			LD		SP, 0000h
			JP		_START		; Jump to start
			DS		1

RST_08:			RST.LIS	08h			; API call
			RET
			DS 	5
			
RST_10:			RST.LIS 10h			; Output
			RET
			DS	5
			
RST_18:			DS		8
RST_20:			DS		8
RST_28:			DS		8
RST_30:			DS		8	
;	
; The NMI interrupt vector (not currently used by AGON)
;
RST_38:			EI
			RETI
;
; This where the fun starts
;
_START:			EI				; Enable the MOS interrupts
			JP	COLD			; Start BBC Basic for Z80
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


