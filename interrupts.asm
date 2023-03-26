;
; Title:	BBC Basic for AGON - Interrupts
; Author:	Dean Belfield
; Created:	06/05/2022
; Last Updated:	23/03/2023
;
; Modinfo:
; 23/03/2023:		Added VBLANK_INIT and VBLANK_HANDLER

			.ASSUME	ADL = 0
				
			INCLUDE	"macros.inc"
			INCLUDE	"equs.inc"
			INCLUDE "mos_api.inc"	; In MOS/src

			SEGMENT CODE
				
			XDEF	VBLANK_INIT
			XDEF	VBLANK_HANDLER	

			XREF	ESCSET	
			XREF	KEYDOWN		; In ram.asm
			XREF	KEYASCII 	; In ram.asm

; Hook into the MOS VBLANK interrupt
;
VBLANK_INIT:		DI
			LD		E, 32h				; Set up the VBLANK interrupt
			LD		HL, VBLANK_HANDLER	
			MOSCALL		mos_setintvector
			EX		DE, HL				; DEU: Address of previous interrupt handler
			LD		HL, VBLANK_HANDLER_JP + 1	; This is a 16-bit address within this segment
			LD		A, MB	 			; Get the current segment
			CALL		SET_AHL16 			; Convert to an absolute 24-bit address
			LD.LIL		(HL), DE			; Self-modify the code
			EI	
			RET

; Set the MSB of HL (U) to A
;
SET_AHL16:		PUSH.LIL	HL
			LD.LIL		HL, 2
			ADD.LIL		HL, SP
			LD.LIL		(HL), A
			POP.LIL		HL
			RET 

; A safe LIS call to ESCSET
; 
DO_KEYBOARD:		MOSCALL		mos_sysvars			; Get the system variables
			LD.LIL		A, (IX + sysvar_vkeydown)	; Fetch key down value (1 = key down, 0 = key up)
			LD		(KEYDOWN), A 			; Store locally
			OR		A 
			JR		Z, $F 				; If it is key up, then skip to next bit
			LD.LIL		A, (IX + sysvar_keyascii)	; Fetch key ASCII value
$$:			LD		(KEYASCII), A 			; Store locally
			CP		1Bh				; Is it escape?
			CALL		Z, ESCSET			; Yes, so set the escape flags
			RET.LIS						; Return to the interrupt handler

;
; Interrupts in mixed mode always run in ADL mode
;
			.ASSUME	ADL = 1

VBLANK_HANDLER:		DI 
			PUSH		AF 
			PUSH		IX
			CALL.LIS	DO_KEYBOARD
			POP		IX 
			POP		AF 
;
; Finally jump to the MOS interrupt
;
VBLANK_HANDLER_JP:	JP		0				; This is self-modified by VBLANK_INIT

			.ASSUME	ADL = 0
				