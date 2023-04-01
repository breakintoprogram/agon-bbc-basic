;
; Title:	BBC Basic for AGON - Interrupts
; Author:	Dean Belfield
; Created:	06/05/2022
; Last Updated:	30/03/2023
;
; Modinfo:
; 23/03/2023:		Added VBLANK_INIT and VBLANK_HANDLER
; 26/03/2023:		Fixed bug - MB changing in interrupt
; 28/03/2023:		Added VBLANK_STOP, removed MB bodge introduced in previous change
; 30/03/2023:		Tweaked DO_KEYBOARD

			.ASSUME	ADL = 0
				
			INCLUDE	"macros.inc"
			INCLUDE	"equs.inc"
			INCLUDE "mos_api.inc"	; In MOS/src

			SEGMENT CODE
				
			XDEF	VBLANK_INIT
			XDEF	VBLANK_STOP
			XDEF	VBLANK_HANDLER	

			XREF	ESCSET	
			XREF	KEYDOWN		; In ram.asm
			XREF	KEYASCII 	; In ram.asm
			XREF	KEYCOUNT	; In ram.asm

; Hook into the MOS VBLANK interrupt
;
VBLANK_INIT:		DI

			LD		A, MB 				; Get a 24-bit pointer to
			LD		HL, VBLANK_HANDLER		; this interrupt handler routine who's
			CALL		SET_AHL16 			; address is a 16-bit pointer in BBC BASIC's segment

			LD		E, 32h				; Set up the VBlank Interrupt Vector
			MOSCALL		mos_setintvector

			PUSH.LIL	HL				; HLU: Pointer to the MOS interrupt vector
			POP.LIL		DE 				; DEU: Pointer to the MOS interrupt vector
			
			LD		HL, VBLANK_HANDLER_JP + 1	; Pointer to the JP address in this segment
			LD		A, MB	 			; Get the segment BBC BASIC is running in
			LD		(VBLANK_HANDLER_MB + 1), A 	; Store in the interrupt handler
			CALL		SET_AHL16 			; Convert pointer to an absolute 24-bit address
			LD.LIL		(HL), DE			; Self-modify the code
			EI	
			RET

; Unhook the custom VBLANK interrupt
;
VBLANK_STOP:		DI
			LD		HL, VBLANK_HANDLER_JP + 1	; Pointer to the JP address in this segment
			LD		A, (VBLANK_HANDLER_MB + 1)	; The stored MB of the segment BBC BASIC is running in
			PUSH		AF 				; Stack the MB for later
			CALL		SET_AHL16			; Convert pointer to an absolute 24-bit address
			LD.LIL		DE, (HL)			; DEU: Address of MOS interrupt vector
			PUSH.LIL	DE				; Transfer to HL
			POP.LIL		HL
			LD		E, 32h
			MOSCALL		mos_setintvector		; Restore the MOS interrupt vector
			POP		AF 				; Restore MB to this segment
			LD		MB, A 
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
			LD		HL, KEYCOUNT 			; Check whether the keycount has changed
			LD.LIL		A, (IX + sysvar_vkeycount)	; by comparing the MOS copy
			CP 		(HL)				; with our local copy
			JR		NZ, DO_KEYBOARD_1		; Yes it has, so jump to the next bit
;
DO_KEYBOARD_0:		XOR		A 				; Clear the keyboard values 
			LD		(KEYASCII), A
			LD		(KEYDOWN), A 
			RET.LIL 					; And return
;
DO_KEYBOARD_1:		LD		(HL), A 			; Store the updated local copy of keycount 
			LD.LIL		A, (IX + sysvar_vkeydown)	; Fetch key down value (1 = key down, 0 = key up)
			OR		A 
			JR		Z, DO_KEYBOARD_0		; If it is key up, then clear the keyboard values
;			
			LD		(KEYDOWN), A 			; Store the keydown value
			LD.LIL		A, (IX + sysvar_keyascii)	; Fetch key ASCII value
			LD		(KEYASCII), A 			; Store locally
			CP		1Bh				; Is it escape?
			CALL		Z, ESCSET			; Yes, so set the escape flags
			RET.LIS						; Return to the interrupt handler

;
; Interrupts in mixed mode always run in ADL mode
;
			.ASSUME	ADL = 1

VBLANK_HANDLER:		DI 
			PUSH		AF 
			PUSH		HL
			PUSH		IX
			LD		A, MB
			PUSH		AF 
VBLANK_HANDLER_MB:	LD		A, 0				; This is self-modified by VBLANK_INIT
			LD		MB, A
			CALL.LIS	DO_KEYBOARD
			POP		AF
			LD		MB, A
			POP		IX 
			POP		HL
			POP		AF 
;
; Finally jump to the MOS interrupt
;
VBLANK_HANDLER_JP:	JP		0				; This is self-modified by VBLANK_INIT

			.ASSUME	ADL = 0
				