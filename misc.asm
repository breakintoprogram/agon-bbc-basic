;
; Title:	BBC Basic for AGON - Miscellaneous helper functions
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	26/07/2022
;
; Modinfo:
; 26/07/2022:	Added NULLTOCR and CRTONULL

			INCLUDE	"equs.inc"
			INCLUDE	"macros.inc"

			.ASSUME	ADL = 0

			SEGMENT CODE
				
			XDEF	ASC_TO_NUMBER
			XDEF	DEC_D_NZ
			XDEF	DEC_E_NZ
			XDEF	SWITCH_A
			XDEF	NULLTOCR
			XDEF	CRTONULL
				
			XREF	OSWRCH

; Read a number and convert to binary
; If prefixed with &, will read as hex, otherwise decimal
;   Inputs: HL: Pointer in string buffer
;  Outputs: HL: Updated text pointer
;           DE: Value
;            A: Terminator (spaces skipped)
; Destroys: A,D,E,H,L,F
;
ASC_TO_NUMBER:		PUSH	BC		; Preserve BC
			LD	DE, 0		; Initialise DE
			CALL	SKIPSP		; Skip whitespace
			LD	A, (HL)		; Read first character
			CP	'&'		; Is it prefixed with '&' (HEX number)?
			JR	NZ, ASC_TO_NUMBER3		; Jump to decimal parser if not
			INC	HL		; Otherwise fall through to ASC_TO_HEX
;
ASC_TO_NUMBER1:		LD	A, (HL)		; Fetch the character
			CALL    UPPRC		; Convert to uppercase  
			SUB	'0'		; Normalise to 0
			JR 	C, ASC_TO_NUMBER4		; Return if < ASCII '0'
			CP 	10		; Check if >= 10
			JR 	C,ASC_TO_NUMBER2		; No, so skip next bit
			SUB 	7		; Adjust ASCII A-F to nibble
			CP 	16		; Check for > F
			JR 	NC, ASC_TO_NUMBER4		; Return if out of range
ASC_TO_NUMBER2:		EX 	DE, HL 		; Shift DE left 4 times
			ADD	HL, HL	
			ADD	HL, HL	
			ADD	HL, HL	
			ADD	HL, HL	
			EX	DE, HL	
			OR      E		; OR the new digit in to the least significant nibble
			LD      E, A
			INC     HL		; Onto the next character
			JR      ASC_TO_NUMBER1		; And loop
;
ASC_TO_NUMBER3:		LD	A, (HL)
			SUB	'0'		; Normalise to 0
			JR	C, ASC_TO_NUMBER4		; Return if < ASCII '0'
			CP	10		; Check if >= 10
			JR	NC, ASC_TO_NUMBER4		; Return if >= 10
			EX 	DE, HL 		; Stick DE in HL
			LD	B, H 		; And copy HL into BC
			LD	C, L
			ADD	HL, HL 		; x 2 
			ADD	HL, HL 		; x 4
			ADD	HL, BC 		; x 5
			ADD	HL, HL 		; x 10
			EX	DE, HL
			ADD8U_DE 		; Add A to DE (macro)
			INC	HL
			JR	ASC_TO_NUMBER3
ASC_TO_NUMBER4:		POP	BC 		; Fall through to SKIPSP here

; Skip a space
; HL: Pointer in string buffer
; 
SKIPSP:			LD      A, (HL)
			CP      ' '
			RET     NZ
			INC     HL
			JR      SKIPSP

; Skip a string
; HL: Pointer in string buffer
;
SKIPNOTSP:		LD	A, (HL)
			CP	' '
			RET	Z 
			INC	HL 
			JR	SKIPNOTSP

; Convert a character to upper case
;  A: Character to convert
;
UPPRC:  		AND     7FH
			CP      '`'
			RET     C
			AND     5FH		; CONVERT TO UPPER CASE
			RET

; Convert the buffer to a null terminated string and back
;	


; Convert BCD to ASCII
; HL: Pointer in string buffer
;  A: BCD number to convert
;
BCD_TO_ASC:		LD	C, A 		; Store A 
			RRCA			; Get high nibble
			RRCA 
			RRCA
			RRCA
			CALL	$F
			LD	A, C 
$$:			AND	0FH 
			ADD 	A, '0'
			LD	(HL), A 
			INC	HL 
			RET 

; Print BCD
;
PRINT_BCD:		ADD	A, 0
			DAA 
PRINT_BCD_1:		LD	C, A 
			RRCA 
			RRCA 
			RRCA 
			RRCA  
			CALL	$F
			LD	A, C 
$$:			AND	0FH 
			ADD	A, '0'
			JP	OSWRCH

; Switch on A - lookup table immediately after call
;  A: Index into lookup table
;
SWITCH_A:		EX	(SP), HL		; Swap HL with the contents of the top of the stack
			ADD	A, A			; Multiply A by two
			ADD8U_HL 			; Add to HL (macro)
			LD	A, (HL)			; follow the call. Fetch an address from the
			INC	HL 				; table.
			LD	H, (HL)
			LD	L, A
			EX	(SP), HL		; Swap this new address back, restores HL
			RET					; Return program control to this new address

; Decrease if not 0
;
DEC_D_NZ:		INC	D 
			DEC	D 
			RET	Z 
			DEC  	D 
			RET 
;
DEC_E_NZ:		INC	E 
			DEC	E 
			RET	Z 
			DEC  	E 
			RET 

; Convert LHED to a FPP
;
HLDE_TO_FPP:		PUSH	DE			
			EXX
			POP	DE 
			EX	DE, HL			; E, D are the least significant bytes
			EXX 
			LD	C, 0			; Exponent
			RET 

; Convert the buffer to a null terminated string and back
;			
NULLTOCR:		PUSH 	BC
			LD	B, 0
			LD	C, CR 
			JR	CRTONULL0
;			
CRTONULL:		PUSH	BC
			LD	B, CR
			LD	C, 0	
;			
CRTONULL0:		PUSH	HL
CRTONULL1:		LD	A, (HL)
			CP 	B 
			JR	Z, CRTONULL2
			INC	HL 
			JR	CRTONULL1
CRTONULL2:		LD	(HL), C
			POP 	HL 
			POP	BC
			RET