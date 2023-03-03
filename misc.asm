;
; Title:	BBC Basic for AGON - Miscellaneous helper functions
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	26/02/2023
;
; Modinfo:
; 26/07/2022:	Added NULLTOCR and CRTONULL
; 28/09/2022:	Added CSTR_FNAME, BUF_DETOKEN, BUF_PBCDL
; 13/10/2022:	Added CSTR_LINE
; 11/01/2023:	Added CSTR_FINDCH. CSTR_ENDSWITH, CSTR_CAT
; 26/02/2023:	Removed BUF_DETOKEN and BUF_PBCDL

			INCLUDE	"equs.inc"
			INCLUDE	"macros.inc"

			.ASSUME	ADL = 0

			SEGMENT CODE
				
			XDEF	ASC_TO_NUMBER
			XDEF	SWITCH_A
			XDEF	NULLTOCR
			XDEF	CRTONULL
			XDEF	CSTR_FNAME
			XDEF	CSTR_LINE
			XDEF	CSTR_FINDCH
			XDEF	CSTR_ENDSWITH
			XDEF	CSTR_CAT
				
			XREF	OSWRCH
			XREF	KEYWDS
			XREF	KEYWDL

; Read a number and convert to binary
; If prefixed with &, will read as hex, otherwise decimal
;   Inputs: HL: Pointer in string buffer
;  Outputs: HL: Updated text pointer
;           DE: Value
;            A: Terminator (spaces skipped)
; Destroys: A,D,E,H,L,F
;
ASC_TO_NUMBER:		PUSH	BC			; Preserve BC
			LD	DE, 0			; Initialise DE
			CALL	SKIPSP			; Skip whitespace
			LD	A, (HL)			; Read first character
			CP	'&'			; Is it prefixed with '&' (HEX number)?
			JR	NZ, ASC_TO_NUMBER3	; Jump to decimal parser if not
			INC	HL			; Otherwise fall through to ASC_TO_HEX
;
ASC_TO_NUMBER1:		LD	A, (HL)			; Fetch the character
			CALL    UPPRC			; Convert to uppercase  
			SUB	'0'			; Normalise to 0
			JR 	C, ASC_TO_NUMBER4	; Return if < ASCII '0'
			CP 	10			; Check if >= 10
			JR 	C,ASC_TO_NUMBER2	; No, so skip next bit
			SUB 	7			; Adjust ASCII A-F to nibble
			CP 	16			; Check for > F
			JR 	NC, ASC_TO_NUMBER4	; Return if out of range
ASC_TO_NUMBER2:		EX 	DE, HL 			; Shift DE left 4 times
			ADD	HL, HL	
			ADD	HL, HL	
			ADD	HL, HL	
			ADD	HL, HL	
			EX	DE, HL	
			OR      E			; OR the new digit in to the least significant nibble
			LD      E, A
			INC     HL			; Onto the next character
			JR      ASC_TO_NUMBER1		; And loop
;
ASC_TO_NUMBER3:		LD	A, (HL)
			SUB	'0'			; Normalise to 0
			JR	C, ASC_TO_NUMBER4	; Return if < ASCII '0'
			CP	10			; Check if >= 10
			JR	NC, ASC_TO_NUMBER4	; Return if >= 10
			EX 	DE, HL 			; Stick DE in HL
			LD	B, H 			; And copy HL into BC
			LD	C, L	
			ADD	HL, HL 			; x 2 
			ADD	HL, HL 			; x 4
			ADD	HL, BC 			; x 5
			ADD	HL, HL 			; x 10
			EX	DE, HL
			ADD8U_DE 			; Add A to DE (macro)
			INC	HL
			JR	ASC_TO_NUMBER3
ASC_TO_NUMBER4:		POP	BC 			; Fall through to SKIPSP here

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
			AND     5FH			; Convert to upper case
			RET			

; Switch on A - lookup table immediately after call
;  A: Index into lookup table
;
SWITCH_A:		EX	(SP), HL		; Swap HL with the contents of the top of the stack
			ADD	A, A			; Multiply A by two
			ADD8U_HL 			; Add to HL (macro)
			LD	A, (HL)			; follow the call. Fetch an address from the
			INC	HL 			; table.
			LD	H, (HL)
			LD	L, A
			EX	(SP), HL		; Swap this new address back, restores HL
			RET				; Return program control to this new address

; Convert the buffer to a null terminated string and back
; HL: Buffer address
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
			
; Copy a filename to DE and zero terminate it
; HL: Source
; DE: Destination (ACCS)
;
CSTR_FNAME:		LD	A, (HL)			; Get source
			CP	32			; Is it space
			JR	Z, $F	
			CP	CR			; Or is it CR
			JR	Z, $F
			LD	(DE), A			; No, so store
			INC	HL			; Increment
			INC	DE			
			JR	CSTR_FNAME		; And loop
$$:			XOR	A			; Zero terminate the target string
			LD	(DE), A
			INC	DE			; And point to next free address
			RET
			
; Copy a CR terminated line to DE and zero terminate it
; HL: Source
; DE: Destination (ACCS)
;
CSTR_LINE:		LD	A, (HL)			; Get source
			CP	CR			; Is it CR
			JR	Z, $F
			LD	(DE), A			; No, so store
			INC	HL			; Increment
			INC	DE			
			JR	CSTR_LINE		; And loop
$$:			XOR	A			; Zero terminate the target string
			LD	(DE), A
			INC	DE			; And point to next free address
			RET
			
; Find the first occurrence of a character (case sensitive)
; HL: Source
;  C: Character to find
; Returns:
; HL: Pointer to character, or end of string marker
;
CSTR_FINDCH:		LD	A, (HL)			; Get source
			CP	C			; Is it our character?
			RET	Z			; Yes, so exit
			OR	A			; Is it the end of string?
			RET	Z			; Yes, so exit
			INC	HL
			JR	CSTR_FINDCH
			
; Check whether a string ends with another string (case insensitive)
; HL: Source
; DE: The substring we want to test with
; Returns:
;  F: Z if HL ends with DE, otherwise NZ
;
CSTR_ENDSWITH:		LD	A, (HL)			; Get the source string byte
			CALL	UPPRC			; Convert to upper case
			LD	C, A
			LD	A, (DE)			; Get the substring byte
			CP	C
			RET	NZ			; Return NZ if at any point the strings don't match
			OR	C			; Check whether both bytes are zero
			RET	Z			; If so, return, as we have reached the end of both strings
			INC	HL
			INC	DE
			JR	CSTR_ENDSWITH		; And loop
			
; Concatenate a string onto the end of another string
; HL: Source
; DE: Second string
;
CSTR_CAT:		LD	A, (HL)			; Loop until we find the end of the first string
			OR	A
			JR	Z, CSTR_CAT_1
			INC	HL
			JR	CSTR_CAT
;
CSTR_CAT_1:		LD	A, (DE)			; Copy the second string onto the end of the first string
			LD	(HL), A
			OR	A			; Check for end of string
			RET	Z			; And return
			INC	HL
			INC	DE
			JR	CSTR_CAT_1		; Loop until finished						