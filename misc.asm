;
; Title:	BBC Basic for AGON - Miscellaneous helper functions
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	13/10/2022
;
; Modinfo:
; 26/07/2022:	Added NULLTOCR and CRTONULL
; 28/09/2022:	Added CSTR_FNAME, BUF_DETOKEN, BUF_PBCDL
; 13/10/2022:	Added CSTR_LINE

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
			XDEF	BUF_DETOKEN
			XDEF	BUF_PBCDL
				
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
			INC	HL 				; table.
			LD	H, (HL)
			LD	L, A
			EX	(SP), HL		; Swap this new address back, restores HL
			RET					; Return program control to this new address

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
			
; Detokenise a character into a buffer
;  A: Character to detokenise
; IX: Output buffer
;
BUF_DETOKEN:		CP      138
			JP      PE,BUF_DETOKEN1
			PUSH    BC
			PUSH    HL
			LD      HL,KEYWDS
			LD      BC,KEYWDL
			CPIR
$$:			LD      A,(HL)
			INC     HL
			CP      138
			PUSH    AF
			CALL    PE,BUF_DETOKEN1
			POP     AF
			JP      PE,$B
			POP     HL
			POP     BC
			RET	
BUF_DETOKEN1:		LD	(IX),A
			INC	IX
			RET	

; Print a number into a buffer
; This is a modified version of PBCD in main.asm
; HL: Number (binary)
; IX: Buffer
;
BUF_PBCDL:		LD	BC,0500h		; C: Leading character (NUL), B: Number of digits in result
			LD      DE,10000		; Start off with the 10,000 column
BUF_PBCD1:		XOR     A			; Counter
BUF_PBCD2:		SBC     HL,DE			; Loop and count how many 10,000s we have
			INC     A
			JR      NC,BUF_PBCD2		
			ADD     HL,DE			; The loop overruns by one, so adjust here
			DEC     A			; A: Number of 10,000s
			JR      Z,BUF_PBCD3		; If it is 0, then skip the next bit
			LD	C, '0'			; C: Set to '0'
BUF_PBCD3:		OR      C			; A is then an ASCII character, or 00h if we've not processed any non-zero digits yet
			JR	Z, $F			; If it is a leading NUL character then ignore
			LD	(IX),A			; Store the character in the buffer
			INC	IX			; Increment the buffer pointer
$$:			LD	A, B			; If on first transition, skip this
			CP	5			; TODO: Need to find out why 
			JR	Z, BUF_PBCD4
			ADD     HL,HL			; HL x  2 : We shift the number being tested left,
			LD      D,H			;         : rrather than shifting DE right
			LD      E,L			;         : This makes a lot of sense
			ADD     HL,HL			; HL x  4
			ADD     HL,HL			; HL x  8
			ADD     HL,DE			; HL x 10
BUF_PBCD4:		LD	DE, 1000		; Set the column heading to 1,000s for subsequent runs
			DJNZ    BUF_PBCD1		; Loop until done
			RET