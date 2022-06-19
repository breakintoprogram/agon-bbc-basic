;
; Title:	BBC Basic for AGON
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	03/05/2022
;
; Modinfo:
			
			.ASSUME	ADL = 0
				
			INCLUDE	"equs.inc"
			INCLUDE "macros.inc"

			SEGMENT CODE
				
			XDEF	OSWRCH
			XDEF	OSLINE
			XDEF	ESCSET
			XDEF 	CLRSCN
			XDEF	MODE
			XDEF	PUTIME
			XDEF	GETIME
			XDEF	PUTCSR
			XDEF 	GETCSR
			XDEF	OSRDCH
			XDEF	PROMPT
			XDEF	OSKEY
			XDEF	TRAP
			XDEF	LTRAP
			XDEF	OSINIT
			XDEF	OSCLI
			XDEF	GETSCHR
			
			XDEF	GCOL
			XDEF	MOVE
			XDEF	PLOT
			XDEF	POINT
			XDEF	DRAW
			XDEF	SOUND
			XDEF	OSBPUT
			XDEF	OSBGET
			XDEF	OSSTAT
			XDEF	OSSHUT
			XDEF	OSOPEN
			XDEF	OSCALL
			XDEF	GETPTR
			XDEF	PUTPTR
			XDEF	GETEXT
			XDEF	RESET
			XDEF	OSLOAD
			XDEF	OSSAVE

			XREF	Edit_Line
			XREF	ASC_TO_NUMBER
			XREF	TX
			XREF	RX
			XREF	RAM_START
			XREF	RAM_END
			XREF	TIME
			XREF	KEY_CODE
			XREF	FLAGS
			XREF	ESCAPE
			XREF	USER
			XREF	RAM_Top
			XREF	EXTERR
			XREF	COUNT0
			XREF	EXPRI
			XREF	COMMA
			XREF	XEQ
			XREF	CHARPOS_X
			XREF	CHARPOS_Y

; OSWRCH: Write a character out to the ESP32 VDU handler
; A: Character to write
;
OSWRCH:			JP	TX

; OSRDCH: Read a character in from the ESP32 keyboard handler
;
OSRDCH:			LD	A, (KEY_CODE)	; Read keyboard
			OR	A 		
			JR	Z, OSRDCH	; Loop until key is pressed
			PUSH	AF
$$:			LD	A, (KEY_CODE)	; And same again
			OR	A 
			JR	NZ, $B 		; But loop until key is released
			POP 	AF  		; Return the keycode
			RET

; OSLINE: Invoke the line editor
;
OSLINE:			JP	Edit_Line 	

; CLRSCN: clears the screen.
;
CLRSCN:			LD	A, 0CH
			JP	OSWRCH
				
; MODE n: Set video mode
;
MODE:			CALL    EXPRI
			EXX
			VDU	16H
			VDU	L
			CALL	CLRSCN				
			JP	XEQ

; PUTIME: set current time to DE:HL, in centiseconds.
;
PUTIME:			LD	(TIME + 2), DE
			LD	(TIME + 0), HL
			RET

; GETIME: return current time in DE:HL, in centiseconds.
;
GETIME:			LD	DE, (TIME + 2)
			LD	HL, (TIME + 0)
			RET

; PUTCSR: move to cursor to x=DE, y=HL
;
PUTCSR:			LD	D, L				; E: X, D: Y
			LD	(CHARPOS_X), DE
			RET

; GETCSR: return cursor position in x=DE, y=HL
;
GETCSR:			LD	DE, (CHARPOS_X)			; E: X, D: Y
			LD	L, D 					
			LD	H, 0				; HL: Y
			LD	D, H 				; DE: X
			RET

; PROMPT: output the input prompt
;
PROMPT: 		LD	A,'>'
			JP	OSWRCH

;OSKEY - Read key with time-limit, test for ESCape.
;Main function is carried out in user patch.
;   Inputs: HL = time limit (centiseconds)
;  Outputs: Carry reset if time-out
;           If carry set A = character
; Destroys: A,H,L,F
;
OSKEY: 			LD	A, (KEY_CODE)	; Read keyboard
			OR	A		; If we have a character
			JR	NZ, $F		; Then process it
			LD	A,H		; Check if HL is 0 (this is passed by INKEY() function
			OR	L
			RET	Z 		; If it is then ret
			HALT			; Bit of a bodge so this is timed in ms
			DEC	HL 		; Decrement the counter and 
			JR	OSKEY 		; loop
$$:			CP	1BH		; If we are not pressing ESC, 
			SCF 			; then flag we've got a character
			RET	NZ		
;
ESCSET: 		PUSH    HL
        		LD      HL,FLAGS
        		BIT     6,(HL)          ; ESC DISABLED?
        		JR      NZ,ESCDIS
        		SET     7,(HL)     	; SET ESCAPE FLAG
ESCDIS: 		POP     HL
        		RET	
;
ESCTEST:		LD	A, (KEY_CODE)
			CP	1BH		; ESC	
			JR	Z,ESCSET
			RET
;
TRAP:			CALL	ESCTEST
LTRAP:			LD	A,(FLAGS)
			OR	A
			RET	P
			LD	HL,FLAGS 
			RES	7,(HL)
			JP	ESCAPE

;OSINIT - Initialise RAM mapping etc.
;If BASIC is entered by BBCBASIC FILENAME then file
;FILENAME.BBC is automatically CHAINed.
;   Outputs: DE = initial value of HIMEM (top of RAM)
;            HL = initial value of PAGE (user program)
;            Z-flag reset indicates AUTO-RUN.
;  Destroys: A,D,E,H,L,F
;
OSINIT:			XOR	A
			LD	(FLAGS), A		; Clear flags and set F = Z
			LD 	HL, USER
			LD	DE, RAM_Top
			LD	E, A			; Page boundary
			RET	

;
;OSCLI - Process an "operating system" command
;
OSCLI: 			CALL    SKIPSP
			CP      CR
			RET     Z
			CP      '|'
			RET     Z
			CP      '.'
			JP      Z,STAR_DOT		; *.
			EX      DE,HL
			LD      HL,COMDS
OSCLI0:			LD      A,(DE)
			CALL    UPPRC
			CP      (HL)
			JR      Z,OSCLI2
			JR      C,HUH
OSCLI1:			BIT     7,(HL)
			INC     HL
			JR      Z,OSCLI1
			INC     HL
			INC     HL
			JR      OSCLI0
;
OSCLI2:			PUSH    DE
OSCLI3:			INC     DE
			INC     HL
			LD      A,(DE)
			CALL    UPPRC
			CP      '.'			; ABBREVIATED?
			JR      Z,OSCLI4
			XOR     (HL)
			JR      Z,OSCLI3
			CP      80H
			JR      Z,OSCLI4
			POP     DE
			JR      OSCLI1
;
OSCLI4:			POP     AF
		        INC     DE
OSCLI5:			BIT     7,(HL)
			INC     HL
			JR      Z,OSCLI5
			LD      A,(HL)
			INC     HL
			LD      H,(HL)
			LD      L,A
			PUSH    HL
			EX      DE,HL
			JP      SKIPSP

HUH:    		LD      A,254
        		CALL    EXTERR
        		DB    	'Bad command'
        		DEFB    0			

SKIPSP:			LD      A,(HL)
        		CP      ' '
        		RET     NZ
        		INC     HL
        		JR      SKIPSP	

UPPRC:  		AND     7FH
			CP      '`'
			RET     C
			AND     5FH			; CONVERT TO UPPER CASE
			RET					

; Each command has bit 7 of the last character set, and is followed by the address of the handler
;
COMDS:  		DB	'CA','T'+80h	; CAT
			DW	STAR_CAT
			DB	'F','X'+80h		; FX
			DW	STAR_FX
			DB	0FFH	

; *CAT / *.
;
STAR_DOT:
STAR_CAT:		RET
	
; OSCLI FX n
;
STAR_FX:		CALL	ASC_TO_NUMBER	; C: FX #
			LD	C, E
			CALL	ASC_TO_NUMBER	; B: First parameter
			LD	B, E
			CALL	ASC_TO_NUMBER	; E: Second parameter
			LD	L, B 		; L: First parameter
			LD	H, E 		; H: Second parameter
			LD	A, C 		; A: FX #, and fall through to OSBYTE	
;
; OSBYTE
;  A: FX #
;  L: First parameter
;  H: Second parameter
;
OSBYTE:			CP	13H
			JR	Z, OSBYTE_13
			JP	HUH

; OSBYTE 0x13 (FX 19): Wait 1/50th of a second
;
OSBYTE_13:		HALT	
			LD	L, 0
			JP	COUNT0

;OSLOAD - Load an area of memory from a file.
;   Inputs: HL addresses filename (CR terminated)
;           DE = address at which to load
;           BC = maximum allowed size (bytes)
;  Outputs: Carry reset indicates no room for file.
; Destroys: A,B,C,D,E,H,L,F
;
OSLOAD:			RET

;OSSAVE - Save an area of memory to a file.
;   Inputs: HL addresses filename (term CR)
;           DE = start address of data to save
;           BC = length of data to save (bytes)
; Destroys: A,B,C,D,E,H,L,F
;
OSSAVE:			RET

; GCOL mode,R,G,B
;
GCOL:			CALL	EXPRI
			EXX
			LD	A, L
			LD	(VDU_BUFFER+0), A	; The mode
			CALL	COMMA

			CALL	EXPRI
			EXX
			LD	A, L
			LD	(VDU_BUFFER+1), A	; Red
			CALL	COMMA

			CALL	EXPRI
			EXX
			LD	A, L
			LD	(VDU_BUFFER+2), A	; Green
			CALL	COMMA

			CALL	EXPRI
			EXX
			LD	A, L
			LD	(VDU_BUFFER+3), A	; Blue				
			
			VDU	12H			; VDU code for GCOL
			VDU	(VDU_BUFFER+0)		; Mode
			VDU	(VDU_BUFFER+1)		; Red
			VDU	(VDU_BUFFER+2)		; Green
			VDU	(VDU_BUFFER+3)		; Blue
			
			JP	XEQ
				

	
; PLOT mode,x,y
;
PLOT:			CALL	EXPRI		; Parse mode
			EXX					
			PUSH	HL		; Push mode (L) onto stack
			CALL	COMMA 	
			CALL	EXPR_W2		; Parse X and Y
			POP	BC		; Pop mode (C) off stack
PLOT_1:			VDU	19H		; VDU code for PLOT				
			VDU	C		;  C: Mode
			VDU	E		; DE: X
			VDU	D
			VDU	L		; HL: Y
			VDU	H
			JP	XEQ

; MOVE x,y
;
MOVE:			CALL	EXPR_W2		; Parse X and Y
			LD	C, 04H		; Plot mode 04H (Move)
			JR	PLOT_1		; Plot
				

; Get two word values from EXPR in DE, HL
; IY: Pointer to expression string
; Returns:
; DE: P1
; HL: P2
;
EXPR_W2:		CALL	EXPRI			; Get first parameter	
			EXX
			PUSH	HL
			CALL	COMMA 
			CALL	EXPRI			; Get second parameter
			EXX
			POP	DE
			RET

; Stuff not implemented yet
;
GETSCHR:
POINT:
DRAW:
SOUND:
OSBPUT:
OSBGET:
OSSTAT:
OSSHUT:
OSOPEN:
OSCALL:
GETPTR:
PUTPTR:
GETEXT:
RESET:
			RET
