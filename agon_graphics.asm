;
; Title:	BBC Basic for AGON - Graphics stuff
; Author:	Dean Belfield
; Created:	07/08/2022
; Last Updated:	19/08/2022
;
; Modinfo:
; 19/08/2022:	Added GETSCHR, POINT

			
			.ASSUME	ADL = 0
				
			INCLUDE	"equs.inc"
			INCLUDE "macros.inc"
			INCLUDE "mos_api.inc"	; In MOS/src
		
			SEGMENT CODE
				
			XDEF	CLG
			XDEF	CLRSCN
			XDEF	MODE
			XDEF	COLOUR
			XDEF	GCOL
			XDEF	MOVE
			XDEF	PLOT
			XDEF	DRAW
			XDEF	POINT
			XDEF	GETSCHR
			
			XREF	OSWRCH
			XREF	ASC_TO_NUMBER
			XREF	EXTERR
			XREF	EXPRI
			XREF	COMMA
			XREF	XEQ
			XREF	NXT
			XREF	BRAKET
			XREF	COUNT0
			XREF	CRTONULL
			XREF	NULLTOCR
			XREF	CRLF
			XREF	EXPR_W2
			XREF	INKEY1
			
; CLG: clears the graphics area
;
CLG:			VDU	10h
			JP	XEQ

; CLS: clears the text area
;
CLRSCN:			LD	A, 0Ch
			JP	OSWRCH
				
; MODE n: Set video mode
;
MODE:			CALL    EXPRI
			EXX
			VDU	16H
			VDU	L
			CALL	CLRSCN				
			JP	XEQ
			
; GET(x,y): Get the ASCII code of a character on screen
;
GETSCHR:		INC	IY
			CALL    EXPRI      		; Get X coordinate
			EXX
			LD	(VDU_BUFFER+0), HL
			CALL	COMMA		
			CALL	EXPRI			; Get Y coordinate
			EXX 
			LD	(VDU_BUFFER+2), HL
			CALL	BRAKET			; Closing bracket		
;
			PUSH	IX			; Get the system vars in IX
			MOSCALL	mos_sysvars		; Reset the semaphore
			RES.LIL	1, (IX+sysvar_vpd_pflags)
			VDU	23			; Do VDU 23,0,3,x;y;
			VDU	0
			VDU	3
			VDU	(VDU_BUFFER+0)
			VDU	(VDU_BUFFER+1)
			VDU	(VDU_BUFFER+2)
			VDU	(VDU_BUFFER+3)
$$:			BIT.LIL	1, (IX+sysvar_vpd_pflags)
			JR	Z, $B			; Wait for the result
			LD.LIL	A, (IX+sysvar_scrchar)	; Fetch the result in A
			OR	A			; Check for 00h
			SCF				; C = character map
			JR	NZ, $F			; We have a character, so skip next bit
			XOR	A			; Clear carry
			DEC	A			; Set A to FFh
$$:			POP	IX			
			JP	INKEY1			; Jump back to the GET command

; POINT(x,y): Get the pixel colour of a point on screen
;
POINT:			CALL    EXPRI      		; Get X coordinate
			EXX
			LD	(VDU_BUFFER+0), HL
			CALL	COMMA		
			CALL	EXPRI			; Get Y coordinate
			EXX 
			LD	(VDU_BUFFER+2), HL
			CALL	BRAKET			; Closing bracket		
;
			PUSH	IX			; Get the system vars in IX
			MOSCALL	mos_sysvars		; Reset the semaphore
			RES.LIL	2, (IX+sysvar_vpd_pflags)
			VDU	23			; Do VDU 23,0,4,x;y;
			VDU	0
			VDU	4
			VDU	(VDU_BUFFER+0)
			VDU	(VDU_BUFFER+1)
			VDU	(VDU_BUFFER+2)
			VDU	(VDU_BUFFER+3)
$$:			BIT.LIL	2, (IX+sysvar_vpd_pflags)
			JR	Z, $B			; Wait for the result
;
; Return the data as a 3 byte number
; &RRGGBB
;
			LD.LIL	H, (IX+(sysvar_scrpixel+1))	; G
			LD.LIL	L, (IX+(sysvar_scrpixel+2))	; B
			EXX
			XOR     A
			LD      C, A				; Integer marker
			LD      H, 0				; 0
			LD.LIL  L, (IX+(sysvar_scrpixel+0))	; R
			POP	IX	
			RET


; COLOUR colour
; COLOUR mode,R,G,B
;
COLOUR:			CALL	EXPRI			; The colour / mode
			EXX
			LD	A, L 
			LD	(VDU_BUFFER+0), A	; Store first parameter; could be colour or mode at this point
			CALL	NXT			; Are there any more parameters?
			CP	','
			JR	Z, COLOUR_1		; Yes, so we're parsing R, G, B next
;
			LD	A, (VDU_BUFFER+0)	; The colour
			LD	C, A
			AND	7Fh
			LD	(VDU_BUFFER+1), A	; Store it in the colour byte
			LD	A, C			; Now process the mode
			AND 	80h
			RLCA
			LD	(VDU_BUFFER+0), A	; And store (0: FG, 1: BG)
;
			LD	HL, COLOUR_LOOKUP	; The lookup table
			LD	A, (VDU_BUFFER+1)	; The colour byte
			AND	7
			LD	C, A			; x 3
			ADD	A, A
			ADD	A, C
			ADD8U_HL			; Index into the lookup table
			VDU	11h			; VDU:COLOUR
			VDU	(VDU_BUFFER+0)		; Mode
			VDU	(HL)			; R
			INC	HL
			VDU	(HL)			; G
			INC 	HL
			VDU	(HL)			; B
			JP	XEQ
;
COLOUR_1:		CALL	COMMA
			CALL	EXPRI			; Parse R
			EXX
			LD	A, L
			LD	(VDU_BUFFER+1), A	
			CALL	COMMA
			CALL	EXPRI			; Parse G
			EXX
			LD	A, L
			LD	(VDU_BUFFER+2), A
			CALL	COMMA
			CALL	EXPRI			; Parse B
			EXX
			LD	A, L
			LD	(VDU_BUFFER+3), A							
			VDU	11h			; VDU:COLOUR
			VDU	(VDU_BUFFER+0)		; Mode
			VDU	(VDU_BUFFER+1)		; R
			VDU	(VDU_BUFFER+2)		; G
			VDU	(VDU_BUFFER+3)		; B
			JP	XEQ
							
; GCOL mode,colour
; GCOL mode,R,G,B
;
GCOL:			CALL	EXPRI			; Parse MODE
			EXX
			LD	A, L 
			LD	(VDU_BUFFER+0), A	
			CALL	COMMA
;
			CALL	EXPRI			; Parse R, or the Colour #
			EXX
			LD	A, L
			LD	(VDU_BUFFER+1), A
			CALL	NXT			; Check for any more parameters
			CP	','
			JR	Z, $F			; Yes, so fetch G and B
;			
			LD	HL, COLOUR_LOOKUP	; The lookup table
			LD	A, (VDU_BUFFER+1)	; The colour
			AND	7
			LD	C, A			; x 3
			ADD	A, A
			ADD	A, C
			ADD8U_HL			; Index into the lookup table
			VDU	12h			; VDU:GCOL
			VDU	(VDU_BUFFER+0)		; Mode
			VDU	(HL)			; R
			INC	HL
			VDU	(HL)			; G
			INC 	HL
			VDU	(HL)			; B
			JP	XEQ
;
; Read in the G and B values
;
$$:			CALL	COMMA
			CALL	EXPRI			; Parse G
			EXX
			LD	A, L
			LD	(VDU_BUFFER+2), A	
			CALL	COMMA
			CALL	EXPRI			; Parse B
			EXX
			LD	A, L
			LD	(VDU_BUFFER+3), A	
			VDU	12h			; GCOL
			VDU	(VDU_BUFFER+0)		; Mode
			VDU	(VDU_BUFFER+1)		; R
			VDU	(VDU_BUFFER+2)		; G
			VDU	(VDU_BUFFER+3)		; B
			JP	XEQ			
			
; BBC Micro Emulated Colours
;
COLOUR_LOOKUP:		DB	00h,00h,00h		; Black
			DB	FFh,00h,00h		; Red
			DB	00h,FFh,00h		; Green
			DB	FFh,FFh,00h		; Yellow
			DB	00h,00h,FFh		; Blue
			DB	FFh,00h,FFh		; Magenta
			DB	00h,FFh,FFh		; Cyan
			DB	FFh,FFh,FFh		; White			

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

; DRAW x1,y1
; DRAW x1,y1,x2,y2
;
DRAW:			CALL	EXPR_W2		; Get X1 and Y1
			CALL	NXT		; Are there any more parameters?
			CP	','
			LD	C, 05h		; Code for LINE
			JR	NZ, PLOT_1	; No, so just do DRAW x1,y1
			VDU	19h		; Move to the first coordinates
			VDU	04h
			VDU	E
			VDU	D
			VDU	L
			VDU	H
			CALL	COMMA
			PUSH	BC
			CALL	EXPR_W2		; Get X2 and Y2
			POP	BC
			JR	PLOT_1		; Now DRAW the line to those positions
			
			
			
