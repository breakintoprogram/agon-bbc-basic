;
; Title:	BBC Basic for AGON - Editor
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	03/05/2022
;
; Modinfo:

			.ASSUME	ADL = 0
				
			INCLUDE	"equs.inc"

			SEGMENT CODE
				
			XDEF	Edit_Line
			
			XREF	FLAGS
			XREF	KEY_CODE
			XREF	CHARPOS_X
			XREF	CHARPOS_Y
			XREF	CURSOR_X
			XREF	CURSOR_Y
			XREF	OSRDCH
			XREF	OSWRCH
			XREF	CRLF
			XREF	LTRAP
			XREF	DEC_D_NZ
			XREF	DEC_E_NZ

; Read/edit a complete line, terminated by CR.
;  HL: Addresses destination buffer. Must be on a page boundary
; Returns:
; Buffer filled, terminated by CR.
;   A: 0
; Destroys: A,BC,DE,HL,IX,F
;
Edit_Line:		LD	(HL), CR		; Initialise buffer
			LD	A, (FLAGS)		; If ESC flag is set, then do nothing
			OR	A 
			LD	A, 0
			RET	M
			LD	DE, (CHARPOS_X)		; Update the cursor position
;			CALL	CERBERUS_CURSOR.Move	
$$:			LD 	A, (KEY_CODE)		; Wait until we've let go of ESC
			CP	1BH	 
			JR	Z, $B
;
Edit_Line_Loop:
;			CALL	CERBERUS_CURSOR.Show
			CALL	OSRDCH			; Wait for character input
			LD	C, A 			; Take a copy of the character
;			CALL	CERBERUS_CURSOR.Hide	; Cursor off
			CALL	LTRAP			; Check for escape
;
			LD	A, (FLAGS)		; Get the flags in B
			LD	B, A
			LD	DE, (CURSOR_X)		; Cursor position
;
			LD	A, C			; Check keyboard edit commands
			CP	7FH			; DEL Delete
			JP	Z, Key_DEL
			CP	08H			; BS Back one character
			JR	Z, Key_BS
			CP 	15H			; HT Advance one character
			JP	Z, Key_HT
			CP	0AH			; LF Down one character
			JP	Z, Key_LF
			CP	0BH			; VT Up one character
			JP	Z, Key_VT
			CP	0DH			; CR Enter
			JR	Z, Key_CR
			CP	09H			; TAB Copy
			JP	Z, Key_TAB
;
			LD	A, C			; Is it a printable character? 
			CP	32
			JR	C, Edit_Line_Loop	; No, so skip
;			
			LD	E, 0			; Get length of current line
			CALL	Get_Length
			LD	A, B 
			CP	255			
			JR	NC, Edit_Line_Loop	; Skip if line limit (255) exceeded
;
			CALL	Insert			; Insert the character into the buffer
			LD	(HL), C			; Store the character
			CALL	Update_1		; Update characters from cursor position
			DEC	B
			CALL	NZ, Update_2
			INC	L			; Move the cursor
			LD	DE, (CHARPOS_X)		; Update the cursor position
;			CALL	CERBERUS_CURSOR.Move
			JR	Edit_Line_Loop

; Enter pressed
;
Key_CR:			LD	A, (HL)			; Move the cursor to the end of the line
			CP	CR
			JR	Z, $F 
			INC	L 
			LD	A, 09H
			CALL OSWRCH
			JR	Key_CR
$$:			LD	A, (FLAGS)		
			AND	11101111B		; Reset the copy bit
			LD	(FLAGS), A
			CALL	CRLF			; Display CRLF
			XOR	A			; Return A = 0
			RET  

; Cursor Left
;
Key_BS:			BIT	4, B			; Are we in COPY mode?
			JR	Z, $F		
			CALL 	Move_Cursor_Left
;			CALL	CERBERUS_CURSOR.Move
			JP	Edit_Line_Loop
$$:			INC	L 			
			DEC	L 			; Check for cursor at beginning of line
			JP	Z, Edit_Line_Loop	; If we are, then do nothing
			DEC	L			; Move the cursor back
			LD 	A, 8
;			
Key_Out:		CALL	OSWRCH			; Echo character back to terminal
			LD	DE, (CHARPOS_X)		; Update the cursor position
;			CALL	CERBERUS_CURSOR.Move
			JP	Edit_Line_Loop		; Loop

; Cursor Right
;
Key_HT:			BIT	4, B			; Are we in COPY mode?
			JR	Z, $F		
Key_HT_1:		CALL	Move_Cursor_Right
;			CALL	CERBERUS_CURSOR.Move
			JP	Edit_Line_Loop
$$:			LD	A, (HL)			
			CP	CR			; Are we at the end of line? (marked with a CR)
			JP	Z, Edit_Line_Loop	; Yes, so do nothing
			INC	L			; Advance the cursor
			LD	A, 9
			JR	Key_Out			; Echo character back to terminal			

; Cursor Down 
;
Key_LF:			BIT	4, B			; Are we in COPY mode?
			JR	Z, $F		
			CALL	Move_Cursor_Down	
;			CALL	CERBERUS_CURSOR.Move
			JP	Edit_Line_Loop
$$:			LD	E, 0			
			CALL	Get_Length 		; Get length of line in B from start of buffer (E=0)
			LD	A, CHAR_COLS			
			ADD	A, L			; Down one line
			CP	B 			; Check with line length
			JR 	C, $F			
			JP	NZ, Edit_Line_Loop
$$:			LD 	L, A 
			LD 	A, 10
			JR	Key_Out			; Echo character back to terminal

; Cursor Up
;
Key_VT:			BIT	4, B			; Are we in COPY mode?
			JR	Z, $F			
			CALL	Move_Cursor_Up		; Yes, so just move the cursor
;			CALL	CERBERUS_CURSOR.Move
			JP	Edit_Line_Loop
$$:			LD	A, -CHAR_COLS
			ADD	A, L 			; Up one line
			JP	NC, Edit_Line_Loop	; If it takes us past the beginning of the line then do nothing
			LD	L, A 			; Store
			LD	A, 11
			JR	Key_Out			; Echo character back to terminal

; Delete
;
Key_DEL:		INC	L			; Check for input ptr at beginning of line
			DEC	L 
			JR	Z, $F
			CALL	Delete
			DEC	L
			LD	A, 08H
			CALL OSWRCH
			CALL	Update_1
			LD	A, 20H
			CALL OSWRCH
			INC	B
			CALL	Update_2
			LD	DE, (CHARPOS_X)		; Update the cursor position
;			CALL	CERBERUS_CURSOR.Move
$$:			JP	Edit_Line_Loop

; Copy
;
Key_TAB:		BIT	4, B 			; Are we in COPY mode?
			JR	NZ, Key_TAB2			; Yes, so do COPY
Key_TAB1:		JP	Edit_Line_Loop
;
Key_TAB2:			PUSH	DE
			LD	E, 0			; Check whether we can insert
			CALL	Get_Length		; Get length of current line
			POP	DE
			LD	A, B 
			CP	255			
			JR	NC, Key_TAB1			; Skip if line limit (255) exceeded
;
			PUSH	DE
			PUSH	HL
			EX	DE, HL 
;			CALL	CERBERUS_GRAPHICS.Get_Char
			LD	C, A			; Store character in C
			POP	HL
			POP	DE
			JR	NC, Key_TAB1
			PUSH	DE
			CALL	Insert			; Insert the character into the buffer
			LD	(HL), C			; Store the character
			CALL	Update_1		; Update characters from cursor position
			DEC	B
			CALL	NZ, Update_2
			INC	L
			POP	DE
			JP	Key_HT_1

; Get line length
;  E: Start pointer value in buffer
; Returns
;  B: Number of characters, excluding CR
;
Get_Length_From_Cursor:	LD	E, L
Get_Length:		LD	B, 0
			LD	D, H
$$:			LD	A, (DE)
			CP	CR
			RET	Z 
			INC	B 
			INC	E
			JR	$B

; Move cursor
; DE: Cursor position
;
Move_Cursor_Left:	EQU	DEC_E_NZ		; In misc.z80
Move_Cursor_Up:		EQU	DEC_D_NZ		; In misc.z80
;
Move_Cursor_Right:	LD	A, CHAR_COLS - 1
			INC	E 
			CP	E
			RET	NC
			LD	E, 0 
;
Move_Cursor_Down	LD	A, CHAR_ROWS - 1
			INC	D
			CP	D 
			RET 	NC 
			DEC 	D
			RET 

; Update from cursor position
;  L: Cursor position
;
Update_1:		LD	D, H			; DE: Current cursor position
			LD	E, L
			LD	B, 0			; B: Number of characters output
$$:			LD	A, (DE)			; Read buffer
			CP	CR 			; Skip if CR
			RET	Z		
			CALL	OSWRCH			; Print the character out
			INC	E 			
			INC	B			; Increment # of chars output
			JR	$B 			; And loop

; Backspace a number of characters 
;  B: Character count
;
Update_2:		INC	B			; Is B=0 (EOL)
			DEC	B 	
			RET	Z			
			LD	A, 08H			; Restore cursor position
$$:			CALL	OSWRCH
			DJNZ	$B
			RET 

; Insert character
;  C: Character to insert
;  L: Cursor position
;
Insert:			CALL	Get_Length_From_Cursor	
			INC	B			; Need to loop at least once
$$:			LD	A, (DE)
			INC	E
			LD	(DE), A 
			DEC	E 
			DEC	E
			DJNZ	$B
			RET

; Delete character
;  L: Cursor position
Delete:			CALL	Get_Length_From_Cursor
			INC	B
			LD	E, L
$$:			LD	A, (DE)
			DEC	E 
			LD	(DE), A
			INC	E 
			INC 	E
			DJNZ	$B
			RET