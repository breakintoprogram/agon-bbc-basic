;
; Title:	BBC Basic Interpreter - Z80 version
;		RAM Module for BBC Basic Interpreter
;		For use with Version 2.0 of BBC BASIC
;		Standard CP/M Distribution Version
; Author:	(C) Copyright  R.T.Russell 31-12-1983
; Modified By:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	03/08/2022
;
; Modinfo:
; 03/08/2022:	Removed TIME and KEY_CODE - now in MOS

			.ASSUME	ADL = 0

			DEFINE	LORAM, SPACE = ROM
			SEGMENT LORAM

			ALIGN 256

			XDEF	ACCS
			XDEF	BUFFER
			XDEF	STAVAR
			XDEF	DYNVAR
			XDEF	FNPTR
			XDEF	PROPTR
			XDEF	PAGE_
			XDEF	TOP
			XDEF	LOMEM
			XDEF 	FREE
			XDEF	HIMEM
			XDEF	LINENO
			XDEF	TRACEN
			XDEF	AUTONO
			XDEF	ERRTRP
			XDEF	ERRTXT
			XDEF	DATPTR
			XDEF	ERL
			XDEF	ERRLIN
			XDEF	RANDOM
			XDEF	COUNT
			XDEF	WIDTH
			XDEF	ERR
			XDEF	LISTON
			XDEF	INCREM
			
			XDEF	FLAGS
;			XDEF	TIME
;			XDEF	KEY_CODE
			XDEF	CURSOR_X
			XDEF	CURSOR_Y
;			XDEF	CHARPOS_X
;			XDEF	CHARPOS_Y
			
			XDEF	INV_START
			XDEF	RAM_START
			XDEF	RAM_END
			XDEF	USER

INV_START:		DS		256				; IM2 Interrupt vectors
	
RAM_START:

;n.b. ACCS, BUFFER & STAVAR must be on page boundaries.
;
ACCS:			DS		256             ; STRING ACCUMULATOR
BUFFER:			DS		256             ; STRING INPUT BUFFER
STAVAR:			DS	 	27*4            ; STATIC VARIABLES
DYNVAR: 		DS 		54*2            ; DYN. VARIABLE POINTERS
FNPTR:  		DS    		2               ; DYN. FUNCTION POINTER
PROPTR: 		DS		2               ; DYN. PROCEDURE POINTER
;
PAGE_:   		DS		2               ; START OF USER PROGRAM
TOP:    		DS		2               ; FIRST LOCN AFTER PROG.
LOMEM:  		DS		2               ; START OF DYN. STORAGE
FREE:   		DS		2               ; FIRST FREE-SPACE BYTE
HIMEM:  		DS		2               ; FIRST PROTECTED BYTE
;
LINENO: 		DS		2               ; LINE NUMBER
TRACEN:			DS		2               ; TRACE FLAG
AUTONO:			DS		2               ; AUTO FLAG
ERRTRP:			DS		2               ; ERROR TRAP
ERRTXT:			DS		2               ; ERROR MESSAGE POINTER
DATPTR:			DS		2               ; DATA POINTER
ERL:			DS		2               ; ERROR LINE
ERRLIN:			DS		2               ; "ON ERROR" LINE
RANDOM:			DS		5               ; RANDOM NUMBER
COUNT:			DS		1               ; PRINT POSITION
WIDTH:			DS		1               ; PRINT WIDTH
ERR:			DS		1               ; ERROR NUMBER
LISTON:			DS		1               ; LISTO & OPT FLAG
INCREM:			DS		1               ; AUTO INCREMENT
;
; Extra Agon-implementation specific system variables
;
FLAGS:			DS		1		; Flags: B7=ESC PRESSED, B6=ESC DISABLED, B4=COPY PRESSED
; NOTE: FLAGS is normally USER-1, accessible as ?(PAGE-1).
; NOTE: ACS string buffer is normally USER-768, accessible as $(PAGE-768)
;
;TIME:			DS		4
;KEY_CODE:		DS		1
;VDU_BUFFER:		DS		8		; 8 bytes of temporary storage for the VDU buffer
CURSOR_X:		DS		1
CURSOR_Y:		DS		1
;CHARPOS_X:		DS		1
;CHARPOS_Y		DS		1
;
; This must be at the end
;
RAM_END:							
	
				ALIGN	256
				
USER:							; Must be aligned on a page boundary
	