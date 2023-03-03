;
; Title:	BBC Basic Interpreter - Z80 version
;		RAM Module for BBC Basic Interpreter
;		For use with Version 2.0 of BBC BASIC
;		Standard CP/M Distribution Version
; Author:	(C) Copyright  R.T.Russell 31-12-1983
; Modified By:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	26/02/2023
;
; Modinfo:
; 03/08/2022:	Removed TIME and KEY_CODE - now in MOS
; 26/02/2023:	Tidied up and updated comments

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
			XDEF	OSWRCHPT
			XDEF	OSWRCHCH
			XDEF	OSWRCHFH
			
			XDEF	RAM_START
			XDEF	RAM_END
			XDEF	USER

RAM_START:

;n.b. ACCS, BUFFER & STAVAR must be on page boundaries.
;
ACCS:			DS		256             ; String Accumulator
BUFFER:			DS		256             ; String Input Buffer
STAVAR:			DS	 	27*4            ; Static Variables
DYNVAR: 		DS 		54*2            ; Dynamic Variable Pointers
FNPTR:  		DS    		2               ; Dynamic Function Pointers
PROPTR: 		DS		2               ; Dynamic Procedure Pointers
;
PAGE_:   		DS		2               ; Start of User Program
TOP:    		DS		2               ; First Location after User Program
LOMEM:  		DS		2               ; Start of Dynamic Storage
FREE:   		DS		2               ; First Free Space Byte
HIMEM:  		DS		2               ; First Protected Byte
;
LINENO: 		DS		2               ; Line Number
TRACEN:			DS		2               ; Trace Flag
AUTONO:			DS		2               ; Auto Flag
ERRTRP:			DS		2               ; Error Trap
ERRTXT:			DS		2               ; Error Message Pointer
DATPTR:			DS		2               ; Data Pointer
ERL:			DS		2               ; Error Line
ERRLIN:			DS		2               ; The "ON ERROR" Line
RANDOM:			DS		5               ; Random Number
COUNT:			DS		1               ; Print Position
WIDTH:			DS		1               ; Print Width
ERR:			DS		1               ; Error Number
LISTON:			DS		1               ; LISTO (bottom nibble)
							; - BIT 0: If set, output a space after the line number
							; - BIT 1: If set, then indent FOR/NEXT loops
							; - BIT 2: If set, then indent REPEAT/UNTIL loops
							; - BIT 3: If set, then output to buffer for *EDIT
							; OPT FLAG (top nibble)
							; - BIT 4: If set, then list whilst assembling
							; - BIT 5: If set, then assembler errors are reported
							; - BIT 6: If set, then place the code starting at address pointed to by O%
							; - BIT 7: Unused
INCREM:			DS		1               ; Auto-Increment Value
;
; Extra Agon-implementation specific system variables
;
FLAGS:			DS		1		; Miscellaneous flags
							; - BIT 7: Set if ESC pressed
							; - BIT 6: Set to disable ESC
OSWRCHPT:		DS		2		; Pointer for *EDIT
OSWRCHCH:		DS		1		; Channel of OSWRCH
							; - 0: Console
							; - 1: File
OSWRCHFH:		DS		1		; File handle for OSWRCHCHN
;
; This must be at the end
;
RAM_END:							
				ALIGN	256			
USER:							; Must be aligned on a page boundary
	