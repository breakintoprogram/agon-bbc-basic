;
; Title:	BBC Basic Interpreter - Z80 version
;		Catch-all for unimplemented functionality
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	03/05/2022
;
; Modinfo:

			.ASSUME	ADL = 0

			SEGMENT CODE
			
			XDEF	CLG
			XDEF	COLOUR
			XDEF	ENVEL
			XDEF	ADVAL
			XDEF	GETIMS
			XDEF	PUTIMS
			
			XREF	EXTERR
		
CLG:
COLOUR:
ENVEL:
ADVAL:
GETIMS:
PUTIMS:
			XOR     A
			CALL    EXTERR
			DEFB    "Sorry"
			DEFB    0
