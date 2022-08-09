;
; Title:	BBC Basic Interpreter - Z80 version
;		Catch-all for unimplemented functionality
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	07/08/2022
;
; Modinfo:
; 07/08/2022:	Moved COLOUR and CLG to patch.asm

			.ASSUME	ADL = 0

			SEGMENT CODE
			
			XDEF	ENVEL
			XDEF	ADVAL
			XDEF	GETIMS
			XDEF	PUTIMS
			
			XREF	EXTERR
				
ENVEL:
ADVAL:
GETIMS:
PUTIMS:
			XOR     A
			CALL    EXTERR
			DEFB    "Sorry"
			DEFB    0
