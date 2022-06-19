;
; Title:	BBC Basic for AGON - Interrupts
; Author:	Dean Belfield
; Created:	06/05/2022
; Last Updated:	06/05/2022
;
; Modinfo:

			.ASSUME	ADL = 0
				
			INCLUDE	"equs.inc"

			SEGMENT CODE
				
			XDEF	INIT_IM2
				
; Initialise IM2
; HL: Address of vector table (must be on page boundary)
;
INIT_IM2:		DI
			LD	DE, INV_DUMMY
			LD	L, 0
$$:			LD	(HL), E
			INC	L
			LD	(HL), D
			INC	L
			JR	NZ, $B
			LD	A, H
			IM	2		; Set the interrupt mode
			LD	I, A	
			RET
				
			.ASSUME ADL = 1


; Interrupts in mixed mode always run in ADL mode
;
INV_DUMMY:		EI
			RETI.L
				