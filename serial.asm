;
; Title:	BBC Basic for AGON - UART support
; Author:	Dean Belfield
; Created:	03/05/2022
; Last Updated:	03/05/2022
;
; Modinfo:

			.ASSUME	ADL = 0		; Switch assembler to 24 bit source
			
			INCLUDE	"equs.inc"
			INCLUDE	"macros.inc"				
			
			SEGMENT	CODE
			
			XDEF	INIT_UART
			XDEF	TX
			XDEF	RX
			XDEF	RX_WAIT
				
PORT			EQU	%C0		; UART0
				
REG_RBR:		EQU	PORT+0		; Receive buffer
REG_THR:		EQU	PORT+0		; Transmitter holding
REG_DLL:		EQU	PORT+0		; Divisor latch low
REG_IER:		EQU	PORT+1		; Interrupt enable
REG_DLH:		EQU	PORT+1		; Divisor latch high
REG_IIR:		EQU	PORT+2		; Interrupt identification
REG_FCT			EQU	PORT+2;		; Flow control
REG_LCR:		EQU	PORT+3		; Line control
REG_MCR:		EQU	PORT+4		; Modem control
REG_LSR:		EQU	PORT+5		; Line status
REG_MSR:		EQU	PORT+6		; Modem status

REG_SCR:		EQU 	PORT+7		; Scratch

TX_WAIT			EQU	2048		; Count before a TX times out

UART_LSR_ERR		EQU 	%80		; Error
UART_LSR_ETX		EQU 	%40		; Transmit empty
UART_LSR_ETH		EQU	%20		; Transmit holding register empty
UART_LSR_RDY		EQU	%01		; Data ready

; Initialise the UART
; HL: Clock divider
;  A: Framing 
; 
INIT_UART:		PUSH 	AF	
			SET_GPIO	PD_DDR,  3	; Set the GPIO pins for UART0
			RES_GPIO	PD_ALT1, 3
			SET_GPIO	PD_ALT2, 3				
			IN0		A,(REG_LCR)	; Turn DLAB on
			OR		 %80			
			OUT0		(REG_LCR),A
			LD		A, L		; Divisor low
			OUT0		(REG_DLL),A		
			LD		A, H		; Divisor high
			OUT0		(REG_DLH),A		
			POP		AF		; Framing (Start/Stop/Parity Bits)
			OUT0		(REG_LCR),A
			LD		A, %07		; Disable hardware FIFOs
			OUT0		(REG_FCT),A
			LD		A, %00		; Turn interrupts off
			OUT0		(REG_IER),A		
			RET
				

; Write a character to the UART
; A: Data to write
; Returns:
; F = C if written
; F = NC if timed out
;
TX:			PUSH		BC		; Stack BC
			PUSH		AF 		; Stack AF
			LD		BC,TX_WAIT	; Set CB to the transmit timeout
TX1:			IN0		A,(REG_LSR)	; Get the line status register
			AND 		UART_LSR_ETX	; Check for TX empty
			JR		NZ, TX2		; If set, then TX is empty, goto transmit
			DEC		BC
			LD		A, B
			OR		C
			JR		NZ, TX1
			POP		AF		; We've timed out at this point so
			POP		BC		; Restore the stack
			OR		A		; Clear the carry flag and preserve A
			RET	
TX2:			POP		AF		; Good to send at this point, so
			OUT0		(REG_THR),A	; Write the character to the UART transmit buffer
			POP		BC		; Restore BC
			SCF				; Set the carry flag
			RET 

; As RX, but wil wait until a character is received
; A: Data read
;
RX_WAIT:		CALL 		RX
			JR		NC,RX_WAIT 
			RET 

; Read a character from the UART
; A: Data read
; Returns:
; F = C if character read
; F = NC if no character read
;
RX:			IN0		A,(REG_LSR)	; Get the line status register
			AND 		UART_LSR_RDY	; Check for characters in buffer
			RET		Z		; Just ret (with carry clear) if no characters
			IN0		A,(REG_RBR)	; Read the character from the UART receive buffer
			SCF 				; Set the carry flag
			RET
				