;
; Title:	BBC Basic for AGON - Audio stuff
; Author:	Dean Belfield
; Created:	19/09/2022
; Last Updated:	21/03/2023
;
; Modinfo:
; 22/02/2023:	Fixed typo in sysvar_audioSuccess
; 21/03/2023:	Now uses vdp defines

			
			.ASSUME	ADL = 0
				
			INCLUDE	"equs.inc"
			INCLUDE "macros.inc"
			INCLUDE "mos_api.inc"	; In MOS/src
		
			SEGMENT CODE
			
			XDEF	SOUND
			
			XREF	COMMA
			XREF	EXPR_W2
			XREF	XEQ
			XREF	LTRAP
			XREF	OSWRCH
			XREF	VDU_BUFFER
			
				
; SOUND channel,volume,pitch,duration
; volume: 0 (off) to -15 (full volume)
; pitch: 0 - 255
; duration: -1 to 254 (duration in 20ths of a second, -1 = play forever)
;
SOUND:			CALL	EXPR_W2			; DE: Channel/Control, HL: Volume
			LD	A, L 			;  A: Volume
			PUSH	AF
			PUSH	DE
			CALL	COMMA
			CALL	EXPR_W2			; DE: Pitch, HL: Duration
			LD	D, E			;  D: Pitch
			LD	E, L 			;  E: Duration
			POP	HL 			; HL: Channel/Control
			POP	AF
			NEG
			CP	16			; Check volume is in bounds
			JP	NC, XEQ			; Out of bounds, do nothing
;
; Store	in VDU vars
; 
			LD	C, A			; Store Volume in C
			LD	A, L
			LD	(VDU_BUFFER+0), A	; Channel
			XOR	A
			LD	(VDU_BUFFER+1), A	; Waveform
; 
; Calculate the volume
; 
			LD	B, 6			; C already contains the volume
			MLT	BC			; Multiply by 6 (0-15 scales to 0-90)
			LD	A, C
			LD	(VDU_BUFFER+2), A
;
; And the frequency
;
			LD	C, E			; Store duration in C
			LD	H, 0			; Lookup the frequency
			LD	L, D
			LD	DE, SOUND_FREQ_LOOKUP
			ADD	HL, HL
			ADD	HL, DE
			LD	A, (HL)
			LD	(VDU_BUFFER+3), A
			INC	HL
			LD	A, (HL)
			LD	(VDU_BUFFER+4), A
;
; And now the duration - multiply it by 50 to convert from 1/20ths of seconds to milliseconds
;
			LD	B, 50			; C contains the duration, so MLT by 50
			MLT	BC
			LD	(VDU_BUFFER+5), BC
;
			PUSH	IX			; Get the system vars in IX
			MOSCALL	mos_sysvars		; Reset the semaphore
SOUND0:			RES.LIL	3, (IX+sysvar_vpd_pflags)
;
			VDU	23			; Send the sound command
			VDU	0
			VDU	vdp_audio
			VDU	(VDU_BUFFER+0)		; 0: Channel
			VDU	(VDU_BUFFER+1)		; 1: Waveform (0)
			VDU	(VDU_BUFFER+2)		; 2: Volume (0-100)
			VDU	(VDU_BUFFER+3)		; 3: Frequency L
			VDU	(VDU_BUFFER+4)		; 4: Frequency H
			VDU	(VDU_BUFFER+5)		; 5: Duration L
			VDU	(VDU_BUFFER+6)		; 6: Duration H
;
; Wait for acknowledgement
;
$$:			BIT.LIL	3, (IX+sysvar_vpd_pflags)
			JR	Z, $B			; Wait for the result
			CALL	LTRAP			; Check for ESC
			LD.LIL	A, (IX+sysvar_audioSuccess)
			AND	A			; Check if VDP has queued the note
			JR	Z, SOUND0		; No, so loop back and send again
;
			POP	IX
			JP	XEQ

; Frequency Lookup Table
; Set up to replicate the BBC Micro audio frequencies
;
; Split over 5 complete octaves, with 53 being middle C
; * C4: 262hz
; + A4: 440hz
;
;	2	3	4	5	6	7	8
;
; B	1	49	97	145	193	241	
; A#	0	45	93	141	189	237	
; A		41	89+	137	185	233	
; G#		37	85	133	181	229	
; G		33	81	129	177	225	
; F#		29	77	125	173	221	
; F		25	73	121	169	217	
; E		21	69	117	165	213	
; D#		17	65	113	161	209	
; D		13	61	109	157	205	253
; C#		9	57	105	153	201	249
; C		5	53*	101	149	197	245
;
SOUND_FREQ_LOOKUP:	DW	 117,  118,  120,  122,  123,  131,  133,  135
			DW	 137,  139,  141,  143,  145,  147,  149,  151
			DW	 153,  156,  158,  160,  162,  165,  167,  170
			DW	 172,  175,  177,  180,  182,  185,  188,  190
			DW	 193,  196,  199,  202,  205,  208,  211,  214
			DW	 217,  220,  223,  226,  230,  233,  236,  240
			DW	 243,  247,  251,  254,  258,  262,  265,  269
			DW	 273,  277,  281,  285,  289,  294,  298,  302
			DW	 307,  311,  316,  320,  325,  330,  334,  339
			DW	 344,  349,  354,  359,  365,  370,  375,  381
			DW	 386,  392,  398,  403,  409,  415,  421,  427
			DW	 434,  440,  446,  453,  459,  466,  473,  480
			DW	 487,  494,  501,  508,  516,  523,  531,  539
			DW	 546,  554,  562,  571,  579,  587,  596,  605
			DW	 613,  622,  631,  641,  650,  659,  669,  679
			DW	 689,  699,  709,  719,  729,  740,  751,  762
			DW	 773,  784,  795,  807,  819,  831,  843,  855
			DW	 867,  880,  893,  906,  919,  932,  946,  960
			DW	 974,  988, 1002, 1017, 1032, 1047, 1062, 1078
			DW	1093, 1109, 1125, 1142, 1158, 1175, 1192, 1210
			DW	1227, 1245, 1263, 1282, 1300, 1319, 1338, 1358
			DW	1378, 1398, 1418, 1439, 1459, 1481, 1502, 1524
			DW	1546, 1569, 1592, 1615, 1638, 1662, 1686, 1711
			DW	1736, 1761, 1786, 1812, 1839, 1866, 1893, 1920
			DW	1948, 1976, 2005, 2034, 2064, 2093, 2123, 2154
			DW	2186, 2217, 2250, 2282, 2316, 2349, 2383, 2418
			DW	2453, 2489, 2525, 2562, 2599, 2637, 2675, 2714
			DW	2754, 2794, 2834, 2876, 2918, 2960, 3003, 3047
			DW	3091, 3136, 3182, 3228, 3275, 3322, 3371, 3420
			DW	3470, 3520, 3571, 3623, 3676, 3729, 3784, 3839
			DW	3894, 3951, 4009, 4067, 4126, 4186, 4247, 4309
			DW	4371, 4435, 4499, 4565, 4631, 4699, 4767, 4836	


