	page	,132
;-----------------------------Module-Header-----------------------------;
; Module Name:	INPORT.INC
;
; Equates for the Micrsoft Universal Pointing Device Interface
; (also known as the InPort Mouse).
;
; Created: Yester-year
; Author:  Walt Moore [waltm] and others
;
; Copyright (c) 1984-1987 Microsoft Corporation
;
; Exported Functions:	none
;
; Public Functions:	none
;
; Public Data:		none
;
; General Description:
;
; Restrictions:
;
;-----------------------------------------------------------------------;



INPORT_ADDR_PTR 	equ	0		;Address pointer register
INPORT_ADDR		equ	00000111b	;  Address lines
INPORT_RESET		equ	10000000b	;  Reset


INPORT_DATA_REG 	equ	1		;Data Register (pointed to by
						;  INPORT_ADDR_PTR)

INPORT_ID		equ	2		;Identification Register
INPORT_REVISION 	equ	00001111b	;  Revision Number
INPORT_VERSION		equ	11110000b	;  Version Number
INPORT_CODE		equ	0DEh		;  Chip code is always this


UPID_TEST_REG		equ	3		;Test Register




;       The following registers are accessed by setting the address
;       register and reading from the data register.  Not all bits
;       are valid in all modes of operation.
;
;       The modes which the fields are valid are noted in the right
;       margin, as stated in Revision 1.0 of the Micrsoft Universal
;       Pointing Device Interface Applications Notes dated November,
;       1985:
;
;               Q - Valid in quadrature mode
;               M - Valid in MSX mode
;               A - Valid in absolute mode
;               D - valid in direct mode



INPORT_MOUSE_STAT	equ	0		;Mouse Status Register
INPORT_STAT_B3		equ	00000001b	;  Status Button 3	    Q D
INPORT_STAT_B2		equ	00000010b	;  Status Button 2	    QMDA
INPORT_STAT_B1		equ	00000100b	;  Status Button 1	    QMD
INPORT_DELTA_B3 	equ	00001000b	;  Delta  Button 3	    Q D
INPORT_DELTA_B2 	equ	00010000b	;  Delta  Button 2	    QMDA
INPORT_DELTA_B1 	equ	00100000b	;  Delta  Button 1	    QMD
INPORT_MOVEMENT 	equ	01000000b	;  Movement		    Q
INPORT_PACKET		equ	10000000b	;  Packet complete	     M A


INPORT_DATA_1		equ	1		;Data Register 1
INPORT_X_MOVE		equ	11111111b	;  X Movement		    QM
INPORT_NIBBLE_2 	equ	00001111b	;  Input data nibble 2	       A
INPORT_NIBBLE_1 	equ	11110000b	;  Input data nibble 1	       A


INPORT_DATA_2		equ	2		;Data Register 2
INPORT_Y_MOVE		equ	11111111b	;  Y Movement		    QM
INPORT_NIBBLE_4 	equ	00001111b	;  Input data nibble 4	       A
INPORT_NIBBLE_3 	equ	11110000b	;  Input data nibble 3	       A


INPORT_DATA_3		equ	3		;Data Register 3
INPORT_NIBBLE_6 	equ	00001111b	;  Input data nibble 6	       A
INPORT_NIBBLE_5 	equ	11110000b	;  Input data nibble 5	       A


INPORT_DATA_4		equ	4		;Data Register 4
INPORT_NIBBLE_8 	equ	00001111b	;  Input data nibble 8	       A
INPORT_NIBBLE_7 	equ	11110000b	;  Input data nibble 7	       A


INPORT_STATUS		equ	5		;Interface Status Register
INPORT_STATE_SW3	equ	00000001b	;  State SW3		    QMDA
INPORT_STATE_SW2	equ	00000010b	;  State SW2		    QMDA
INPORT_STATE_SW1	equ	00000100b	;  State SW1		    QMDA
;			equ	00001000b
INPORT_STATE_XA 	equ	00010000b	;  State XA		    QMDA
INPORT_STATE_XB 	equ	00100000b	;  State XB		    QMDA
INPORT_STATE_YA 	equ	01000000b	;  State YA		    QMDA
INPORT_STATE_YB 	equ	10000000b	;  State YB		    QMDA


INPORT_CONTROL		equ	6		;Interface Control Register
INPORT_SW3		equ	00000001b	;  SW3			    QMDA
INPORT_SW2		equ	00000010b	;  SW2			    QMDA
INPORT_SW1		equ	00000100b	;  SW1			    QMDA
;			equ	00001000b
INPORT_XA		equ	00010000b	;  XA			    QMDA
INPORT_XB		equ	00100000b	;  XB			    QMDA
INPORT_YA		equ	01000000b	;  YA			    QMDA
INPORT_YB		equ	10000000b	;  YB			    QMDA


INPORT_MODE		equ	7		;Mode Register
INPORT_TIMER		equ	00000111b	;  Timer Select bits	    QMDA
INPORT_0		equ	00000000b	;      0 Hz - INTR = 0	    QMDA
INPORT_HZ_30		equ	00000001b	;     30 Hz		    QMDA
INPORT_HZ_50		equ	00000010b	;     50 HZ		    QMDA
INPORT_HZ_100		equ	00000011b	;    100 Hz		    QMDA
INPORT_HZ_200		equ	00000100b	;    200 HZ		    QMDA
;			equ	00000101b	;    Reserved		    QMDA
INPORT_1		equ	00000110b	;      0 HZ - INTR = 1	    QMDA
INPORT_EXT_CLK		equ	00000111b	;    External		    QMDA
INPORT_DATA_IE		equ	00001000b	;  Data Interrupt Enable    QMDA
INPORT_TIMER_IE 	equ	00010000b	;  Timer Interrupt Enable   QMDA
INPORT_HOLD		equ	00100000b	;  Hold 		    QMDA
INPORT_MODE_SEL 	equ	11000000b	;  Mode Select		    QMDA
INPORT_QUAD		equ	00000000b	;    Quadrature mode	    QMDA
INPORT_MSX		equ	01000000b	;    MSX mode		    QMDA
INPORT_ABS		equ	10000000b	;    ABS mode		    QMDA
INPORT_DIRECT		equ	11000000b	;    Direct mode	    QMDA
