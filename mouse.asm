       page    ,132
;-----------------------------Module-Header-----------------------------;
; Module Name:	MOUSE.ASM
;
; Windows mouse driver data and initialization routines.
;
; Created: Yester-year
; Author:  Walt Moore [waltm], Mr. Mouse, and a cast of tens.
;  Sun 04-Dec-1988 21:31:36  -by-  David N. Weise  [davidw]
; Made it bi-modal.
;  03-May-1989 RAL -by- Ralph Lipe
;   Calls Windows/386 VxD when mouse detected
;
; Copyright (c) 1987, 1988, 1989  Microsoft Corporation
;
; Exported Functions:
;
; Public Functions:
;
; Public Data:
;
; General Description:
;
;   This segment contains all static data used by the mouse routines.  The
;   hardware interrupt routine is included in the static data so that it
;   has addressability to the static data through the CS register.  Thus
;   the data segment of this module MUST be fixed in memory, while the
;   code segment can be moveable and/or discardable.
;
;-----------------------------------------------------------------------;

	title	Mouse - Main Mouse Module

	.xlist
	include cmacros.inc
	include windefs.inc
	include mouse.inc
	.list

	externNP ps2_search
	externNP ps2_enable
	externNP ps2_disable
	externNP ps2_int

	externA  __WINFLAGS

sBegin	Data

globalW 	WinFlags,__WINFLAGS	;Windows environment flags
globalB 	vector,-1		;Vector # of mouse interrupt
globalB 	mask_8259,0FFh		;8259 interrupt enable mask, FF if none
globalB 	old_8259_mask,0FFh	;Value of mouse irq bit before enable
globalB 	mouse_flags,0		;Various flags
globalB 	mouse_type,0		;Type of mouse (inport/bus/serial/etc.)

		even			;Want words on word boundary

globalW 	io_base,0		;Mouse port base address
globalD 	event_proc,0		;Mouse event procedure when enabled
globalD 	bios_proc,0		;Contents of old interrupt vector

globalW 	interrupt_rate,30	;Maximum interrupt rate of mouse

page

sEnd	Data

sBegin	Code
assumes cs,Code

;--------------------------Exported-Routine-----------------------------;
; int Inquire(lp_mouse_info);
;
; Information regarding the mouse is returned to the caller.
;
; Entry:
;	None
; Returns:
;	AX = # bytes returned in lp_mouse_info
; Error Returns:
;	None
; Registers Preserved:
;	SI,DI,DS,BP
; Registers Destroyed:
;	AX,BX,CX,DX,ES,FLAGS
; Calls:
;	None
; History:
;	Fri 21-Aug-1987 11:43:42 -by-  Walt Moore [waltm] & Mr. Mouse
;	Initial version
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes cs,Code
	assumes ds,Data

cProc	Inquire,<FAR,PUBLIC,WIN,PASCAL>,<di>

	parmD	lp_mouse_info

cBegin
	les	di,lp_mouse_info	;Get far pointer of destination area
	assumes es,nothing

	mov	al,mouse_flags		;Get and save msExists, msRelative
	cbw
	xchg	al,ah			;AL = 0FFh if mouse exists, 00 if not
	.errnz	MF_MOUSE_EXISTS-80h

	mov	ah,0FFh 		;AH = 0FFh if relative coordinates
	stosw				;  (but is currently ignored!)
	.errnz	msExists
	.errnz	msRelative-msExists-1

	mov	ax,NUMBER_BUTTONS	;Return number of buttons
	stosw
	.errnz	msNumButtons-msRelative-1

	mov	ax,interrupt_rate	;Return maximum interrupt rate
	stosw
	.errnz	msRate-msNumButtons-2

	mov	ax,X_SPEED		;Return threshold before acceleration
	stosw
	.errnz	msXThresh-msRate-2

if Y_SPEED ne X_SPEED			;Generally they're the same
	mov	ax,Y_SPEED		;Return threshold before acceleration
endif
	stosw
	.errnz	msYThresh-msXThresh-2

	xor	ax,ax			;Return useless x,y resolution info
	stosw
	stosw
	.errnz	msXRes-msYThresh-2
	.errnz	msYRes-msXRes-2
	.errnz	msYRes+2 - size MOUSEINFO

	mov	ax,size MOUSEINFO	;Return size of info

cEnd
page

;--------------------------Exported-Routine-----------------------------;
; void Enable(lp_event_proc);
;
; Enable hardware mouse interrupts, with the passed procedure address
; being the target of all mouse events.
;
; This routine may be called while already enabled.  In this case the
; passed event procedure should be saved, and all other initialization
; skipped.
;
; Entry:
;	None
; Returns:
;	None
; Error Returns:
;	None
; Registers Preserved:
;	SI,DI,DS,BP
; Registers Destroyed:
;	AX,BX,CX,DX,ES,FLAGS
; Calls:
;	ps2_enable
; History:
;	Fri 21-Aug-1987 11:43:42 -by-  Walt Moore [waltm] & Mr. Mouse
;	Initial version
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes cs,Code
	assumes ds,Data

cProc	Enable,<FAR,PUBLIC,WIN,PASCAL>,<si,di>

	parmD	new_event_proc

cBegin

;	The new event procedure is always saved regardless of the
;	mouse already being enabled.  This allows the event proc
;	to be changed as needed.

	cli				;Protect against interrupt while
	mov	ax,off_new_event_proc	;  changing the vector
	mov	wptr event_proc[0],ax
	mov	ax,seg_new_event_proc
	mov	wptr event_proc[2],ax
	sti

;	If the mouse is already enabled, or it doesn't exist, then
;	we're all done with the enable call.

	mov	al,mouse_flags		;If enabled or mouse doesn't exist,
	xor	al,MF_MOUSE_EXISTS	;  then skip the enabling
	test	al,MF_ENABLED+MF_MOUSE_EXISTS
	jnz	enable_done
	call	ps2_enable		;Mouse specific initialization
	or	mouse_flags,MF_ENABLED	;Show enabled now

enable_done:

cEnd
page

;--------------------------Exported-Routine-----------------------------;
; void Disable();
;
; Disable hardware mouse interrupts, restoring the previous mouse
; interrupt handler and 8259 interrupt enable mask.
;
; This routine may be called while already disabled.  In this case the
; disabling should be ignored.
;
; Entry:
;	None
; Returns:
;	None
; Error Returns:
;	None
; Registers Preserved:
;	SI,DI,DS,BP
; Registers Destroyed:
;	AX,BX,CX,DX,ES,FLAGS
; Calls:
;	ps2_disable
; History:
;	Fri 21-Aug-1987 11:43:42 -by-  Walt Moore [waltm] & Mr. Mouse
;	Initial version
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes cs,Code
	assumes ds,Data

cProc	Disable,<FAR,PUBLIC,WIN,PASCAL>,<si,di>

cBegin
	test	mouse_flags,MF_ENABLED
	jz	disable_done		;Mouse is already disabled
	call	ps2_disable		;Disable as needed
	and	mouse_flags,not MF_ENABLED

disable_done:

cEnd
page

;--------------------------Exported-Routine-----------------------------;
; WORD WEP();
;
; Generic WEP.
;
; Entry:
;	None
; Returns:
;	AX = 1
; Error Returns:
;	None
; Registers Preserved:
;	all
; Registers Destroyed:
;	none
; Calls:
;	nothing
; History:
;  Wed 18-Oct-1989 11:44:39  -by-  David N. Weise  [davidw]
; Wrote it!
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes cs,Code
	assumes ds,Data

cProc	WEP,<FAR,PUBLIC,WIN,PASCAL>
;	parmW	stuff
cBegin nogen
	mov	ax,1
	ret	2
cEnd nogen
page

;--------------------------Exported-Routine-----------------------------;
; int MouseGetIntVect();
;
; The interrupt vector used by the mouse is returned to the caller.
; If no mouse is found, then -1 is returned.
;
; Entry:
;	None
; Returns:
;	AX = interrupt vector
;	AX = -1 if no mouse was found
; Error Returns:
;	None
; Registers Preserved:
;	SI,DI,DS,BP
; Registers Destroyed:
;	AX,BX,CX,DX,ES,FLAGS
; Calls:
;	None
; History:
;	Fri 21-Aug-1987 11:43:42 -by-  Walt Moore [waltm] & Mr. Mouse
;	Initial version
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes cs,Code
	assumes ds,Data

cProc	MouseGetIntVect,<FAR,PUBLIC,WIN,PASCAL>

cBegin

;	NOTE! vector must be less than 7Fh for the sign extension to work

	mov	al,vector		;Will be -1 if mouse wasn't found
	cbw				;AX = -1 if no mouse was found
cEnd
page

;---------------------------Public-Routine-----------------------------;
; hook_us_in
;
; This is a utility routine for the specific mouse handlers.  The
; following initialization will be performed in preperation of
; hooking in the interrupt handler:
;
;	save old 8259 mask
;	disable our IRQ at the 8259
;	save old interrupt vector contents
;	set  new interrupt vector
;
; Entry:
;	None offset in Data segment of interrupt handler
; Returns:
;	None
; Error Returns:
;	None
; Registers Preserved:
;	DI,SI,DS,BP
; Registers Destroyed:
;	AX,BX,CX,DX,ES,FLAGS
; Calls:
;	Int 21h
; History:
;	Mon 24-Aug-1987 22:41:22 -by-  Walt Moore [waltm] & Mr. Mouse
;	Initial version
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes cs,Code
	assumes ds,Data

		public	hook_us_in
hook_us_in	proc	near

	mov	ah,mask_8259		;Get our 8259 enable mask
	xor	ah,0FFh 		;A 1 bit for our IRQ (not AH)
	jz	hook_us_in_no_irq	;No IRQ is involved
	cli
	in	al,MASK_PORT		;Get current 8259 mask
	mov	bl,al
	or	al,ah
	and	bl,ah			;Isolate old mouse IRQ bit
	out	MASK_PORT,al		;Disable mouse int
	sti

	mov	old_8259_mask,bl	;Will be 0 if previously enabled

hook_us_in_no_irq:
	mov	ah,35h			;Save old interrupt vector
	mov	al,vector
	int	21h
	mov	wptr bios_proc[0],bx
	mov	wptr bios_proc[2],es

	push	ds
	push	seg ps2_int
	mov	ah,25h			;Set our interrupt vector
	mov	al,vector		; need the real DS here; cs[vector] bad
	pop	ds			; now it's safe to set DS for DS:DX
	mov	dx,CodeOFFSET ps2_int
	int	21h
	pop	ds
	sti

	ret

hook_us_in	endp
page

;---------------------------Public-Routine-----------------------------;
; enable_our_int
;
; The 8259 is enabled for our interrupt, as specified by mask_8259.
;
; This routine will only be called by those functions which actually
; require an IRQ.
;
; Entry:
;	None
; Returns:
;	None
; Error Returns:
;	None
; Registers Preserved:
;	BX,CX,DX,SI,DI,ES,DS,BP
; Registers Destroyed:
;	AX,FLAGS
; Calls:
;	None
; History:
;	Mon 24-Aug-1987 22:41:22 -by-  Walt Moore [waltm] & Mr. Mouse
;	Initial version
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes cs,Code
	assumes ds,Data

		public	enable_our_int
enable_our_int	proc	near

	mov	ah,mask_8259		;Get our enable mask
	cli
	in	al,MASK_PORT		;Get current 8259 mask
	and	al,ah			;Set our IRQ bit to 0 (enabled)
	out	MASK_PORT,al
	sti
	ret

enable_our_int	endp
page

;---------------------------Public-Routine-----------------------------;
; unhook_us
;
; This is a utility routine for the specific mouse handlers.  The
; interrupt vector will be restored to its previous value and the
; old IRQ enable bit will be restored.
;
; Entry:
;	None
; Returns:
;	'Z' set   if IRQ should be left enabled  at the device
;	'Z' clear if IRQ should be left disabled at the device
; Error Returns:
;	None
; Registers Preserved:
;	SI,DI,DS,BP
; Registers Destroyed:
;	AX,BX,CX,DX,ES,FLAGS
; Calls:
;	Int 21h
; History:
;	Mon 24-Aug-1987 22:41:22 -by-  Walt Moore [waltm] & Mr. Mouse
;	Initial version
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes cs,Code
	assumes ds,Data

		public	unhook_us
unhook_us	proc	near

	mov	ah,mask_8259		;Disable our interrupt at the
	xor	ah,0FFh 		;  8259 before changing the (not AH)
	jz	unhook_no_irq		;  vector since old versions
	cli				;  of DOS may not cli/sti
	in	al,MASK_PORT		;  while setting int vectors
	or	al,ah
	out	MASK_PORT,al
	sti

unhook_no_irq:
	cmp	wptr bios_proc[2],0	; PS2 mouse may have failed enable
	jz	unhook_no_bios_proc	;  so bios proc might not be set.
	push	ds
	mov	ah,25h			;Restore old interrupt vector
	mov	al,vector
	lds	dx,bios_proc
	assumes ds,nothing

	int	21h

	pop	ds
	assumes ds,Data

unhook_no_bios_proc:

;	If IRQ mask will be set to whatever value it was when we were
;	enabled.  If there is no Int 33h mouse driver in the system,
;	then flag the caller that interrupts should be disabled at the
;	device.  If there is an Int33h mouse driver and interrupts
;	were disabled upon entry, also flag the caller to disable
;	interrupts at the device.  Only allow device interrupts to
;	continue being generated if there is an Int33h mouse and
;	the IRQ was previously enabled.
;
;	This is in part a fix for running on IRQ2 in Compaqs and
;	other machines which try to allow this.  I don't recommend
;	IRQ2 for ATs and compatibles, but some people try and it
;	almost seems to work if mouse.com is there to handle the
;	interrupt.

	test	old_8259_mask,0FFh	;If interrupts were previous disabled
	jnz	unhook_exit		; then leave them disabled ('Z' clear)
	mov	ah,mask_8259
	cli
	in	al,MASK_PORT
	and	al,ah
	out	MASK_PORT,al
	sti
	mov	al,mouse_flags
	not	al
	and	al,MF_INT33H		;'Z' clear if to disable at the device

unhook_exit:
	ret

unhook_us	endp
page

;---------------------------Public-Routine-----------------------------;
; Initialize
;
; All boot time initialization will be performed.  This basically
; involves searching for a mouse in the system.  The ordering in
; which we will search is as follows:
;
;	VMware paravirtual mouse
;
; After a mouse handler has been found, the Data segment will be
; resized to the minimum needed to support the mouse in use.
;
; Entry:
;	CX = size of heap
;	DI = module handle
;	DS = automatic data segment
;	ES:SI = address of command line (not used)
; Returns:
;	AX <> 0 to show success
; Error Returns:
;	None
; Registers Preserved:
;	SI,DI,DS,BP
; Registers Destroyed:
;	AX,BX,CX,DX,ES,FLAGS
; Calls:
;	PS2_search
; History:
;	Fri 21-Aug-1987 11:43:42 -by-  Walt Moore [waltm] & Mr. Mouse
;	Initial version
;-----------------------------------------------------------------------;

;------------------------------Pseudo-Code------------------------------;
; {
; }
;-----------------------------------------------------------------------;

	assumes cs,Code
	assumes ds,Data

cProc	Initialize,<FAR,PUBLIC,WIN,PASCAL>,<si,di>

cBegin

;-----------------------------------------------------------------------;
;	Regardless of using the Int33h mouse driver, we need to
;	know if it is present for disabling IRQs when the mouse
;	is disabled.
;-----------------------------------------------------------------------;

if 0	;----------------------------------------------------------------

; This code was for earlier versions of the Microsoft mouse driver which
; didn't support the GetMouseInfo call.  Windows 3.0 is shipping DOS
; mouse driver 7.0x.  6.?? added the GetMouseInfo call.

;	We would have liked to have done a mouse reset to see if
;	the mouse was installed.  However, this disables any
;	user-installed call out (like menus).  What we will do
;	instead is the following:
;
;	    Swap in a bogus interrupt procedure and an event
;	    mask which says don't call out.  If the mouse driver
;	    is present, then we should get back something different
;	    than what was passed in.  If we do get back something
;	    different, we'll restore that data and see if what is
;	    returned is what was originally passed in.	If so, we
;	    have a mouse driver, version 6.00 or greater and menus
;	    will still be enabled.
;
;	    If the above test fails, we'll do the mouse reset to
;	    see if the mouse is installed.  This will trash menus,
;	    but there isn't much we can do about it.

	test	hello,1 		;!!! just for now  (WinFlags)
	jnz	check_ps2
	push	cs			;ES:DX --> bogus_far_ret
	pop	es
	mov	dx,CodeOFFSET bogus_far_ret
	xor	cx,cx			;Don't call installed int proc
	mov	ax,SWAP_INT_PROC	;Swap interrupt routine
	int	MOUSE_SYS_VEC
	cmp	dx,CodeOFFSET bogus_far_ret
	jne	swap_them_back		;Different interrupt proc address,
	mov	ax,cs			;  might be a mouse
	mov	bx,es
	cmp	ax,bx
	je	try_mouse_reset 	;Int proc address same, might be < 6.00

;-----------------------------------------------------------------------;
;	The procedure address which was returned was different that what
;	we passed in.  Restore it and see if what is returned is what we
;	had originally passed in.  If so, this is a 6.00 or greater mouse.
;-----------------------------------------------------------------------;

swap_them_back:
	mov	ax,SWAP_INT_PROC	;Swap interrupt routine.  Should get
	int	MOUSE_SYS_VEC		;  back what we set initially
	cmp	dx,CodeOFFSET bogus_far_ret
	jne	try_mouse_reset 	;Not a 6.00+ mouse driver, go see
	mov	ax,cs			;  if it might some other version
	mov	bx,es
	cmp	ax,bx
	jne	try_mouse_reset 	;Not 6.00+
	jcxz	sys_mouse_present	;6.00 or greater

else	;-----------------------------------------------------------------

	xor	bx,bx			;Current DOS mouse drivers implement
	mov	ax,INT33H_GETINFO	;  a Get Mouse Info call
	int	MOUSE_SYS_VEC
	cmp	bh,6			;Major version in bh, started @ 6.??
	jb	try_mouse_reset
	jmp	short sys_mouse_present

endif	;-----------------------------------------------------------------

try_mouse_reset:
	xor	ax,ax			;Check for DOS mouse driver
	.errnz	INT33H_RESET
	int	MOUSE_SYS_VEC
	or	ax,ax			;Zero if no mouse installed
	jz	check_ps2

sys_mouse_present:
	or	mouse_flags,MF_INT33H	;Show mouse driver is present

check_ps2:
	mov	mouse_type, MT_PS2	;Assume PS/2 mouse
	call	ps2_search		;PS/2 mouse port?
	jc	got_mouse
	mov	mouse_type, MT_NO_MOUSE ;Reset mouse type to none
	mov	si,MAX_INT_SIZE
	jmp	short resize_ds

got_mouse:
;
;	If running under Windows/386 then tell the Win386 mouse driver what
;	type of mouse we found.
;
	push	ax
	push	bx
	push	di
	push	es

	%OUT Test flags for Win386 here!
	xor	di, di
	mov	es, di
	mov	ax, 1684h		;Get device API entry point
	mov	bx, VMD_DEVICE_ID	;for the Virtual Mouse Device
	int	2Fh
	mov	ax, es
	or	ax, di			;Q: Does VMD have API entry point?
	jz	copy_mouse_routines	;   N: Done

	push	cs			;Return to here after call to Win386
	mov	bx, offset vmd_call_done;virtual mouse driver
	push	bx
	push	es			;Call this SEG:OFF by doing a far
	push	di			;return

	mov	ax, 100h		;Set mouse type & int VECTOR API call
	mov	bl, mouse_type
	mov	bh, vector

BogusFarRetProc PROC FAR
	ret				;"Return" to VMD's API entry point
BogusFarRetProc ENDP

vmd_call_done:
copy_mouse_routines:
	pop	es
	pop	di
	pop	bx
	pop	ax


	or	mouse_flags,MF_MOUSE_EXISTS

resize_ds:
	dec	si

;	Claims to resize the data segment, but who cares anymore?

	mov	ax,1			;Successful initialization

cEnd
page

;-----------------------------------------------------------------------;
;	bogus_far_ret is a far return instruction which can be
;	passed off to the INT 33h mouse detection logic routine.
;-----------------------------------------------------------------------;

bogus_far_ret	proc	far

	ret

bogus_far_ret	endp

sEnd	Code
end	Initialize
