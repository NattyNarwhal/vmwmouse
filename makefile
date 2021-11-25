#   Microsoft Mouse Driver
#
#   Requirements:
#
#	MASM 4.01 or greater with the environment variable INCLUDE set to
#	the directories containing CMACROS.INC, and WINDEFS.INC.
#
#	MASM 4.00 or greater with the ASM inference definition changed to
#	include the directories containing CMACROS.INC, and WINDEFS.INC.


#   Options:
#
#	The command line may define options to MASM by defining the OPT
#	macro.	By defining the OPT parameter in the make file, any
#	possible interaction with an environment definition is avoided.

OPT = -l				    #NOP the options feature


#   Define the default assemble command.  This command could actually
#   be overridden from the command line, but shouldn't be.

ASM = masm -v -ML  $(OPT)					# MASM 4.01 & >
#   ASM = masm -v -ML  $(OPT) -I\finc				# MASM 4.00


#   Define the default inference rules

.asm.obj:
	$(ASM) $*,$@;

mouse:  mouse.drv

mouse.obj:	mouse.asm mouse.inc

bus8255.obj:	bus8255.asm mouse.inc

ser8250.obj:	ser8250.asm mouse.inc ins8250.inc

inport.obj:	inport.asm mouse.inc inport.inc

ps2.obj:	ps2.asm mouse.inc

int33h.obj:	int33h.asm mouse.inc

mouse.drv:	mouse.def mouse.obj ser8250.obj 	\
		inport.obj bus8255.obj ps2.obj int33h.obj
      link @mouse.lnk
      rc mouse.drv
      mapsym mouse
