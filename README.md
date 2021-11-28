# VMware mouse driver for Windows 3.x

Running Windows 3.1 in VMware (or seemingly, QEMU, but it's not yet tested),
but annoyed by having to grab and ungrab the cursor manually?

Wish you could just move the cursor in and out like a modern OS (one with USB
tablet support or VMware Tools drivers), with no Ctrl+Alt dancing?

With this driver, now you can. It implements the interface that VMware uses
(the [backdoor][1]), replacing the existing PS/2 mouse driver.

https://user-images.githubusercontent.com/3161292/143529005-8bedce21-109d-43d1-96c9-8c958a613bd6.mp4

## Well, how does it work?

Glad you asked!

Normally, mice work by sending a delta of their movements. You'd have to trap
the mouse inside of the guest for this to work; any tracking difference would
result in a very hard to control cursor. Being able to send the absolute
coordinates would be great, because you can know the exact point when the
cursor hits the edge.

However, there wasn't a standard way of doing absolute positioning with PC
input devices until the USB tablet standard, and Windows 3.x/DOS massively
predate USB, let alone have a USB stack. For those situations, VMware offers
absolute positioning through a port I/O interface.

So let's use [said interface][2]. What we need to do is threefold for an MVP:

1. On initialization, make the cursor absolute (four calls)
2. On deinitialization, make the cursor relative (a single call)
3. Instead of parsing PS/2 mouse events, ask VMware for mouse events instead
4. Well, we should check if we're even using a supported host, but hey...

The challenge is doing this from real or 286 protected mode, because we need to
set the 32-bit extended versions of the registers (i.e. `EAX` instead of `AX`).
The toolchain we're using is MASM, since we're keeping this easy and using the
example drivers from the DDK, just modified. This means we can just plop the
`.386` directive in a suitable place in the code, and `EAX` will become
available to use.

One interesting bit with the mouse driver is the `SF_ABSOLUTE` bit in a Windows
mouse driver. When passed as the flags for the mouse event (in `AX`), it'll be
an absolute position instead of relative - exactly what we want, without any
trickery! Even better, it takes a range of 0 through FFFFh, as basically a
percentage of where it is on the screen, in `BX` and `CX`. This way, you don't
need to know the resolution of the screen when calculating the absolute
position. Turns out this is exactly how VMware sends the coordinates in `EBX`
and `ECX`!

The unfortunate difference is in button handling; VMware sends what buttons
are currently held, while Windows wants to know when the button goes down and
when it comes up. I've implemented a crude solution, but I think it could be
done a lot better. Right now, only two buttons are supported, but in theory,
we could send a third with a refactor to the driver. (We're also throwing away
the wheel event - that could be four and five, considering Windows 3.x predates
the wheel entirely.)

One annoying thing about the sample driver is because the fact it supports
multiple types of mouse, it uses a tactic of copying the interrupt handler into
a specifically sized buffer. This means you can't go over 210 bytes for the
interrupt handler right now; this could be alleviated with a major refactor or
rewrite. For now, I've excised the normal PS/2 mouse handling in favour of only
using the VMware backdoor. I've also had to occasionally be worried about the
length of instructions; shaving things off by only using the 16-bit view of a
register, for example.

Overall, I'm glad this was surprisingly easy, considering I didn't know x86
assembly before, and I only implemented this in a day - with lots of struggling
against MASM and typos.

## Supported hosts

Only VMware is tested. QEMU allegedly implements VMware's mouse interface, but
I haven't tested it.

## Building from source

Make sure you have the [Windows 3.1 Device Development Kit][3] installed.

Inspect the values in `SETUPENV.BAT` and `INSTALL.BAT`, then run:

```
setupenv.bat
nmake
```

## Installation

If building from source; after building, run `INSTALL.BAT`.

If using binaries, run Windows Setup. Point where the driver and INF file are;
if you're using the floppy, it'll likely be `A:`. Restart Windows and enjoy.
Serve with garnish.

Note that if you're using the GUI version of Windows Setup, after selecting
the VMware mouse driver for the first time, the combo box might go blank. If
that's the case, you need to select it again. This doesn't happen from the DOS
version of Windows Setup (when setup is run outside of Windows or at install).

[1]: https://wiki.osdev.org/VMware_tools
[2]: https://wiki.osdev.org/VMware_tools#Absolute_Mouse_Coordinates
[3]: https://winworldpc.com/download/3d0639c3-9e18-c39a-11c3-a4e284a2c3a5
