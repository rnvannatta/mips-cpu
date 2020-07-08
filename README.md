# mips-cpu
Pipelined MIPS I CPU in VHDL with hazard resolution, based on design in Harris &amp; Harris' Digital Design

The main file to look at is pipeline.vhd

It has a simple memory map of 64 32bit words of memory, with the top word being mapped to the LEDs on the front of the display. The reset, tied to a button, is asynchronous assert, synchronous deassert. I verified this with ad-hoc testing and waveform analysis, and ran this successfuly on an Arty S7 development board. There is no branch or load delay slot.

You may be wondering about the lack of record types. The book I used didn't mention them, and googling around brought up information that made me not want to try them out, such as lack of generic support (It's in VHDL 2008, which is poorly supported) and general pain. But I'll probably go back and try them out anyway.
