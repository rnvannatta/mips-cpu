# mips-cpu
Pipelined MIPS I CPU in VHDL with hazard resolution, based on design in Harris &amp; Harris' Digital Design

It has a simple memory map of 64 32bit words of memory, with the top word being mapped to the LEDs on the front of the display. The reset, tied to a button, is asynchronous assert, synchronous deassert. I verified this with ad-hoc testing and waveform analysis, and ran this successfuly on an Arty S7 development board. There is no branch or load delay slot.
