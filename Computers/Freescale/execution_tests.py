#!/usr/bin/python2

import sys, inspect

import Assembly
from HCS08 import MC9S08QG8 as uProc


def test_ADC():

    # ***Step 1: Create a new CPU, preloaded with your program.
    cpu = uProc("ADC #$0F")

    #  ***Step 2: Set up the CPU however you'd like. Some notes that may help:
    #
    # -You can access registers and flags directly ("cpu.A = 12", "cpu.C = True")
    # -The ram is a dictionary ("associative array" or set of name/value pairs) which maps an address to a value.
    #  You can access address $FF via cpu.ram[0xFF], like so "cpu.ram[0xFF] = 13".
    # -The flash is organized the same way, but is called cpu.flash.
    #
    # For more information, view the contents of HCS08.py.

    cpu.A = 0x0F
    cpu.C = True

    # ***Step 3: Run the instruction (or instructions). In this case, we'll single step, executing the first instruciton specified.
    cpu.step()

    # ***Step 4: Verify that the instruction acted as expected.
    assert cpu.A == 0x1F, "ADC instruction did not set A correctly. Should have been 31, was " + repr(cpu.A)
    assert cpu.C == False, "C was set inappropriately."
    assert cpu.V == False, "V was set inappropriately."
    assert cpu.H == True, "H was set inappropriately."
    assert cpu.N == False, "N was set inappopriately."
    assert cpu.Z == False, "Z was set inappropriately."


#The following launches all of the contained tests.
#It should remain the last entry in the module.

#if this script was launched directly
if __name__ == '__main__':

    #for each test function define in the module
    for _, test in inspect.getmembers(sys.modules[__name__], inspect.isfunction):

        #execute the function
        test()
