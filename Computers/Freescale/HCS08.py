"""
    Computer definition for the HCS08 microprocessor.
"""

import sys
sys.path.append('..');

import random
from Computers.Definition import Computer
from Computers.Freescale.Assembly import Assembler

class HCS08(Computer):
    """
        Abstract class representing the HCS08 microprocessor as a computational engine.
    """

    #
    # The HCS08's core registers
    #

    A = 0
    """ The microprocessor's accumulator."""

    X = 0
    """ The microprocessor's index register (low byte.)"""

    H = 0
    """ The microprocessor's index register (high byte.)"""

    SP = 0
    """ The stack pointer. """

    PC = 0
    """ The program counter. """

    #
    # The HCS08's flags
    #

    V = 0
    """ Signed overflow.  """

    H = 0
    """ Half carry. """

    I = 0
    """ Interrupt Mask """

    N = 0
    """ Negative."""

    Z = 0
    """ Zero flag. """

    C = 0
    """ Carry flag. """

    #
    # The HCS08's RAM and Flash.
    #
    ram = None
    """ The device's RAM. """

    flash = None
    """ The device's read-only flash memory."""


    def __init__(self, code=None):
        """
            Initializes a new HCS08 microprocessor.
        """

        #initialize the device's RAM
        self.initialize_ram()

        #initialize the contents of the device's flash
        self.initialize_flash()

        #if code was provided, use it to fill the flash
        if code is not None:
            asm = Assembler(self.flash)
            asm.process_code(code)

    def initialize_ram(self):
        """
            Initialializes the RAM to random values, simulating a true device startup.
        """

        self.ram = {}

        #for each valid block of RAM:
        for block in self.memory_map['ram']:

            #initialize each byte to a random value
            for i in range(block['start'], block['end']+1):

                #simulate a random byte value
                self.ram[i] = random.randrange(0, 256)

    def initialize_flash(self):
        """
            Initialializes the FLASH to zero, creating an initial state for the assembler.
        """

        self.flash = {}

        #for each valid block of flash:
        for block in self.memory_map['flash']:

            #initialize each byte to zero
            for i in range( block['start'], block['end'] +1):

                #simulate a random byte value
                self.flash[i] = 0

    def load_program(code):
        """
            Convenince method, which calls the assemler on the given block of flash.
        """
        pass



class MC9S08QG8(HCS08):
    """
        Device-specific mappings for the MC8S08QG8.
    """

    memory_map = \
        {
            'ram': [ {'start': 0x0000, 'end': 0x025F}, {'start': 0x1800, 'end': 0x1850} ],
            'flash': [ {'start': 0xE000, 'end': 0xFFFF } ]
        }
