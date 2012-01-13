"""
    Computer definition for the HCS08 microprocessor.
"""

import sys
sys.path.append('..');

import random
import inspect

import Instructions

from utils import merge_word, split_word
from Computers.Definition import Computer
from Computers.Exceptions import UserCodeException, ExecutionException
from Computers.Freescale.Assembly import Assembler

class InvalidOpcodeException(ExecutionException):
    """
        Exception which is thrown when the CPU tries to evaluate an opcode for which no operation class exists.
    """
    pass

class InvalidMemoryException(ExecutionException):
    """
        Exception which is thrown if the CPU attempts read from or write to non-existant memory.
    """
    pass


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

    V = False
    """ Signed overflow.  """

    H = False
    """ Half carry. """

    I = False
    """ Interrupt Mask """

    N = False
    """ Negative."""

    Z = False
    """ Zero flag. """

    C = False
    """ Carry flag. """

    #
    # The HCS08's RAM and Flash.
    #
    ram = None
    """ The device's RAM. """

    flash = None
    """ The device's read-only flash memory."""

    #
    # Shortcuts to registers, and relevant RAM addresses.
    #

    REGISTERS = ('A', 'X', 'H', 'SP', 'PC')
    """ Convenience 'contsant', which contains the names of each of the device's registers. Can be used with self.__dict__ to iterate over each of the registers."""

    FLAGS = ('V', 'H', 'I', 'N', 'Z', 'C')
    """ Convenience 'contsant', which contains the names of each of the device's flags. Can be used with self.__dict__ to iterate over each of the flags."""


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
            self.load_program(code)

    def initialize_registers(self):
        """
            Initializes each of the registers to a random value, simulating a true device startup.
        """

        #for each of the register's _names_
        for name in self.REGISTERS:

            #set that register's contents to a random byte value, using "reflection"
            self.__dict__[name] = random.randrange(0, 256)

    def initialize_flags(self):
        """
            Initializes each of the flags to either on or off randomly, simulating a true device startup.
        """

        #for each of the flags's _names_
        for name in self.FLAGS:

            #set that flag's contents to a random byte value, using "reflection"
            self.__dict__[name] = random.randrange(0, 2)


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



    def load_program(self, code):
        """
            Convenince method, which calls the assemler on the given block of flash.
        """

        #create a new assembler targeting this device's flash
        asm = Assembler(self.flash)

        #and use it to assemble the given code
        asm.process_code(code)

        #reset the microcontroller
        self.reset()


    def reset(self):
        """
            Resets the microcontroller to its power-on state.
            Does not alter the contents of flash memory.
        """

        #restore the device's RAM, registers, and flags to a power-on state
        self.initialize_ram()
        self.initialize_registers()
        self.initialize_flags()

        #set the program counter to the start of flash
        self.PC = min(self.flash)

        #set the stack pointer to the end of flash
        self.SP = max(self.flash)


    def step(self):
        """
            Single steps (executes a single operation).
        """

        #get the opcode at the address specified by the PC; incrementing the PC in the process
        opcode = self.get_current_opcode()

        #get the instruction object for the given opcode, and addressing mode it represents
        instruction, address_mode = self.get_instruction_from_opcode(opcode)

        #read the operand
        operand = instruction.read_operand(address_mode, self)

        #and execute the instruction in question
        instruction.execute(address_mode, self, operand)


    def get_HX(self):
        """
            Returns the value of HX (the system indexing register) as a 16-bit word.
        """

        #merge H and X to form HX, the 16-bit indexing register
        return merge_byte(self.H, self.X)

    def set_HX(self, value):
        """
            Sets the value of the H and X registers as a single 16-bit unit.
        """

        #split the value into a MSB and LSB, and assign those to H and X respectively
        self.H, self.X = split_word(value)


    def set_ram_byte(self, addr, value):
        """
            Sets a value in RAM, with checks. Requires ram to be initialized.
        """

        #if the address is not within initialized ram
        if addr not in self.ram:
            raise InvalidMemoryException('Tried to write to address ' + repr(addr) + ', which does not exist in valid RAM.')

        #set the value
        self.ram[addr] = value % 256

    def set_ram_word(self, addr, word):
        """
            Sets a word in RAM, with checks. Requires ram to be initialized.
        """

        #split the word into two seperate bytes
        msb, lsb = split_word(word)

        #and write each of the two bytes, in big endian order:
        self.set_ram_byte(addr, msb)
        self.set_ram_byte(addr + 1, lsb)


    def set_by_identifier(self, identifier, value, is_word=False):
        """
            Sets a register or RAM value by its name, or address.

            For example, set_by_identifier('A', 12) would set the A register to 12,
            and set_by_identifier(0, 12) would set ram[0] to 12.
        """

        #if the identifier is an integer, use it as a RAM address
        if isinstance(identifier, int):

            #if we've been told the value is a word, set two bytes of RAM
            if is_word:
                self.set_ram_word(identifier, value)

            #otherwise, set one, truncating if necessary
            else:
                self.set_ram_byte(identifier, value)

        #if the identifer is a register name, set that register's value
        elif identifier in self.REGISTERS:
            self.__dict__[identifier] = value


    def get_by_identifier(self, identifier, is_word=False):
        """
            Gets the value of a register, Flash, or RAM by its name, or address.

            For example, get_by_identifier('A') returns the value in the accumulator,
            and get_by_identifier(0x01) returns the value in ram[1].
        """

        #if the identifier is an integer, use it as a memory address
        if isinstance(identifier, int):

            #if the address is in RAM, return the appropriate value
            if identifier in self.ram:
                return self.ram[identifier]

            #do the same for an address in flash
            elif identifier in self.flash:
                return self.flash[identifier]

            #if the address was neither in flash nor RAM, throw an exception
            else:
                raise InvalidMemoryException('One of your instructions tried to read from address ' + repr(identifier) + ', which isn\'t a valid memory address.')

        #if the identifier is a register name, return the register's contents
        elif identifier in self.REGISTERS:
            return self.__dict__[identifier]

        else:
            raise ExecutionException('The CPU tried to read from a non-existant register ' + repr(identifier) + '. This is likely an issue with the simulator; please bring this error to an instructor\'s attention.')


    def update_NZ(self):
        """
            Update the N (negative) and Z (zero) flags according to the current accumulator value.
        """
        self.N = (self.A > 127)
        self.Z = (self.A == 0)


    def fetch_byte(self):
        """
            Fetches the byte at the address contained in the system's program counter, then increments the program counter.
        """

        #get the byte which the program counter points to
        byte = self.flash[self.PC]

        #increment the program counter
        self.PC += 1

        #and return the byte
        return byte

    def fetch_word(self):
       """
            Fetches the word at the address contained in the system's program counter, then adds two to the program counter, so it points past the end of the read word.
       """

       #fetch two bytes, then merge them into a word
       return merge_word(self.fetch_byte(), self.fetch_byte())



    def get_current_opcode(self):
        """
            Returns the opcode at the address contained by the system's program counter, and increments the program counter to point past the opcode.
        """

        #gets the first (and typically only) byte of the opcode
        opcode = [self.fetch_byte()]

        #if the first byte of the opcode is 9E, we have a two-byte opcode; get the second byte
        if opcode[0] == 0x9E:
            opcode.append(self.fetch_byte())

        #return the complete opcode
        return opcode



    def get_instruction_from_opcode(self, opcode):
        """
            Retrieves the HCS08_Instruction class which assembles to the given opcode.

            Returns a 2-tuple in the form of (HCS08_Instruction object, addressing mode).
        """

        #for each defined operation
        for _, instruction in inspect.getmembers(Instructions, lambda x : inspect.isclass(x)):

            #if the given instruction is a HCS08 operation represented by the given mnemonic
            if issubclass(instruction, Instructions.HCS08_Operation) and opcode in instruction.machine_codes.values():

                #determine the addressing mode of the instruction by using dictionary reverse-lookup
                #(e.g. look for the first dictionary key with a value equal to the opcode; we use a generator
                addressing_mode = (key for key in instruction.machine_codes if instruction.machine_codes[key] == opcode).next()

                #return the class
                return instruction, addressing_mode


        #if we didn't find a class, raise an InvalidOpcodeException
        raise InvalidOpcodeException('The microcontroller attempted to execute the opcode ' + repr(opcode) + ' which does not correspond to a valid instruction. Did the execution "flow" run past the end of your program?')



def get_operation_by_mnemonic(mnemonic):

    #for each defined operation
    for _, instruction in inspect.getmembers(Instructions, lambda x : inspect.isclass(x)):

        #if the given instruction is a HCS08 operation represented by the given mnemonic
        if issubclass(instruction, Instructions.HCS08_Operation) and mnemonic.lower() in instruction.mnemonics:

            #return the class
            return instruction


class MC9S08QG8(HCS08):
    """
        Device-specific mappings for the MC8S08QG8.
    """

    memory_map = \
        {
            'ram': [ {'start': 0x0000, 'end': 0x025F}, {'start': 0x1800, 'end': 0x1850} ],
            'flash': [ {'start': 0xE000, 'end': 0xFFFF } ]
        }
