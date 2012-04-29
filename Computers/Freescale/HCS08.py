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

    HC = False
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
    # Internal status registers, which inidicate the CPU's progress.
    #

    _halted = False
    """ True iff the CPU has been halted; indicates that the CPU should stop executing the current program, if one is running. """

    Cycles = 0
    """ The amount of CPU cycles since the last device reset. """

    cycle_limit = float("inf")
    """ Initially, allow an unlimited amount of execution cycles. """

    #
    # Shortcuts to registers, and relevant RAM addresses.
    #

    REGISTERS = ('A', 'X', 'H', 'SP', 'PC')
    """ Convenience 'contsant', which contains the names of each of the device's registers. Can be used with self.__dict__ to iterate over each of the registers."""

    FLAGS = ('V', 'HC', 'I', 'N', 'Z', 'C')
    """ Convenience 'contsant', which contains the names of each of the device's flags. Can be used with self.__dict__ to iterate over each of the flags."""

    EXPOSED_RAM = tuple(range(0x80, 0x90)) #DEBUG ONLY: make larger later
    """ Contains a range of RAM addresses will be returned during state serialization. """

    EXPOSED_PROPERTIES = ('Cycles',)
    """ Contains a list of properties which will be returned during state serialization. """

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



    def load_program(self, code, blacklist = None, whitelist = None, required = None):
        """
           Loads a given program into program memory.
        """

        #reset the device's flash to its original state
        self.initialize_flash()

        #create a new assembler targeting this device's flash
        asm = Assembler(self.flash)

        #if an instruction blacklist has been provided, pass it to the assembler
        if blacklist:
            asm.set_blacklist(blacklist)

        #if an instruction whitelist has been provided, pass it to the assembler
        if whitelist:
            asm.set_whitelist(whitelist)

        #if a list of required instructions was provided, use it
        if required:
            asm.set_required(required)

        #and use it to assemble the given code
        asm.process_code(code, terminate_with_stop=True, enforce_required=True)

        #store the symbol list from the assembler, for use with breakpoints
        self.symbols = asm.symbols

        #store the symbol list from the assembler, for use with breakpoints
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

        #reset the list of altered RAM
        self.altered_ram = []
        self.altered_registers = []

        #and take a snapshot of the initial register and flag state
        self._initial_registers = self._register_snapshot()

        #reset the executed cycle count
        self.Cycles = 0

        #set the program counter to the start of flash
        self.PC = min(self.flash)

        #set the stack pointer to the end of RAM
        #self.SP = max(self.ram)
        self.SP = self.memory_map['ram'][0]['end'];

        #ensure the CPU isn't halted
        self._halted = False

    def step(self):
        """
            Single steps (executes a single operation).

            This will perform a single instruction, even if the CPU is halted.
        """

        #get the opcode at the address specified by the PC; incrementing the PC in the process
        opcode = self.get_current_opcode()

        #get the instruction object for the given opcode, and addressing mode it represents
        instruction, address_mode = self.get_instruction_from_opcode(opcode)

        #read the operand
        operand = instruction.read_operand(address_mode, self)

        #and execute the instruction in question
        instruction.execute(address_mode, self, operand)

        #TODO: increase cycle count accurately
        self.Cycles += 1

        #if we've exceeded our runtime limit, halt the CPU
        if self.Cycles > self.cycle_limit:
            self.halt()



    def halt(self):
        """
            Stops the CPU mid-execution by setting the halted flag to true.
            The CPU can be restarted by calling resume() or reset().
        """
        self._halted = True


    def resume(self):
        """
            Resumes opreation of the CPU by setting the halted flag to false.
        """
        self._halted = False

    def halted(self):
        """
            Returns true iff the CPU has been halted.
        """
        return self._halted


    def single_step(self):
        """
            Alias for step().
        """
        self.step()


    def run(self):
        """
            Runs until a STOP instruction (or undefined memory) is encountered.
        """
        self.run(lambda x : False)


    def run_until(self, condition):
        """
            Runs until a given condition evaluates to false. Condition is a function or lambda which accepts the CPU as its argument.
        """

        #while the stop condition is _not_ true, and the CPU is not halted, run
        while not condition(self) and not self._halted:
            self.step()


    def get_CCR(self):
        """
            Returns a byte which contains all of the system's flags as one contiguous Condition Code Register.
        """

        #start off with 0b01100000, as the two reserved bits are always 1
        ccr = 96

        #and OR in each of the flags, according to their place
        ccr |= self.V << 7
        ccr |= self.H << 4
        ccr |= self.I << 3
        ccr |= self.N << 2
        ccr |= self.Z << 1
        ccr |= self.C

        #return the newly constructed CCR
        return ccr

    def set_CCR(self, ccr):
        """
            Sets each of the flags using a single-byte condition code register.
        """

        self.V = bool(ccr & (1 << 7))
        self.H = bool(ccr & (1 << 4))
        self.I = bool(ccr & (1 << 3))
        self.N = bool(ccr & (1 << 2))
        self.Z = bool(ccr & (1 << 1))
        self.C = bool(ccr & 1)


    def get_HX(self):
        """
            Returns the value of HX (the system indexing register) as a 16-bit word.
        """

        #merge H and X to form HX, the 16-bit indexing register
        return merge_word(self.H, self.X)

    def set_HX(self, value):
        """
            Sets the value of the H and X registers as a single 16-bit unit.
        """

        #split the value into a MSB and LSB, and assign those to H and X respectively
        self.H, self.X = split_word(value)

    def set_flash_byte(self, addr, value):
        """
            Sets a value in flash, with checks. Requries flash to be initialized.
        """

        #if the address is not within initialized flash
        if addr not in self.flash:
            raise InvalidMemoryException('Tried to write address' + repr(addr) + ', which does not exist in valid flash.');

        #set the value
        self.flash[addr] = value % 256;

    def set_flash_word(self, addr, word):
        """
            Sets a word in flash, with checks. Requires flash to be initialized.
        """

        #split the word into two seperate bytes
        msb, lsb = split_word(word)

        #and write each of the two bytes, in big endian order:
        self.set_flash_byte(addr, msb)
        self.set_flash_byte(addr + 1, lsb)


    def set_ram_byte(self, addr, value):
        """
            Sets a value in RAM, with checks. Requires ram to be initialized.
        """

        #if the address is not within initialized ram
        if addr not in self.ram:
            raise InvalidMemoryException('Tried to write to address ' + repr(addr) + ', which does not exist in valid RAM.')
        #if we've yet to mark this address as altered
        if addr not in self.altered_ram:
            self.altered_ram.append(addr)

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


    def set_by_identifier(self, identifier, value, is_word=False, allow_flash=False):
        """
            Sets a register or RAM value by its name, or address.

            For example, set_by_identifier('A', 12) would set the A register to 12,
            and set_by_identifier(0, 12) would set ram[0] to 12.
        """

        #always work with words for word-size registers
        if identifier in ('SP', 'PC'):
            is_word = True

        #if the identifier is the full HX register, return H merged with X
        if identifier == 'HX':
            self.set_HX(value)

        #if we're in a mode which allows writes to flash, and we're trying to set a flash value
        elif isinstance(identifier, int) and (identifier in self.flash) and allow_flash:

            #if we've been told the value is a word, set two bytes of flash
            if is_word:
                self.set_flash_word(identifier, value)

            #otherwise, set one, truncating if necessary
            else:
                self.set_flash_byte(identifier, value)

        #if the identifier is an integer, use it as a RAM address
        elif isinstance(identifier, int):

            #if we've been told the value is a word, set two bytes of RAM
            if is_word:
                self.set_ram_word(identifier, value)

            #otherwise, set one, truncating if necessary
            else:
                self.set_ram_byte(identifier, value)

        #if the identifer is a register name, set that register's value
        elif identifier in self.REGISTERS or identifier in self.FLAGS:

            if is_word:
               self.__dict__[identifier] = value % 0x10000
            else:
               self.__dict__[identifier] = value % 0x100


    def get_by_identifier(self, identifier, is_word=False):
        """
            Gets the value of a register, Flash, or RAM by its name, or address.

            For example, get_by_identifier('A') returns the value in the accumulator,
            and get_by_identifier(0x01) returns the value in ram[1].
        """

        #assume we're working with words for word-size registers
        if identifier in ('SP', 'PC'):
            is_word = True

        #if we've been instructed to retrieve the operator HX, then do so
        if identifier == 'HX':
            return self.get_HX()

        #if the identifier is an integer, use it as a memory address
        elif isinstance(identifier, int):

            #if the address is in RAM, return the appropriate value
            if identifier in self.ram:

                #if we've been asked to retrieve a word, do so
                if is_word:
                    return merge_word(self.ram[identifier], self.ram[identifier + 1])

                #otherwise, return a byte
                else:
                    return self.ram[identifier]

            #do the same for an address in flash
            elif identifier in self.flash:

                #if we've been asked to retrieve a word, do so
                if is_word:
                    return merge_word(self.flash[identifier], self.flash[identifier + 1])

                #otherwise, return a byte
                else:
                    return self.flash[identifier]

            #if the address was neither in flash nor RAM, throw an exception
            else:
                raise InvalidMemoryException('One of your instructions tried to read from address ' + repr(identifier) + ', which isn\'t a valid memory address.')

        #if the identifier is a register name, return the register's contents
        elif identifier in self.REGISTERS or identifier in self.FLAGS or identifier in self.EXPOSED_PROPERTIES:
            return self.__dict__[identifier]

        else:
            raise ExecutionException('The CPU tried to read from a non-existant register ' + repr(identifier) + '. This is likely an issue with the simulator; please bring this error to an instructor\'s attention.')


    def update_NZ(self, V = None):
        """
            Update the N (negative) and Z (zero) flags according to the current accumulator value.

            If clear_V has been set, the V flag will be cleared, as well.
        """
        self.N = (self.A > 127)
        self.Z = (self.A == 0)

        #if a value for V was specified as well, use it
        if V is not None:
            self.V = V


    def fetch_byte(self):
        """
            Fetches the byte at the address contained in the system's program counter, then increments the program counter.
        """

        #get the byte which the program counter points to
        try:
            byte = self.flash[self.PC]
        except KeyError:
            raise InvalidMemoryException('Tried to execute the byte at address ' + repr(self.PC) + ', which does not exist in valid flash.');

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

    def push_byte(self, byte):
        """
            Pushes a given byte onto the stack.
        """

        #set the value pointed to by the stack pointer
        self.set_by_identifier(self.SP, byte)

        #and grow the SP (the stack grows downward from the end of RAM)
        self.SP -= 1

    def push_word(self, word):
        """
            Pushes a given word onto the stack.
        """

        #split the word into two bytes
        msb, lsb = split_word(word)

        #then, push those bytes- pushing the LSB first, so the bytes remain in big endian order
        self.push_byte(lsb)
        self.push_byte(msb)

    def pull_byte(self):
        """
            Pulls a byte from the stack, and returns it.
        """

        #shrink the stack
        self.SP += 1

        #and return the value it points to
        return self.get_by_identifier(self.SP)

    def pull_word(self):
        """
            Pulls a word from the stack, and returns it.
        """

        #pull two bytes from the stack
        msb = self.pull_byte()
        lsb = self.pull_byte()

        #and merge them into a single word
        return merge_word(msb, lsb)


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

    def list_changes(self):
        """
            Returns a list of changes since the device reset.
        """

        #create a list of all registers which have changed since system initialization
        changedlist = self._registers_changed_since_snapshot(self._initial_registers)

        #and append any RAM that has been altered
        changedlist.extend([repr(x) for x in self.altered_ram])

        #if changes have occured, list them
        if changedlist:
            return '|'.join(changedlist)
        
        #otherwise, return a dash, indicating that nothing was altered
        else:
            return '-'


    def _register_snapshot(self):
        """
            Returns a dictionary which holds a snapshot of the device's registers (and flags).
        """

        snapshot = {}

        #add each register to the snapshot
        for identifier in self.REGISTERS + self.FLAGS:
            snapshot[identifier] = self.get_by_identifier(identifier)

        return snapshot

    def _registers_changed_since_snapshot(self, snapshot):
        """
            Returns a list of registers changed since the last snapshot.
        """
        
        #start an empty list of changed registers
        changed = []

        #for each register in the snapshot
        for register in snapshot:

            #if the register's value has changed
            if snapshot[register] != self.get_by_identifier(register):
                
                #add it to the list of changed regitsers
                changed.append(register)

        #and return the changedlist
        return changed



    def serialize_state(self, readable=False, all_inclusive=False):
        """
            Serializes the state into a format which can be easily exchanged with other programs.
        """

        buf = []

        #if the user has request the full RAM be serialized, do so
        if all_inclusive:
            desired_ram = tuple(self.ram.keys())
            desired_flash = tuple(self.flash.keys())

        #otherwise, use only the visible RAM defined by the EXPOSED_RAM constant
        else:
            desired_ram = self.EXPOSED_RAM + tuple(self.altered_ram)
            desired_flash = tuple()

        #for each of the microprocessor areas queued for serialization
        for identifier in self.REGISTERS + self.FLAGS + desired_ram + desired_flash + self.EXPOSED_PROPERTIES:

            #read the value at the given location
            value = self.get_by_identifier(identifier)
            value = int(value)

            #if the readable flag is set, add a readable version of the item to the buffer
            if readable:
                buf.append('{id:>4} | ${value:0=2x}'.format(id=identifier, value=value) + "\n")

            #otherwise, serialize using a minimal-space format
            else:
                buf.append('{id}={value}&'.format(id=identifier, value=value))

        #return the completed buffer
        return ''.join(buf)

    def unserialize_state(self, serial):
        """
            Reads state data from a serialized state, overriding the current state of the microprocessor.

            State elements may be provided selectively; for example, this method can be used to only override the value of A.
        """

        #split the serialized data into a list of term definitions
        terms = serial.split('&')

        #for each term
        for term in terms:

            #split the term into its name and value
            identifier, _, value = term.partition('=')

            #try to convert the identifier into an int; which may not always be possible
            #(if it isn't possible, this doesn't mean anything in particular is wrong; for example, 'A' won't convert to an integer properly, but references the accumulator)
            try:
                identifier = int(identifier)
            except:
                pass

            #parse the value- this conversion _is_ required- otherwise we were handed invalid state
            try:
                value = int(value)
            except TypeError:
                raise UserCodeException('An inappropriate value was provided for ' + identifier + ' by the grading routine provided by your instructor. Please bring this to your instructor\'s attention.')

            #if a valid identifier was provided, adjust the state accordingly
            if identifier or (identifier is 0):
                self.set_by_identifier(identifier, value, is_word=False, allow_flash=True)


    def handle_system_command(self, command):
        """
            Handles sytem-specific commands.
        """

        #seperate the argument from the command
        command, _, argument = command.partition(' ')

        #if a flash dump has been requested, dump the contents of flash
        if command in ('dumpflash', 'flash', 'flashdump'):
            print [hex(self.flash[i]) for i in self.flash]

        #handle the 'symbols' instruction by printing the symbol list
        elif command in ('symbol', 'symbols'):
            print {i : hex(self.symbols[i]) for i in self.symbols}


    def limit_runtime(self, max_runtime):
        """
            Sets a cap on the total runtime, in bus cycles.
        """
        self.cycle_limit = max_runtime


    def __repr__(self):
        """
            Prints a human-readable version of the given CPU.
        """
        return self.device_name() + "\n" + self.serialize_state(readable=True)

    @staticmethod
    def shorthand():
        return 'HCS08'


class MC9S08QG8(HCS08):
    """
        Device-specific mappings for the MC8S08QG8.
    """

    memory_map = \
        {
            'ram': [ {'start': 0x0000, 'end': 0x025F}, {'start': 0x1800, 'end': 0x184F} ],
            'flash': [ {'start': 0xE000, 'end': 0xFFFF } ]
        }

    @staticmethod
    def device_name():
        return 'Freescale Semicondcutors MC9S08QG8 (Simulated)'


class MC9S08QG4(HCS08):
    """
        Device-specific mappings for the MC8S08QG8.
    """

    memory_map = \
        {
            'ram': [ {'start': 0x0000, 'end': 0x015F}, {'start': 0x1800, 'end': 0x184F} ],
            'flash': [ {'start': 0xF000, 'end': 0xFFFF } ]
        }

    @staticmethod
    def device_name():
        return 'Freescale Semicondcutors MC9S08QG4 (Simulated)'

