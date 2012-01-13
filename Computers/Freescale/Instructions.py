"""
    Class objects which represent an instruction on a HCS08 microprocessor.

    Copyright (c) 2012, Binghamton University
    Kyle Temkin <ktemkin@binghamton.edu>

"""

from Computers.Exceptions import UserCodeException, ExecutionException
from utils import parse_number, parse_operand, split_word, is_numeric, fit_to_size, merge_word, sign_extend


def _machine_codes_standard(suffix, include_imm=True):

    #return the standard set of machine codes for the instruction with the given suffix
    machine_codes = \
            {
                'dir': [0xB0 + suffix],
                'ext': [0xC0 + suffix],
                'ix2': [0xD0 + suffix],
                'ix1': [0xE0 + suffix],
                'ix' : [0xF0 + suffix],
                'sp2': [0x9E, 0xD0 + suffix],
                'sp1': [0x9E, 0xE0 + suffix],
            }

    #if we've been instructed to include the immediate addresing mode, include it
    if include_imm:
        machine_codes['imm'] = [0xA0 + suffix]

    #return the newly created machinec odes
    return machine_codes


def _machine_codes_axh_inherent(suffix, h_suffix = None):
    """
        Helper method which generates a normal set of machine codes for AXH inherent values.
    """

    #create the default array of machine codes
    machine_codes = \
            {
                'dir':  [0x30 + suffix],
                'inha': [0x40 + suffix],
                'inhx': [0x50 + suffix],
                'ix1':  [0x60 + suffix],
                'ix' :  [0x70 + suffix],
                'sp1':  [0x9E, 0x60 + suffix]
            }

    #if a suffix for inherent-h mode is provided, use it
    if h_suffix is not None:
        machine_codes['inhh'] = [0x80 + h_suffix]

    #return the newly generated machine codes
    return machine_codes

def _machine_codes_bit_operation(prefix, is_clear):

    #clear expressions start with a suffix of 1, set of 0
    suffix = 1 if is_clear else 0

    #and return the set of machine codes for a bit operation
    return \
            {
                'dirb0': [(0x10 * prefix) + suffix],
                'dirb1': [(0x10 * prefix) + suffix + 2],
                'dirb2': [(0x10 * prefix) + suffix + 4],
                'dirb3': [(0x10 * prefix) + suffix + 6],
                'dirb4': [(0x10 * prefix) + suffix + 8],
                'dirb5': [(0x10 * prefix) + suffix + 10],
                'dirb6': [(0x10 * prefix) + suffix + 12],
                'dirb7': [(0x10 * prefix) + suffix + 14]
            }

def _machine_codes_jump_operation(suffix):

    return \
            {
                'dir': [0xB0 + suffix],
                'ext': [0xC0 + suffix],
                'ix2': [0xD0 + suffix],
                'ix1': [0xE0 + suffix],
                'ix' : [0xF0 + suffix],
            }

def _calculate_branch_offset(target_token, symbol_list, assembler, instruction_length=2):
    """
        Calculate the correct branch offset given the target of a jump, which may be an address or label.

        Result may be a relative placeholder, if the target label is not defined.
    """

    #and append the branch offset
    target = parse_operand(target_token, symbol_list)

    #if we were provided a label, rather than an offset, we have to calculate its offset
    if not is_numeric(target_token):

        #resolve the address of the next instruction, which should be equal to the current location, plus the length of this instruction
        #(the length of this instruction is equal to the length of the base instruction, plus the one byte we will be adding to it)
        next_instruction = (assembler.location + instruction_length)

        #if the target resolved to an integer, then compute the correct offset in relation to the next instruction
        if isinstance(target, int):
            target = (target - next_instruction)

            #verify the branch is in range (it should almost _always_ be)
            if target > 127 or target < -128:
                raise InvalidBranchException ('Target ' + target_token + ' cannot be reached by the branch instruction, as it is out of range. Consult the course text for more information.')

            #if the branch is in range, normalize it so it falls within the 0-256 range
            else:
                target = target % 256

        #otherwise, we have a placeholder
        else:

            #add a relative tag to the placeholder, which indicates that the placeholder should resolve to an address relative to the next instruction
            target = (target, ('R', next_instruction))


    #return the computed target
    return target



class InvalidAddressingException(UserCodeException):
    """
        Specified when a given instruction is supplied with an invalid addressing mode.
    """
    pass

class ExecutionAddressingException(ExecutionException):
    """
        Execution thrown when an invalid addressing mode is requested at runtime.
    """
    pass


class HCS08_Operation(object):
    """
        Parent class for all HSC08 instructions and psuedo-operations.
    """

    mnemonics = []
    machine_codes = {}

    @classmethod
    def assemble(cls, tokens, symbols_list, assembler):
        """
            Returns a list of machine code bytes which represent the given instruction.
            If any symbols cannot be resolved, their names will be included instead.
        """
        return []

    @classmethod
    def modify_state(cls, assembler, tokens):
        pass

    @classmethod
    def read_operand(cls, address_mode, cpu):
        return ()

    @classmethod
    def execute(cls, address_mode, cpu, operand):
        pass

    @classmethod
    def shorthand(cls):
        if len(cls.mnemonics) > 0:
            return cls.mnemonics[0].upper()
        else:
            return repr(cls)


class HCS08_PseudoOp(HCS08_Operation):
    """
        Parent class for all psuedo-operations.
    """

    @classmethod
    def execute(cls, address_mode, cpu, operand):
        raise ExecutionException('Something went wrong, and the simulator tried to execute a ' + cls.shorthand() + ' pseudo-operation. This is likely an error in the simulator; please bring this to your instructor\'s attention.')


class HCS08_Instruction(HCS08_Operation):
    """
        Parent class for all instructions.
    """

    @classmethod
    def add_operand_to_code(cls, mode_word, mode_byte, operand):
        """
            Helper method which adds an operand to the machine code for this instruction (under the given mode).
        """


        #get the base machine code for this addressing mode
        try:

            #if the operand is a list, add it directly
            if isinstance(operand, list):
                code = cls.machine_codes[mode_word][:]
                code.extend(operand)

            #if the operand is a placeholder tuple:
            elif isinstance(operand, tuple):

                #if the placeholder uses relative addressing, use byte mode
                if isinstance(operand[1], tuple):
                    code = cls.machine_codes[mode_byte][:]

                #otherwise, use word mode
                else:
                    code = cls.machine_codes[mode_word][:]

                #then, add the placeholder directly
                code.append(operand)

            #if the operand is a string, check to see if the instructions upports the mode_word
            elif isinstance(operand, str):

                #if possible, use the word mode
                if mode_word in cls.machine_codes:
                    code = cls.machine_codes[mode_word][:]
                    code.extend([(operand, 1), (operand, 0)])

                #otherwise, fall back on the byte mode
                else:
                    code = cls.machine_codes[mode_byte]
                    code.append(operand)

            #if the operand is larger than a byte, split it
            elif operand > 255:
                code = cls.machine_codes[mode_word][:]
                code.extend(split_word(operand))

            #otherwise, add it directly
            else:
                code = cls.machine_codes[mode_byte][:]
                code.append(operand)

            #and return the final code
            return code

        except KeyError as e:
            raise InvalidAddressingException('Invalid addressing mode ' + repr(e.args) +  ' for instruction ' + cls.shorthand() + '; operand was ' + repr(operand) + '.');




    @classmethod
    def get_machine_code_direct(cls, tokens, symbol_list):
        """
            Gets the machine code the given instruction under direct/extended addressing modes.
        """
        #get the operand
        operand = parse_operand(tokens['direct'], symbol_list)

        #and add it to the code, given the appropriate mode
        return cls.add_operand_to_code('ext', 'dir', operand)

    @classmethod
    def get_machine_code_immediate(cls, tokens, symbol_list):
        """
            Gets the machine code for the given instruction under direct/extended addressing modes.
        """

        #add the immediate argument directly to the machine code, and return it
        return cls.add_operand_to_code('imm2', 'imm', parse_operand(tokens['immediate'], symbol_list))

    @classmethod
    def get_machine_code_indexed(cls, tokens, symbol_list):
        """
            Gets the machine code for the given instruction under the indexed modes.
        """

        #if an index offset was specified, use one of the indexed offset modes
        if 'index_offset' in tokens:

            #get the offset for the given value
            offset = parse_operand(tokens['index_offset'], symbol_list)

            #add the offset to the machine code, using the appropriate mode
            return cls.add_operand_to_code('ix2', 'ix1', offset);

        #otherwise, use raw indexed mode
        else:
            return cls.machine_codes['ix'][:]

    @classmethod
    def get_machine_code_stack_offset(cls, tokens, symbol_list):
        """
            Gets the machine code for the given instruction under stack offset addressing.
        """

        #get the stack offset
        offset = parse_operand(tokens['stack_offset'], symbol_list)

        #and return the appropriate machine code
        return cls.add_operand_to_code('sp2', 'sp1', offset)


    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):
        """
            Default handler for `HCS08_Operation.get_machine_code`.
            Attempts to use the addressing mode to select the correct machine code.
        """

        try:

            #handle immediate addressing mode
            if 'immediate' in tokens:
                return cls.get_machine_code_immediate(tokens, symbol_list)

            #handle direct / extended addressing modes
            elif 'direct' in tokens:
                return cls.get_machine_code_direct(tokens, symbol_list)

            #handle indexed, indexed offset, and indexed post increment modes
            elif 'index' in tokens:
                return cls.get_machine_code_indexed(tokens, symbol_list)

            #handle stack offset
            elif 'stack_offset' in tokens:
                return cls.get_machine_code_stack_offset(tokens, symbol_list)

            #if no operands were speicifed, assume inherent addressing mode
            else:
                    return cls.machine_codes['inh'][:]


        except KeyError as e:
            print tokens
            raise InvalidAddressingException('Invalid addressing mode ' + repr(e.args[0]).upper() + ' for instruction ' + cls.shorthand() + '.')


    @classmethod
    def read_operand(cls, address_mode, cpu):
        """
            Returns the correct operand for the given operation, given the CPU.
        """

        #handle immediate mode
        if address_mode == 'imm':

            #return the next byte directly
            return (None, cpu.fetch_byte())

        #16-bit immediate mode
        elif address_mode == 'imm2':

            #return the next two bytes
            return (None, cpu.fetch_word())

        #direct addressing
        elif address_mode == 'dir':

            #get the next byte, as it gives us the RAM address in question
            addr = cpu.fetch_byte()

            #and return the byte at the RAM address in question
            return (addr, cpu.get_by_identifier(addr))

        #extended addressing
        elif address_mode == 'ext':

            #get the next two bytes, and merge them into a 16-bit RAM address
            addr = cpu.fetch_word()

            #return the byte at the RAM address in question
            return (addr, cpu.get_by_identifier(addr))

        #indexed  addressing
        elif address_mode == 'ix':

            #return the byte at the RAM address pointed to by the indexing register, HX
            return (cpu.get_HX(), cpu.get_by_identifier(cpu.get_HX()))

        #index or stack pointer offset
        elif address_mode in ('ix1', 'ix2', 'sp1', 'sp2'):

            #if we're in one of the two byte-offset modes
            if address_mode in ('ix1', 'sp1'):

                #retrieve and sign extend the next byte, which is the index offset
                offset = sign_extend(cpu.fetch_byte())

            #otherwise, fetch the offset directly from the stack
            else:
                offset = cpu.fetch_word()

            #if this is an index-offset mode, use the value of HX as the base address
            if address_mode in ('ix1', 'ix2'):
                base = cpu.get_HX()

            #otherwise, use the value in the SP
            else:
                base = cpu.SP


            #calculate the 16-bit target address, allowing for rollover, so signed addition works
            addr = (base + offset) % 0xFFFF

            #return the byte at the computed address
            return (addr, cpu.get_by_identifier(addr))

        #if this in inherent mode, return no operand
        elif address_mode == 'inh':
                return ()

        #if none of the above matched, throw an InvalidAddressingExc
        else:
            raise ExecutionException('An error occurred, in which the ' + cls.shorthand() + ' operand was requested with the "' + address_mode + '" addressing mode, which isn\'t supported. This is likely an error in the simulator; please bring this to your instructor\'s attention.')


    @classmethod
    def execute(cls, address_mode, cpu, operand):
        raise ExecutionException('The simulator is missing the code necessary to execute ' + cls.shorthand() + ' instructions. This is likely an error in the simulator; please bring this to your instructor\'s attention.')



class HCS08_Instruction_with_AXH_Inherent(HCS08_Instruction):
    """
        Extension to the normal instruction which better supports inherent addressing of A/X/H.

    """

    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):
        """
            Extension to the standard assembly routine which supports Inherent addressing with a target of A or X.
        """

        try:

            #get the mnemonic for the current instruction
            mnemonic = tokens['mnemonic'].lower()

            if mnemonic[:-1] in cls.mnemonics:

                #target is A
                if mnemonic[-1] == 'a':
                    return cls.machine_codes['inha'][:]

                #target is X
                elif mnemonic[-1] == 'x':
                    return cls.machine_codes['inhx'][:]

                #target is H
                elif mnemonic[-1] == 'h':
                    return cls.machine_codes['inhh'][:]

        #if those modes weren't provided, throw an error
        except KeyError:
            raise InvalidAddressingException('Could not address the ' + mnemonic[:-1] + ' instruction as ' + mnemonic + '. Check the CPU quick reference for more information.')

        #deletgate to the parent class
        return super(HCS08_Instruction_with_AXH_Inherent, cls).assemble(tokens, symbol_list, assembler)


    @classmethod
    def read_operand(cls, address_mode, cpu):
        """
            Gets the operand for a given instruction, with support for inha, inhx, and inhh addressing modes.
        """

        #Inherent Accumulator mode: return the accumulator's value as the operand
        if address_mode == 'inha':
            return (('A', cpu.A))

        #Inherent X mode: return X's value as the operand
        elif address_mode == 'inhx':
            return (('X', cpu.X))

        #Inherent H mode: return H's value as the operand
        elif address_mode == 'inhh':
            return (('H', cpu.H))

        #if a non-inherent addressing mode was specified, delegate to the base class
        else:
            return super(HCS08_Instruction_with_AXH_Inherent, cls).read_operand(address_mode, cpu)


class HCS08_Constructed_Branch(HCS08_Instruction_with_AXH_Inherent):

    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):
        """
            Assemble a constructed branch, which is essentially the same as a HCS08_Instruction_with_AHX_Inherent instruction, except with a relative address appended.
        """

        #get the machine code for the simple
        base = super(HCS08_Constructed_Branch, cls).assemble(tokens, symbol_list, assembler)

        #if we don't have a target, but have a basic direct addressing scheme, intepret the direct item as a target
        #(for speed, the parser is ignorant of supported addressing modes)
        if 'target' not in tokens and tokens.keys() == ['mnemonic', 'direct']:
            tokens['target'] = tokens['direct']

        #calculate the correct branch offset given the target
        target = _calculate_branch_offset(tokens['target'], symbol_list, assembler, instruction_length = len(base) + 1)

        #append the new offset to the existing instruction
        base.append(target)

        #and return it
        return base

    @classmethod
    def read_operand(cls, address_mode, cpu):

        #get the operand for the base class
        base = super(HCS08_Constructed_Branch, cls).read_operand(address_mode, cpu)

        #and return it, plus a sign extended single byte, which encodes the branch offset
        return base + (None, sign_extend(cpu.fetch_byte()))



class HCS08_Simple_Branch(HCS08_Instruction):


    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):
        """
            Assemble a Simple Branch instruction.
        """

        #compute the branch offset given the target
        offset = _calculate_branch_offset(tokens['direct'], symbol_list, assembler)

        #simple branches _always_ use the relative instruction type
        return cls.add_operand_to_code(None, 'rel', offset)

    @classmethod
    def read_operand(cls, address_mode, cpu):

        #simple branches always directly provide a branch offset, which should be sign extended
        return (None, sign_extend(cpu.fetch_byte()))


    @classmethod
    def execute(cls, address_mode, cpu, operand):
        """
            Execute the simple branch instruction.
        """

        #if the branch condition is met
        if cls.predicate():
            cpu.PC += operand

    @classmethod
    def predicate(cls, cpu):
        """
            The conditions under which the given branch is executed.
        """
        raise NotImplementedError('Branch predicate must be implemented by any classes extending branch.');


class HCS08_Bit_Operation(HCS08_Instruction):

    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):
        """
            Assemble a bit operation, which uses a unique addressing scheme.
        """

        try:

            #extract the operand for the bit operation
            operand = parse_operand(tokens['target'], symbol_list)

            #and append it to the correct machine code, which is determined by the bit number
            return cls.add_operand_to_code(None, 'dirb' + tokens['direct'][0], operand)

        except KeyError:
            raise InvalidAddressingException('The specified ' + cls.shorthand() + 'operation is addressed incorrectly or malformed. Check the CPU quick reference, and try again.');

    @classmethod
    def read_operand(cls, address_mode, cpu):

        #bit operations are always directly addressed; get a single byte offset from the program
        addr = cpu.fetch_byte()

        #return the address directly, as bit operations affect RAM
        return (addr, cpu.ram[addr])


    @staticmethod
    def get_mask_from_address_mode(cls, address_mode):
        """
            Returns a mask which only has the given bit in question high, where bit is determined by the addressing mode.
            This is primarily used to calculate the result of bit operations.

            For example, dirb2 would produce 0b00000100.
        """

        #TODO: abstract these to a constant somewhere

        #get the bit number in question
        bit_no = ['dirb0', 'dirb1', 'dirb2', 'dirb3', 'dirb4', 'dirb5', 'dirb6', 'dirb7'].index(address_mode)

        #and build the appropriate mask
        return 1 << bit_no


class HCS08_Bit_Branch(HCS08_Instruction):

    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):

        try:

            #extract the operand and branch target
            operand = parse_operand(tokens['direct'], symbol_list)
            offset = parse_operand(tokens['target'], symbol_list)

            #if the branch target was non-numeric, convert it to an offset
            if not is_numeric(tokens['target']):
                offset = (offset - (assembler.location + 3)) % 256  #here, three bytes is the size of this instruction

            #get the base machine code for this instruction, which includes the bit number
            code = cls.machine_codes['dirb' + tokens['bit'][0]][:]

            #add the operand and offset
            code.append(operand)
            code.append(offset)

            #and return the newly created machine code
            return code

        except KeyError:
            raise InvalidAddressingException('The specified ' + cls.shorthand() + 'operation is addressed incorrectly or malformed. Check the CPU quick reference, and try again.');

    @classmethod
    def read_operand(cls, address_mode, cpu):

        #bit branches have two "operands":
        #a single direct value, from RAM
        addr = cpu.ram[cpu.fetch_byte()]

        #and a branch offset
        offset = cpu.fetch_byte()

        #return them, in that order
        return ((addr, cpu.ram[addr]), (None, offset))


class HCS08_Not_Implemented(HCS08_Instruction):
    """
        Class for instructions permenantly not implemented.
        These aren't implemented as they don't make sense as part of a simulator.
    """

    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):
        raise NotImplementedError('The simulator doesn\'t support the ' + cls.shorthand() + ' instruction. You should answer this question without it.') #TODO: replace with userexception



class ORG(HCS08_PseudoOp):
    """
        ORG (Origin) Psuedo-Op
        Sets the location counter.
    """

    mnemonics = ['org']

    @classmethod
    def modify_state(cls, assembler, tokens):

        #compute the new location pointer
        origin = parse_operand(tokens['direct'], assembler.symbols)

        #if it's not an integer, something went wrong
        if not isinstance(origin, int):
            raise InvalidAddressingException('The argument to an ORG psuedo-op must be absolute; that is, it cannot be expressed in terms of anything except for numbers and EQU constants. (You provided ' + repr(origin) + ')');

        #adjut the location pointer
        assembler.location = origin


class DS(HCS08_PseudoOp):
    """
        DS (Define Storage) Pseudo-Op
        Reserves a set of bytes (typically in RAM) by incrementing the location counter.
    """

    mnemonics = ['ds', 'ds.b']

    @classmethod
    def modify_state(cls, assembler, tokens):

        #increase the location counter by the size to reserve (in this case, a byte) times the amount given
        assembler.location += cls.size_to_reserve() * parse_number(tokens['direct'])


    @staticmethod
    def size_to_reserve():
        """
            Returns the size which this psuedo-op reserves.
        """

        #reserve one
        return 1

class DS_W(DS):
    """
        DS.W (Define Storage Words) Pseudo-Op
        Reserves a set of words (typicaly in RAM) by incrementing the location counter.
    """

    mnemonics = ['ds.w']

    @staticmethod
    def size_to_reserve():

        #reserve a word (two bytes)
        return 2


class DC(HCS08_PseudoOp):
    """
        Defines one or more constant terms, which are placed directly in flash.
        Assumes byte
    """

    mnemonics = ['dc', 'dc.b']

    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):

        try:
            #attempt to get a set of integers which represent the constant defined
            definitions = cls.parse_defines(tokens['defined'], symbol_list)

        except KeyError:
            raise InvalidAddressingException('You cannot use the DC psuedo-op without an argument; it must be followed by a constant to define. See the course text for more information.')

        return definitions

    @staticmethod
    def unit_size():
        """
            Returns the unit size of the Define Constant operation; this is equal to the amount of bytes generated per single non-string constant.
        """
        return 1

    @classmethod
    def parse_defines(cls, defines, symbol_list):

        raw_defines = []

        item = None

        try:

            for item in defines.asList():


                #if the item is an ASCII string
                if len(item) == 2 and item[0] == '"':

                    #add its raw bytes to our result
                    raw_defines.extend([ord(c) for c in item[1]])

                #if the item is a raw ASCII character
                elif len(item) == 2 and item[0] == "'":

                    #fit the character to match the unit size of the DC operation
                    raw = fit_to_size(ord(item[1]), cls.unit_size())

                    #add the raw byte contained to our result
                    raw_defines.extend(raw)

                #otherwise, parse it as an operand
                else:

                    #break the operand down into bytes, and then
                    raw = parse_operand(item, symbol_list, cls.unit_size())
                    raw_defines.extend(raw)

        except TypeError as e:
            print defines
            raise InvalidAddressingException('One or more of your ' + cls.shorthand() + ' statements is malformed, near ' + repr(item) + ' [Debug information:' + repr(e) + ']' )


        #return the raw definitions
        return raw_defines

class DCW(DC):
    """
        Define word-size constant
    """

    mnemonics = ['dc.w']

    @staticmethod
    def unit_size():
        return 2


class EQU(HCS08_PseudoOp):
    """
        EQU (Equivalent) Pseudo-Op
        Sets a value in the symbol table.
    """

    mnemonics = ['equ']

    @classmethod
    def modify_state(cls, assembler, tokens):

        #if 'direct' no in tokens: #TODO: raise UserException

        #override the symbol table entry with the current
        assembler.symbols[tokens['label'][0]] = parse_operand(tokens['direct'], assembler.symbols)


class ADC(HCS08_Instruction):
    """
        Add with carry instruction.
    """
    mnemonics = ['adc']
    machine_codes = _machine_codes_standard(0x9)

    @classmethod
    def execute(cls, address_mode, machine, operand):
        """
            Execute the ADC instruction.
        """

         #extract the operand, discarding its adddress
        _, operand = operand

        #store the initial state of the carry
        carry = machine.C

        #calculate the flags
        machine.C = (machine.A + operand + machine.C) > 256;

        #get the carry from the 7th to 8th bit
        carry_less = (machine.A & 0x7F) + (operand & 0x7F) + carry > 127;

        #compute the signed overflow
        machine.V = machine.C ^ carry_less

        #compute the half carry
        machine.H = (machine.A & 0x0F) + (operand & 0x0F) + carry > 16;

        #finally, perform the operations
        machine.A = (machine.A + operand + carry) % 256;

        #set the negative flag according to the sign bit, and the zero flag if the result is zero
        machine.N = machine.A > 127;
        machine.Z = machine.A == 0;


class ADD(HCS08_Instruction):
    """
        ADD (Add without carry) instruction.
    """
    mnemonics = ['add']
    machine_codes = _machine_codes_standard(0xB)

    @classmethod
    def execute(cls, address_mode, machine, operand):

        #extract the operand, discarding its adddress
        _, operand = operand

        #calculate the flags
        machine.C = (machine.A + machine.C) > 256;

        #get the carry from the 7th to 8th bit
        carry_less = (machine.A & 0x7F) + (operand & 0x7F) > 127;

        #compute the signed overflow
        machine.V = machine.C ^ carry_less

        #compute the half carry
        machine.H = (machine.A & 0x0F) + (operand & 0x0F)  > 127;

        #finally, perform the operations
        machine.A = (machine.A + operand) % 256;

        #set the negative flag according to the sign bit, and the zero flag if the result is zero
        cpu.update_NZ()


class AIS(HCS08_Instruction):
    """
        AIS (Add Immediate to the Stack Pointer) instruction.
    """
    mnemonics = ['ais']
    machine_codes = { 'imm': [0xA7] }

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the operand value, discarding its address
        _, operand = operand

        #add the immediate to the stack
        cpu.SP += operand;



class AIX(HCS08_Instruction):
    """
        AIX (Add Immediate to the Stack Pointer) instruction.
    """
    mnemonics = ['aix']
    machine_codes =  { 'imm': [0xAF] }

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the operand value, discarding its address
        _, operand = operand

        #add the operand to HX
        cpu.set_HX(cpu.get_HX() + operand)


class AND(HCS08_Instruction):
    """
        AND (Logical AND) instruction.
    """
    mnemonics = ['and']
    machine_codes = _machine_codes_standard(0x4)

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the operand value, discarding its address
        _, operand = operand

        #perform the logical AND
        cpu.A &= operand

        #and adjust the value of the N and Z flags
        cpu.update_NZ()


class ASL(HCS08_Instruction_with_AXH_Inherent):
    """
        ASL (Arithmetic Shift Left) instruction.
    """
    mnemonics = ['asl', 'asla', 'aslx']
    machine_codes = _machine_codes_axh_inherent(0x8)

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #split the operand into its target location and value
        target, value = operand

        #set the C flag if the resulting shift left will "push" out a 1
        cpu.C = value > 127

        #calculate the result
        result = (operand << 1) & 0xFF

        #and store it in the appropriate location
        cpu.set_by_identifier(target, result)


class ASR(HCS08_Instruction_with_AXH_Inherent):
    """
        ASR (Arithmetic Shift Right) instruction.
    """
    mnemonics = ['asr', 'asra', 'asrx']
    machine_codes = _machine_codes_axh_inherent(0x7)


    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #split the operand into its target location and value
        target, value = operand[0]

        #set the C flag if the resulting shift right will "push" out a 1
        cpu.C = value & 0x01

        #calculate the result
        result = opernad >> 1

        #and store it to the appropriate location
        cpu.set_by_identifier(target, result)


class BCC(HCS08_Simple_Branch):
    """
        BCC (Branch if Carry Clear)
        Branches if the carry bit is clear.
    """
    mnemonics = ['bcc']
    machine_codes = { 'rel': [0x24] }

    @classmethod
    def predicate(cls, cpu):

        #branch if the carry is false
        return not cpu.C


class BCLR(HCS08_Bit_Operation):
    """
        BCLR- clears the appropriate bit of a byte in memory.
    """
    mnemonics = ['bclr']
    machine_codes = _machine_codes_bit_operation(prefix=1, is_clear=True)


    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #split the operand into its target location and value
        target, value = operand[0]

        #get a bit mask from the address mode
        mask = cls.get_mask_from_address_mode(address_mode)

        #and use it to clear the appropriate bit
        cpu.set_by_identifier(target, value & ~mask)


class BCS(HCS08_Simple_Branch):
    """
        BCR (Branch if Carry Set)
        Branches if the carry bit is set (1).
    """
    mnemonics = ['bcs']
    machine_codes = { 'rel': [0x25] }

    @classmethod
    def predicate(cls, cpu):

        #branch if the carry is true
        return cpu.C


class BEQ(HCS08_Simple_Branch):
    """
        BEQ (Branch if Equal)
    """
    mnemonics = ['beq']
    machine_codes = { 'rel': [0x27] }

    @classmethod
    def predicate(cls, cpu):

        #branches if Z is set
        return cpu.Z


class BGE(HCS08_Simple_Branch):
    """
        BGE (Branch if Greater than or Equal To)
    """
    mnemonics = ['bge']
    machine_codes = { 'rel': [0x90] }

    @classmethod
    def predicate(cls, cpu):

        #branches if >=, signed:  N^V
        return not(cpu.N ^ cpu.V)


class BGND(HCS08_Not_Implemented):
    """
        BGND: Enter background debug mode.
    """
    mnemonics = ['bgnd']



class BGT(HCS08_Simple_Branch):
    """
        BGT (Branch if Greater Than)
    """
    mnemonics = ['bgt']
    machine_codes =  { 'rel': [0x92] }

    @classmethod
    def predicate(cls, cpu):

        #branch if >, signed
        return not (cpu.Z or (cpu.N ^ cpu.V))


class BHCC(HCS08_Simple_Branch):
    """
        BHCC (Branch if Half Carry Clear)
    """
    mnemonics = ['bhcc']
    machine_codes = { 'rel': [0x28] }

    @classmethod
    def predicate(cls, cpu):

        #branch if the half carry is clear
        return not self.H

class BHCS(HCS08_Simple_Branch):
    """
        BHCS (Branch if Half Carry Set)
    """
    mnemonics = ['bhcs']
    machine_codes = { 'rel': [0x29] }

    @classmethod
    def predicate(cls, cpu):

        #branch if the half carry is set
        return self.H


class BHI(HCS08_Simple_Branch):
    """
        BHI (Branch if Higher)
    """
    mnemonics = ['bhi']
    machine_codes = { 'rel': [0x22] }

    @classmethod
    def predicate(cls, cpu):

        #branch if >, unsigned
        return not (self.C or self.Z)


class BHS(HCS08_Simple_Branch):
    """
        BHS (Branch if Higher or Same)
    """
    mnemonics = ['bhs']
    machine_codes = { 'rel': [0x24] }

    @classmethod
    def predicate(cls, cpu):

        #branch if >=, unsigned
        return not self.C

class BIH(HCS08_Simple_Branch):
    """
        BIH (Branch if IRQ Pin High)
    """
    mnemonics = ['bih']
    machine_codes = { 'rel': [0x2F] }

    @classmethod
    def predicate(cls, cpu):

        #TODO: possibly implement the IRQ pin as a register in the future, or use the appropriate port input?
        return False

class BIL(HCS08_Simple_Branch):
    """
        BIL (Branch if IRQ Pin Low)
    """
    mnemonics = ['bil']
    machine_codes = { 'rel': [0x2E] }

    @classmethod
    def predicate(cls, cpu):
        return False


class BIT(HCS08_Instruction):
    """
        BIT (Bit Test)
        Calculates what the flags would be if the accumulator was AND'd with the operand.
    """
    mnemonics = ['bit']
    machine_codes = _machine_codes_standard(0x5)

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the operand value, discarding its address
        _, operand = operand

        #calculate the result of the accumulator and'd with the operand, but don't set the result
        result = cpu.A and operand

        #clear the V flag, as specified by the instruction set specs
        cpu.V = False

        #and set the N and Z flags
        cpu.N = (result > 128)
        cpu.Z = (result == 0)


class BLE(HCS08_Simple_Branch):
    """
        BLE (Branch if Less than or Equal)
    """
    mnemonics = ['ble']
    machine_codes = { 'rel': [0x93] }

    @classmethod
    def predicate(cls, cpu):

        #branch if <=, signed
        return cpu.Z or (cpu.N ^ cpu.V)


class BLO(HCS08_Simple_Branch):
    """
        BLO (Branc if Lower)
    """
    mnemonics = ['blo']
    machine_codes = { 'rel': [0x25] }

    @classmethod
    def predicate(cls, cpu):

        #branch if <, unsigned
        return cpu.C


class BLS(HCS08_Simple_Branch):
    """
        BLS (Branch if Lower or Same)
    """
    mnemonics = ['bls']
    machine_codes = { 'rel': [0x23] }

    @classmethod
    def predicate(cls, cpu):

        #branch if <=, unsigned
        return cpu.C or cpu.Z


class BLT(HCS08_Simple_Branch):
    """
        BLT (Branch if Less Than)
    """
    mnemonics = ['blt']
    machine_codes = { 'rel': [0x91] }

    @classmethod
    def predicate(cls, cpu):

        #branch if <, signed
        return cpu.N ^ cpu.Z


class BMC(HCS08_Simple_Branch):
    """
        BMC (Branch if Interrupt Mask is Clear)
    """
    mnemonics = ['bmc']
    machine_codes = { 'rel': [0x2C] }

    @classmethod
    def predicate(cls, cpu):
        return not cpu.I


class BMI(HCS08_Simple_Branch):
    """
        BMI (Branch if Minus)
    """
    mnemonics = ['bmi']
    machine_codes = { 'rel': [0x2B] }

    @classmethod
    def predicate(cls, cpu):

        #branch if N=1
        return cpu.N


class BMS(HCS08_Simple_Branch):
    """
        BMS (Branch if Interrupt Mask is Set)
    """
    mnemonics = ['bms']
    machine_codes = { 'rel': [0x2D] }

    @classmethod
    def predicate(cls, cpu):
        return cpu.I


class BNE(HCS08_Simple_Branch):
    """
        BNE (Branch if Not Equal)
    """
    mnemonics = ['bne']
    machine_codes = { 'rel': [0x26] }

    @classmethod
    def predicate(cls, cpu):
        return cpu.Z


class BPL(HCS08_Simple_Branch):
    """
        BPL (Branch of IRQ Pin Low)
    """
    mnemonics = ['bpl']
    machine_codes = { 'rel': [0x2A] }

    @classmethod
    def predicate(cls, cpu):

        #branch if not negative
        return not self.N

class BRA(HCS08_Simple_Branch):
    """
        BRA (Branch Always)
    """
    mnemonics = ['bra']
    machine_codes = { 'rel': [0x20] }

    @classmethod
    def predicate(cls, cpu):
        return True

class BRCLR(HCS08_Bit_Branch):
    """
        BRCLR (Branch if Bit Clear)
        Branches if the given bit is clear.
    """
    mnemonics = ['brclr']
    machine_codes = _machine_codes_bit_operation(prefix=0, is_clear=True)


    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the operand value, discarding its address
        _, operand = operand[0]

        #and extract the branch target
        _, offset =  operand[1]

        #get a bitmask, which will allow us to test just the relevant bit
        mask = HCS08_Bit_Operation.get_mask_from_address_mode(address_mode)

        #if the given bit is clear (the operand AND's with the mask to be zero), branch
        if not (mask & operand):
            cpu.PC += offset


class BRN(HCS08_Simple_Branch):
    """
        BRN (Branch Never)
        Branches if I=0.
    """
    mnemonics = ['brn']
    machine_codes = { 'rel': [0x21] }

    @classmethod
    def predicate(cls, cpu):
        return False


class BRSET(HCS08_Bit_Branch):
    """
        BRSET (Branch if Bit Set)
        Branches if the given bit is set.
    """
    mnemonics = ['brset']
    machine_codes = _machine_codes_bit_operation(prefix=0, is_clear=False)

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the operand value, discarding its address
        _, operand = operand[0]

        #and extract the branch target
        _, offset =  operand[1]

        #get a bitmask, which will allow us to test just the relevant bit
        mask = HCS08_Bit_Operation.get_mask_from_address_mode(address_mode)

        #if the given bit is set (the operand AND's with the mask to be one), branch
        if mask & operand:
            cpu.PC += offset


class BSET(HCS08_Bit_Operation):
    """
        BSET (Set Bit)
    """
    mnemonics = ['bset']
    machine_codes = _machine_codes_bit_operation(prefix=1, is_clear=False)

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #split the operand into its target location and value
        target, value = operand[0]

        #get a bit mask from the address mode
        mask = cls.get_mask_from_address_mode(address_mode)

        #and use it to set the appropriate bit
        cpu.set_by_identifier(target, value mask)


class BSR(HCS08_Simple_Branch):
    """
        BSR (Branch to Subroutine)
        Branches if I=0.
    """
    mnemonics = ['bsr']
    machine_codes = { 'rel': [0xAD] }

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the offset value, which indicates the subroutine's address
        _, offset = operand

        #push the return address onto the stack
        cpu.push_word(cpu.PC)

        #and branch to the subroutine
        cpu.PC += offset



class CBEQ(HCS08_Constructed_Branch):
    """
        Compare and Branch if Equal
    """
    mnemonics = ['cbeq', 'cbeqa', 'cbeqx']
    machine_codes = _machine_codes_axh_inherent(0x01)

    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):
        """
            Assembles the CBEQ instruction, which has a special case in which its inherent modes require multiple arguments.
        """
        #get the base case machine codes, for the non-special cases
        base = super(CBEQ, cls).assemble(tokens, symbol_list, assembler)

        #if this is one of our inherently addressed instructions
        if base[0] in (cls.machine_codes['inha'][0], cls.machine_codes['inhx'][0]):

            #insert the immediate before the branch target
            base.insert(1, parse_operand(tokens['immediate'], symbol_list))


        #return the (possibly corrected) machine code
        return base

    @classmethod
    def read_operand(cls, address_mode, cpu):
        """
            Gets the operands for a CBEQ instruction.
        """

        #if this is one of the special case inherents, handle the opcode read here
        if address_mode in ('inha', 'inhx'):

            #each of the inherent modes above have two operands: an immediate, followed by a relative offset
            return ((None, cpu.fetch_byte()), (None, cpu.fetch_byte())

        #otherwise, delegate to the parent class
        else:
            return super(CBEQ, cls).read_operand(address_mode, cpu)



    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the operand value
        _, operand = operand[0]

        #and extract the branch offset
        _, offset = operand[1]

        #if we're in the special CBEQX mode, compare X and the operand
        if address_mode = 'inhx':
            predicate = (cpu.X == operand)

        #otherwise, compare the operand to the accumulator
        else:
            predicate = (cpu.A == operand)

        #if the branch condition was met, branch
        if predicate:
            self.PC += offset



class CLC(HCS08_Instruction):
    """
        Clear Carry Bit
    """
    mnemonics = ['clc']
    machine_codes = { 'inh': [0x98] }

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #clear the carry
        cpu.C = False


class CLI(HCS08_Instruction):
    """
        Clear Interrupt Mask
    """
    mnemonics = ['cli']
    machine_codes = { 'inh': [0x9A] }

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #clear the interrupt mask
        cpu.I = False


class CLR(HCS08_Instruction_with_AXH_Inherent):
    """
        Clears a given register or memory address.
    """
    mnemonics = ['clr', 'clra', 'clrx', 'clrh']
    machine_codes = _machine_codes_axh_inherent(0xF, h_suffix=0xC)

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the operand target, ignoring its value
        target, _ = operand

        #clear the target address or register
        self.set_by_identifier(target, 0)

        #set the flags as specified in the instruction set spec
        self.V = 0
        self.N = 0
        self.Z = 1


class CMP(HCS08_Instruction):
    """
        Compare accumulator with memory
    """
    mnemonics = ['cmp']
    machine_codes = _machine_codes_standard(0x1)

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the operand value, discarding its address
        _, operand = operand

        #perform a theoretical subtraction, which we will use to determine the flag values
        result = cpu.A - operand

        #get quick references to the signs of the accumulator, the operand, and the result
        a_sign = cpu.A > 127
        result_sign = result > 127
        operand_sign = operand > 127

        #flag values, taken directly from the instruction set spec:
        self.V = (a_sign and not result_sign and not operand_sign) or (not a_sign and operand_sign and result_sign)
        self.N = result_sign
        self.Z = (result == 0)
        self.C = (operand_sign and not a_sign) or (operand_sign and result_sign) or (result_sign and not a_sign)


class COM(HCS08_Instruction_with_AXH_Inherent):
    """
        One's compliment a given register or memory address.
    """
    mnemonics = ['com', 'coma', 'comx']
    machine_codes = _machine_codes_axh_inherent(0x03)

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the operand value, as well as the target register
        target, operand = operand

        #perform the one's compliment, and store the result
        self.set_by_identifier(target, ~operand)


class CPHX(HCS08_Instruction):
    """
        Compare word with HX.
    """
    mnemonics = ['cphx']

    machine_codes = \
            {
                'imm2': [0x65],
                'dir':  [0x75],
                'ext':  [0x3E],
                'sp1':  [0x9E, 0xF3]
            }

    @classmethod
    def read_operand(cls, address_mode, cpu):

        #if we're using a two-byte immediate, the standard operand retrieval works properly; use it
        if address_mode == 'imm2':
            return super(CPHX, cls).read_operand(address_mode, cpu)

        #handle direct/extended addressing
        elif address_mode in ('dir', 'ext'):

            #read the byte address
            addr = cpu.fetch_byte() if address_mode == 'dir' else cpu.fetch_word()

            #and retrieve the _word_ at that address
            value = self.get_by_identifier(addr, is_word=True)

            #return the operand
            return addr, value

        elif address_mode == 'sp1':

            #fetch the single byte stack offset
            offset = cpu.fetch_byte()

            #and use it to compute the resultant address
            addr = cpu.SP + offset

            #get the _word_ at that address
            value = self.get_by_identifier(addr, is_word=True)

            #return the operand
            return addr, value

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the operand value, discarding its address
        _, operand = operand

        #perform a theoretical subtraction, which we will use to determine the flag values
        result = cpu.get_HX() - operand

        #get quick references to the signs of the accumulator, the operand, and the result
        HX_sign = cpu.get_HX() >  0x7FFF
        result_sign = result > 0x7FFF
        operand_sign = operand > 0x7FFF

        #flag values, taken directly from the instruction set spec:
        self.V = (hx_sign and not result_sign and not operand_sign) or (not hx_sign and operand_sign and result_sign)
        self.N = result_sign
        self.Z = (result == 0)
        self.C = (operand_sign and not hx_sign) or (operand_sign and result_sign) or (result_sign and not hx_sign)


class CPX(HCS08_Instruction):
    """
        Compare X with Memory
    """
    mnemonics = ['cpx']
    machine_codes = _machine_codes_standard(0x3)

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the operand value, discarding its address
        _, operand = operand

        #perform a theoretical subtraction, which we will use to determine the flag values
        result = cpu.X - operand

        #get quick references to the signs of the accumulator, the operand, and the result
        x_sign = cpu.X > 127
        result_sign = result > 127
        operand_sign = operand > 127

        #flag values, taken directly from the instruction set spec:
        self.V = (x_sign and not result_sign and not operand_sign) or (not x_sign and operand_sign and result_sign)
        self.N = result_sign
        self.Z = (result == 0)
        self.C = (operand_sign and not x_sign) or (operand_sign and result_sign) or (result_sign and not x_sign)



class DAA(HCS08_Instruction):
    """
        Decimal Adjust Accumulator
    """
    mnemonics = ['daa']
    machine_codes = { 'inh': [0x72] }


    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #split the accumulator into nibbles:
        msn = cpu.A >> 4
        lsn = cpu.A & 0x0F;

        #perform the operation denoted on table A-2 of the instruction set spec
        #(transcribed to match the table exactly)
        if      cpu.C == 0  and msn <= 8    and cpu.H == 0  and lsn >= 0xA  :   cpu.A += 0x06
        elif    cpu.C == 0  and msn <= 9    and cpu.H == 1  and lsn <= 3    :   cpu.A += 0x06
        elif    cpu.C == 0  and msn >= 0xA  and cpu.H == 0  and lsn <= 9    :   cpu.A += 0x60;  cpu.C = 1
        elif    cpu.C == 0  and msn >= 9    and cpu.H == 0  and lsn >= 0xA  :   cpu.A += 0x66;  cpu.C = 1
        elif    cpu.C == 0  and msn >= 0xA  and cpu.H == 1  and lsn <= 3    :   cpu.A += 0x66;  cpu.C = 1
        elif    cpu.C == 1  and msn <= 2    and cpu.H == 0  and lsn <= 9    :   cpu.A += 0x60
        elif    cpu.C == 1  and msn <= 2    and cpu.H == 0  and lsn >= 0xA  :   cpu.A += 0x66
        elif    cpu.C == 1  and msn <= 3    and cpu.H == 1  and lsn <= 3    :   cpu.A += 0x66

        #set the flags:
        self.V = None #undefined by instruction set spec
        self.N = cpu.A > 127
        self.Z = cpu.A == 0


class DBNZ(HCS08_Constructed_Branch):
    """
        Decrement, then Branch if target is Zero
    """
    mnemonics = ['dbnz', 'dbnza', 'dbnzx']
    machine_codes = _machine_codes_axh_inherent(0xB)

    @classmethod
    def execute(cls, address_mode, cpu, operand):

        #extract the operand value, and location
        target, operand = operand[0]

        #extract the branch offset
        _, offset = operand[1]

        #decrement the operand
        cpu.set_by_identifier(target, operand - 1)

        #and branch if the post-decrement result is not zero
        if operand == 1:
            cpu.PC += offset


class DEC(HCS08_Instruction_with_AXH_Inherent):
    """
        Decrement Target
    """
    mnemonics = ['dec', 'deca', 'decx']
    machine_codes = _machine_codes_axh_inherent(0xA)

class DIV(HCS08_Instruction):
    """
        Divide
    """
    mnemonics = ['div']
    machine_codes = { 'inh': [0x52] }

class EOR(HCS08_Instruction):
    """
        Exclusive OR Memory with Accumulator
    """
    mnemonics = ['eor']
    machine_codes = _machine_codes_standard(0x8)

class INC(HCS08_Instruction_with_AXH_Inherent):
    """
        Increment Target
    """
    mnemonics = ['inc', 'inca', 'incx']
    machine_codes = _machine_codes_axh_inherent(0xC)

class JMP(HCS08_Instruction):
    """
        Jump
    """
    mnemonics = ['jmp']
    machine_codes = _machine_codes_jump_operation(0xC)

class JSR(HCS08_Instruction):
    """
        Jump to Subroutine
    """
    mnemonics = ['jsr']
    machine_codes = _machine_codes_jump_operation(0xD)


class LDA(HCS08_Instruction):
    """
        Load value into accumulator
    """
    mnemonics = ['lda']
    machine_codes = _machine_codes_standard(0x06)

class LDHX(HCS08_Instruction):
    """
        Load word into HX.
    """
    mnemonics = ['ldhx']

    machine_codes = \
            {
                'imm2': [0x45],
                'dir':  [0x55],
                'ext':  [0x32],
                'ix2':  [0x9E, 0xBE],
                'ix1':  [0x9E, 0xCE],
                'ix' :  [0x9E, 0xAE],
                'sp1':  [0x9E, 0xFE],
            }

class LDX(HCS08_Instruction):
    """
        Load X from Memory
    """
    mnemonics = ['ldx']
    machine_codes = _machine_codes_standard(0xE)

class LSL(ASL):
    """
        Logical Shift Left- Identical to ASL
    """
    mnemonics = ['lsl', 'lsla', 'lslx']

    #Note:  Do not impelement further; identical to ASL.
    #       Any changes should be made there.


class LSR(HCS08_Instruction_with_AXH_Inherent):
    """
        Logical Shift Right
    """
    mnemonics = ['lsr', 'lsra', 'lsrx']
    machine_codes = _machine_codes_axh_inherent(0x4)

class MOV(HCS08_Instruction):
    """
        Move (copy from RAM to RAM)
    """
    mnemonics = ['mov']

    machine_codes = \
            {
                'dir': [0x4E],
                'ix1': [0x5E], #technically post increment
                'imm': [0x6E],
                'ix':  [0x7E] #technically post increment
            }

    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):

        #perform the normal, core processing to handle the first argument
        code = super(MOV, cls).assemble(tokens, symbol_list, assembler)

        #and append the move target
        if 'target' in tokens:
            code.append(parse_operand(tokens['target'], symbol_list))

        #in the event that we have the format MOV dd,X+, the parser will interpret that as an index offset, which will automatically produce the correct output
        #if that did not occur, it's likely the MOV instruction is malformed
        elif 'index_offset' not in tokens:
            raise InvalidAddressingException('The MOV instruction cannot be used this way. Are you using a supported addressing mode? Check the CPU quick reference, and try again.')

        #and return the new machine code
        return code



class MUL(HCS08_Instruction):
    """
        Multiply
    """
    mnemonics = ['mul']
    machine_codes = { 'inh': [0x42] }


class NEG(HCS08_Instruction_with_AXH_Inherent):
    """
        Negate via Two's Compliment
    """

    mnemonics = ['neg', 'nega', 'negx']
    machine_codes = _machine_codes_axh_inherent(0x0)


class NOP(HCS08_Instruction):
    """
        No Operation- do nothing for one machine cycle
    """
    mnemonics = ['nop']
    machine_codes = { 'inh': [0x9D] }


class NSA(HCS08_Instruction):
    """
        Nibble Swap Accumulator
    """
    mnemonics = ['nsa']
    machine_codes = { 'inh': [0x62] }


class ORA(HCS08_Instruction):
    """
        Or Accumulator with Memory
    """
    mnemonics = ['ora']
    machine_codes = _machine_codes_standard(0xA)


class PSH(HCS08_Instruction_with_AXH_Inherent):
    """
        Push family of instructions.
    """
    mnemonics = ['psh', 'psha', 'pshx', 'pshh']

    machine_codes = \
            {
                'inha': [0x87],
                'inhx': [0x89],
                'inhh': [0x8B],
            }

class PUL(HCS08_Instruction_with_AXH_Inherent):
    """
        Pull family of instructions.
    """
    mnemonics = ['pul', 'pula', 'pulx', 'pulh']

    machine_codes = \
            {
                'inha': [0x86],
                'inhx': [0x88],
                'inhh': [0x8A],
            }

class ROL(HCS08_Instruction_with_AXH_Inherent):
    """
        Rotate Left through Carry
    """

    mnemonics = ['rol', 'rola', 'rolx']
    machine_codes = _machine_codes_axh_inherent(0x9)


class ROR(HCS08_Instruction_with_AXH_Inherent):
    """
        Rotate Right through Carry
    """

    mnemonics = ['ror', 'rora', 'rorx']
    machine_codes = _machine_codes_axh_inherent(0x6)


class RSP(HCS08_Instruction):
    """
        Reset Low Byte of Stack Pointer
    """
    mnemonics = ['rsp']
    machine_codes = { 'inh': [0x9C] }


class RTI(HCS08_Instruction):
    """
        Return from Interrupt
    """
    mnemonics = ['rti']
    machine_codes = { 'inh': [0x80] }


class RTS(HCS08_Instruction):
    """
        Return from Subroutine
    """
    mnemonics = ['rts']
    machine_codes = { 'inh': [0x81] }



class SBC(HCS08_Instruction):
    """
        Subtract with Carry
    """
    mnemonics = ['sbc']
    machine_codes = _machine_codes_standard(0x2)


class SEC(HCS08_Instruction):
    """
        Set Carry Bit
    """
    mnemonics = ['sec']
    machine_codes = { 'inh': [0x99] }


class SEI(HCS08_Instruction):
    """
        Set Interrupt Mask
    """
    mnemonics = ['sei']
    machine_codes = { 'inh': [0x9B] }


class STA(HCS08_Instruction):
    """
        Store Accumulator in Memory
    """
    mnemonics = ['sta']
    machine_codes = _machine_codes_standard(0x7, include_imm=False)

class STHX(HCS08_Instruction):
    """
        Store H:X in Memory
    """
    mnemonics = ['sthx']
    machine_codes = \
            {
                'dir': [0x35],
                'ext': [0x96],
                'sp1': [0x9E, 0xFF]
            }

class STOP(HCS08_Not_Implemented):
    """
        Stops processing and enables interrupts.
        Not implemented due to scope of simulator.
    """

    mnemonics = ['stop']


class STX(HCS08_Instruction):
    """
        Store X to Memory
    """
    mnemonics = ['stx']
    machine_codes = _machine_codes_standard(0xF, include_imm=False)


class SUB(HCS08_Instruction):
    """
        Subtract value from Accumulator
    """
    mnemonics = ['sub']
    machine_codes = _machine_codes_standard(0x0)


class SWI(HCS08_Instruction):
    """
        Software Interrupt
    """
    mnemonics = ['swi']
    machine_codes = { 'inh': [0x83] }


class TAP(HCS08_Instruction):
    """
        Transfer Accumulator to CCR
    """
    mnemonics = ['tap']
    machine_codes = { 'inh': [0x84] }


class TAX(HCS08_Instruction):
    """
        Transfer Accumulator to X
    """
    mnemonics = ['tax']
    machine_codes = { 'inh': [0x97] }


class TPA(HCS08_Instruction):
    """
        Transfer CCR to Accumulator
    """
    mnemonics = ['tpa']
    machine_codes = { 'inh': [0x85] }


class TST(HCS08_Instruction_with_AXH_Inherent):
    """
        Test for Negative or Zero
    """

    mnemonics = ['tst', 'tsta', 'tstx']
    machine_codes = _machine_codes_axh_inherent(0xD)


class TSX(HCS08_Instruction):
    """
        Transfer SP to HX
    """
    mnemonics = ['tsx']
    machine_codes = { 'inh': [0x95] }


class TXA(HCS08_Instruction):
    """
        Transfer X to A
    """
    mnemonics = ['txa']
    machine_codes = { 'inh': [0x9F] }


class TXS(HCS08_Instruction):
    """
        Transfer HX to SP
    """
    mnemonics = ['txs']
    machine_codes = { 'inh': [0x94] }


class WAIT(HCS08_Not_Implemented):
    """
        Wait for Interrupt
    """
    mnemonics = ['wait']
    machine_codes = {'inh': [0x8F] }

