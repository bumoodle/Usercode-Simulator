"""
    Class objects which represent an instruction on a HCS08 microprocessor.

    Copyright (c) 2012, Binghamton University
    Kyle Temkin <ktemkin@binghamton.edu>

"""



from utils import parse_number, parse_operand, split_word, is_numeric


class InvalidAddressingException(Exception):
    """
        Specified when a given instruction is supplied with an invalid addressing mode.
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
    def shorthand(cls):
        if len(cls.mnemonics) > 0:
            return cls.mnemonics[0].upper()
        else:
            return repr(cls)


class HCS08_PseudoOp(HCS08_Operation):
    """
        Parent class for all psuedo-operations.
    """
    pass

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
                code = cls.machine_codes[mode_word]
                code.extend(split_word(operand))

            #otherwise, add it directly
            else:
                code = cls.machine_codes[mode_byte]
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
        operand = parse_number(tokens['direct'])

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
        offset = parse_operand(tokens['stack_offset'])

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
                cls.get_machine_code_stack_offset(tokens, symbol_list)

            #if no operands were speicifed, assume inherent addressing mode
            else:
                    return cls.machine_codes['inh'][:]


        except KeyError as e:
            print tokens
            raise InvalidAddressingException('Invalid addressing mode ' + repr(e.args[0]).upper() + ' for instruction ' + cls.shorthand() + '.')

class HCS08_Instruction_with_AXInherent(HCS08_Instruction):
    """
        Extension to the normal instruction which better supports inherent addressing of A/X.
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
                    return cls.machine_codes['inha']

                #target is X
                elif mnemonic[-1] == 'x':
                    return cls.machine_codes['inhx']

        #if those modes weren't provided, throw an error
        except KeyError:
            raise InvalidAddressingException('Could not address the ' + mnemonic[:-1] + ' instruction as ' + mnemonic + '. Check the CPU quick reference for more information.')

        #deletgate to the parent class
        return super(HCS08_Instruction_with_AXInherent, cls).assemble(tokens, symbol_list, assembler)


class HCS08_Simple_Branch(HCS08_Instruction):


    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):
        """
            Assemble a Simple Branch instruction.
        """

        #get the location from the assembly tokens
        offset = parse_operand(tokens['direct'], symbol_list)

        #if we provided a label (rather than a true branch offset), use it to compute an offset
        if not is_numeric(tokens['direct']):
            offset = (offset - (assembler.location + 2)) % 256  #here, two bytes is the size of this instruction

        #simple branches _always_ use the relative instruction type
        return cls.add_operand_to_code(None, 'rel', offset)

    def execute(cls, machine, operand):
        """
            Execute the simple branch instruction.
        """

        #if the branch condition is met
        if cls.predicate():
            self.PC += operand


    def predicate():
        """
            The conditions under which the given branch is executed.
        """
        raise NotImplementedError('Branch predicate must be implemented by any classes extending branch.');


class HCS08_Bit_Operation(HCS08_Instruction):

    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):
        raise NotImplementedError('Bit Operations are not yet implemented.')

class HCS08_Bit_Branch(HCS08_Instruction):

    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):
        raise NotImplementedError('Bit Branches are not yet implemented.')


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
            print "not int?" #TODO: throw exception

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

    @staticmethod
    def size_to_reserve():

        #reserve a word (two bytes)
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
        assembler.symbols[tokens['label'][0]] = parse_number(tokens['direct'])


class ADC(HCS08_Instruction):
    """
        Add with carry instruction.
    """
    mnemonics = ['adc']

    machine_codes = \
            {
                'imm': [0xA9],
                'dir': [0xB9],
                'ext': [0xC9],
                'ix2': [0xD9],
                'ix1': [0xE9],
                'ix' : [0xF9],
                'sp2': [0x9E, 0xD9],
                'sp1': [0x9E, 0xE9],
            }

    @classmethod
    def execute(cls, machine, operand):
        """
            Execute the ADC instruction.
        """

        #calculate the flags
        machine.C = (machine.A + operand + machine.carry) > 256;

        #get the carry from the 7th to 8th bit
        carry_less = (machine.A & 0x7F) + (operand & 0x7F) + machine.carry > 127;

        #compute the signed overflow
        machine.V = machine.C ^ carry_less

        #compute the half carry
        machine.H = (machine.A & 0x0F) + (operand & 0x0F) + machine.carry > 127;

        #finally, perform the operations
        machine.A = (machine.A + operand + machine.carry) % 256;

        #set the negative flag according to the sign bit, and the zero flag if the result is zero
        machine.N = machine.A > 127;
        machine.Z = machine.A == 0;


class ADD(HCS08_Instruction):
    """
        ADD (Add without carry) instruction.
    """
    mnemonics = ['add']

    machine_codes = \
            {
                'imm': [0xAB],
                'dir': [0xBB],
                'ext': [0xCB],
                'ix2': [0xDB],
                'ix1': [0xEB],
                'ix' : [0xFB],
                'sp2': [0x9E, 0xDB],
                'sp1': [0x9E, 0xEB],
            }


class AIS(HCS08_Instruction):
    """
        AIS (Add Immediate to the Stack Pointer) instruction.
    """
    mnemonics = ['ais']

    machine_codes = \
            {
                'imm': [0xA7],
            }


class AIS(HCS08_Instruction):
    """
        AIS (Add Immediate to the Stack Pointer) instruction.
    """
    mnemonics = ['ais']

    machine_codes = \
            {
                'imm': [0xAF],
            }


class AND(HCS08_Instruction):
    """
        AND (Logical AND) instruction.
    """
    mnemonics = ['and']

    machine_codes = \
            {
                'imm': [0xA4],
                'dir': [0xB4],
                'ext': [0xC4],
                'ix2': [0xD4],
                'ix1': [0xE4],
                'ix' : [0xF4],
                'sp2': [0x9E, 0xD4],
                'sp1': [0x9E, 0xE4],
            }

class ASL(HCS08_Instruction_with_AXInherent):
    """
        ASL (Arithmetic Shift Left) instruction.
    """
    mnemonics = ['asl', 'asla', 'aslx']

    machine_codes = \
            {
                'dir': [0x38],
                'inha' : [0x48],
                'inhx' : [0x58],
                'ix1': [0x68],
                'ix' : [0x78],
                'sp1': [0x9E, 0x68]
            }

class ASR(HCS08_Instruction_with_AXInherent):
    """
        ASR (Arithmetic Shift Right) instruction.
    """
    mnemonics = ['asr', 'asra', 'asrx']

    machine_codes = \
            {
                'dir': [0x37],
                'inha' : [0x47],
                'inhx' : [0x57],
                'ix1': [0x67],
                'ix' : [0x77],
                'sp1': [0x9E, 0x67]
            }

class BCC(HCS08_Simple_Branch):
    """
        BCC (Branch if Carry Clear)
        Branches if the carry bit is clear.
    """
    mnemonics = ['bcc']

    machine_codes = \
            {
                'rel': [0x24],
            }


class BCLR(HCS08_Bit_Operation):
    """
        BCC (Branch if Carry Clear)
        Branches if the carry bit is clear.
    """
    mnemonics = ['bclr']

    machine_codes = \
            {
                'dirb0': [0x11],
                'dirb1': [0x13],
                'dirb2': [0x15],
                'dirb3': [0x17],
                'dirb4': [0x19],
                'dirb5': [0x1B],
                'dirb6': [0x1D],
                'dirb7': [0x1F],
            }


class BCS(HCS08_Simple_Branch):
    """
        BCR (Branch if Carry Set)
        Branches if the carry bit is set (1).
    """
    mnemonics = ['bcs']

    machine_codes = \
            {
                'rel': [0x25],
            }

class BEQ(HCS08_Simple_Branch):
    """
        BEQ (Branch if Equal)
    """
    mnemonics = ['bcs']

    machine_codes = \
            {
                'rel': [0x25],
            }

class BGE(HCS08_Simple_Branch):
    """
        BGE (Branch if Greater than or Equal To)
    """
    mnemonics = ['bge']

    machine_codes = \
            {
                'rel': [0x90],
            }

class BGND(HCS08_Not_Implemented):
    """
        BGND: Enter background debug mode.
    """

    mnemonics = ['bgnd']

    pass

class BGT(HCS08_Simple_Branch):
    """
        BGT (Branch if Greater Than)
    """
    mnemonics = ['bgt']

    machine_codes = \
            {
                'rel': [0x92],
            }

class BHCC(HCS08_Simple_Branch):
    """
        BHCC (Branch if Half Carry Clear)
    """
    mnemonics = ['bhcc']

    machine_codes = \
            {
                'rel': [0x28],
            }

class BHCS(HCS08_Simple_Branch):
    """
        BHCS (Branch if Half Carry Set)
    """
    mnemonics = ['bhcs']

    machine_codes = \
            {
                'rel': [0x29],
            }

class BHI(HCS08_Simple_Branch):
    """
        BHI (Branch if Higher)
    """
    mnemonics = ['bhi']

    machine_codes = \
            {
                'rel': [0x22],
            }


class BHS(HCS08_Simple_Branch):
    """
        BHS (Branch if Higher or Same)
    """
    mnemonics = ['bhs']

    machine_codes = \
            {
                'rel': [0x24],
            }

class BIH(HCS08_Simple_Branch):
    """
        BIH (Branch if IRQ Pin High)
    """
    mnemonics = ['bih']

    machine_codes = \
            {
                'rel': [0x2F],
            }

class BIL(HCS08_Simple_Branch):
    """
        BIL (Branch of IRQ Pin Low)
    """
    mnemonics = ['bil']

    machine_codes = \
            {
                'rel': [0x2E],
            }

class BIT(HCS08_Instruction):
    """
        BIT (Bit Test)
        Calculates what the flags would be if the accumulator was AND'd with the operand.
    """
    mnemonics = ['bit']

    machine_codes = \
            {
                'imm': [0xA5],
                'dir': [0xB5],
                'ext': [0xC5],
                'ix2': [0xD5],
                'ix1': [0xE5],
                'ix' : [0xF5],
                'sp2': [0x9E, 0xD5],
                'sp1': [0x9E, 0xE5],
            }


class BLE(HCS08_Simple_Branch):
    """
        BLE (Branch if Less than or Equal)
    """
    mnemonics = ['ble']

    machine_codes = \
            {
                'rel': [0x93],
            }

class BLO(HCS08_Simple_Branch):
    """
        BLO (Branc if Lower)
    """
    mnemonics = ['blo']

    machine_codes = \
            {
                'rel': [0x25],
            }

class BLS(HCS08_Simple_Branch):
    """
        BLS (Branch if Lower or Same)
    """
    mnemonics = ['bls']

    machine_codes = \
            {
                'rel': [0x23],
            }

class BLT(HCS08_Simple_Branch):
    """
        BLT (Branch if Less Than)
    """
    mnemonics = ['blt']

    machine_codes = \
            {
                'rel': [0x91],
            }

class BMC(HCS08_Simple_Branch):
    """
        BMC (Branch if Interrupt Mask is Clear)
    """
    mnemonics = ['bmc']

    machine_codes = \
            {
                'rel': [0x2C],
            }

class BMI(HCS08_Simple_Branch):
    """
        BMI (Branch if Minus)
    """
    mnemonics = ['bmi']

    machine_codes = \
            {
                'rel': [0x2B],
            }

class BMS(HCS08_Simple_Branch):
    """
        BMS (Branch if Interrupt Mask is Set)
    """
    mnemonics = ['bms']

    machine_codes = \
            {
                'rel': [0x2D],
            }

class BNE(HCS08_Simple_Branch):
    """
        BNE (Branch if Not Equal)
    """
    mnemonics = ['bne']

    machine_codes = \
            {
                'rel': [0x26],
            }

class BPL(HCS08_Simple_Branch):
    """
        BPL (Branch of IRQ Pin Low)
    """
    mnemonics = ['bpl']

    machine_codes = \
            {
                'rel': [0x2A],
            }

class BRA(HCS08_Simple_Branch):
    """
        BRA (Branch Always)
    """
    mnemonics = ['bra']

    machine_codes = \
            {
                'rel': [0x20],
            }

class BRCLR(HCS08_Bit_Branch):
    """
        BRCLR (Branch if Bit Clear)
        Branches if the given bit is clear.
    """
    mnemonics = ['brclr']

    machine_codes = \
            {
                'dirb0': [0x01],
                'dirb1': [0x03],
                'dirb2': [0x05],
                'dirb3': [0x07],
                'dirb4': [0x09],
                'dirb5': [0x0B],
                'dirb6': [0x0D],
                'dirb7': [0x0F],
            }

class BRN(HCS08_Simple_Branch):
    """
        BRN (Branch Never)
        Branches if I=0.
    """
    mnemonics = ['brn']

    machine_codes = \
            {
                'rel': [0x21],
            }


class BRSET(HCS08_Bit_Branch):
    """
        BRSET (Branch if Bit Set)
        Branches if the given bit is set.
    """
    mnemonics = ['brset']

    machine_codes = \
            {
                'dirb0': [0x00],
                'dirb1': [0x02],
                'dirb2': [0x04],
                'dirb3': [0x06],
                'dirb4': [0x08],
                'dirb5': [0x0A],
                'dirb6': [0x0C],
                'dirb7': [0x0E],
            }


class BSET(HCS08_Bit_Operation):
    """
        BSET (Set Bit)
    """
    mnemonics = ['bset']

    machine_codes = \
            {
                'dirb0': [0x10],
                'dirb1': [0x12],
                'dirb2': [0x14],
                'dirb3': [0x16],
                'dirb4': [0x18],
                'dirb5': [0x1A],
                'dirb6': [0x1C],
                'dirb7': [0x1E],
            }


class BSR(HCS08_Simple_Branch):
    """
        BSR (Branch to Subroutine)
        Branches if I=0.
    """
    mnemonics = ['brn']

    machine_codes = \
            {
                'rel': [0x21],
            }

    #TODO: override execute


class CBEQ(HCS08_Instruction_with_AXInherent):
    """
        Compare and Branch if Equal
    """
    mnemonics = ['cbeq', 'cbeqa', 'cbeqx']

    machine_codes = \
            {
                'dir':  [0x31],
                'inha': [0x41],
                'inhx': [0x51],
                'ix1': [0x61], #technically is also post-increment
                'ix' : [0x71], #technically is also post-increment
                'sp1':  [0x9E, 0x61],
            }

    @classmethod
    def assemble(cls, tokens, symbol_list, assembler):

        #get the machine code for the simple
        base = super(CBEQ, cls).assemble(tokens, symbol_list, assembler)

        #and append the branch offset
        target = parse_operand(tokens['target'], symbol_list)

        #if we were provided a label, rather than an offset, calculate an offset
        if not is_numeric(tokens['target']):
            offset = (target - (assembler.location + len(base) + 1)) % 255 #location = base location + the length of the instruction without the rel, + 1(the length of the rel)

        #append the new offset to the existing instruction
        base.append(offset)

        #and return it
        return base
