"""
    UserCode simulator
    by Kyle Temkin <ktemkin@binghamton.edu>

    HSC08 Assembly Language definition & helper routines

    (c) Binghamton University, 2012
    Licensed under the GPL, the GNU Public License, V3.0

"""

import sys
sys.path.append('../..');

from Computers.Exceptions import UserCodeException


import inspect, string
import Instructions


from pyparsing import alphas, alphanums, Word, Optional, Literal, oneOf, nums, Group, restOfLine, CaselessLiteral, ZeroOrMore, ParseException, opAssoc, operatorPrecedence, ParserElement

#enable packrat parsing for better performance
ParserElement.enablePackrat()



class InvalidMnemonicException(UserCodeException):
    """
        Standard exception which indicates that the user provided a mnemonic not supported by this computer.
    """
    pass

class UnresolvedSymbolException(UserCodeException):
    """
        Exception which indicates the user provided a symbol whose value could not be determined; such as a constant they forgot to define.
    """
    pass

class DisallowedInstructionException(UserCodeException):
    """
        Exception which indicates the user tried to use an instruction which was either part of the blacklist, or not part of the whitelist.
    """
    pass

class RequiredInstructionMissing(UserCodeException):
    """
        Exception which indicates the user failed to use one of the instructions in the requiredlist.
    """
    pass

class InvalidSyntaxException(UserCodeException):
    """
        Exception which indicates a syntax error in the user code.
    """
    pass



def get_all_mnemonics(predicate = lambda x : True):
    """
        Returns all valid assembly mnemonics, as defined in the instruciton set.
    """

    #start with an empty collection of mnemonics
    mnemonics = ''

    #for each class in the Instructions module
    for _, instruction in inspect.getmembers(Instructions, lambda x : inspect.isclass(x)):

        if issubclass(instruction, Instructions.HCS08_Operation) and predicate(instruction):

            #add each valid mnemonic to the list
            for mnemonic in instruction.mnemonics:
                mnemonics += mnemonic + ' '

    #and return the completed list of mnemonics, without the final space
    return mnemonics[:-1]


class Tokens:
    """
        A collection of PyParsing tokens
    """
    #
    # Instructions and pseudo-op definitions
    #

    #true instruction mnemonics
    mnemonic = oneOf(get_all_mnemonics(lambda cls : not issubclass(cls, Instructions.DC)), caseless=True)

    #dc psueod-op mnemonics
    dc_pseudo_op = CaselessLiteral('dc.w') | CaselessLiteral('dc.b') | CaselessLiteral('dc')

    #
    # Basic assembler token definitions
    #

    hexdigits = nums + "abcdefABCDEF"

    #number, in one of the supported formats, (hex, binary, octal)
    number = Group(Optional(oneOf('$ % @')) + Word(hexdigits + '-', hexdigits))

    #numbers which can be used as an argument to BCLR and similar
    bitNumber = oneOf('0 1 2 3 4 5 6 7')

    #assembler labels
    label = ~(mnemonic) + ~(dc_pseudo_op) +  Word(alphas, alphanums + '_') + Optional(Literal(':')).suppress()

    #reference to a previously defined label
    reference = ~CaselessLiteral('SP') + ~CaselessLiteral('X') + Word(alphas, alphanums + '_')

    #todo: allow processing of numeric literals
    operand = number ^ reference

    #
    # Basic expressions, which can be used to evaluate things like ~RAMSTART or RAMSTART+1
    #

    #the operations allowed in an ASM expression
    allowed_opers = \
        [
            (Literal('-'), 1, opAssoc.RIGHT), #sign
            (Literal('~'), 1, opAssoc.RIGHT), #bitwise inversion
            (oneOf('<<  >>'), 2, opAssoc.LEFT), #bit shift operators
            (oneOf('* /'), 2, opAssoc.LEFT), #multiplication, division, and bitwise AND
            (oneOf('| ^'), 2, opAssoc.LEFT), #bitwise OR, bitwise XOR
            (Literal('%'), 2, opAssoc.LEFT), #modulus operator
            (oneOf('+ -'), 2, opAssoc.LEFT) #addition, subtraction
        ]

    #a recursive grammar which allows operations to be performed on numbers
    operand_expression = operatorPrecedence(operand, allowed_opers)


    #
    # Instruction Suffixes, which specify the acceptable arguments to the various instruction classes
    #

    #immediate addressing
    immediate_suffix = Literal('#').suppress() + operand_expression.setResultsName('immediate')

    #immediate and extended addressing
    direct_suffix = operand_expression.setResultsName('direct')

    #indexed (and indexed offset) addressing
    indexed_suffix = Optional(operand_expression).setResultsName('index_offset') + Literal(',').suppress() + (CaselessLiteral('X+').setResultsName('index') | CaselessLiteral('X').setResultsName('index'))

    #stack offset addressing
    stack_suffix = operand_expression.setResultsName('stack_offset') + Literal(',').suppress() + CaselessLiteral('SP').suppress();

    #numbered bit suffix
    bit_suffix = bitNumber.setResultsName('bit') + Literal(',').suppress() + operand_expression.setResultsName('direct') + Literal(',').suppress() + operand_expression.setResultsName('target')

    #loop primitive / move suffixes
    branch_suffix = (indexed_suffix | immediate_suffix | stack_suffix | direct_suffix) + Literal(',').suppress() + operand_expression.setResultsName('target')

    #
    # Pseudo-Op Suffix
    #

    #constants, as allowed by the assembler: numbers, double-quoted strings, and single-quoted characters
    constant = operand_expression | Group(Literal('"') + Word(string.printable.replace('"', '')) + Literal('"').suppress()) | Group(Literal("'") + Word(string.printable.replace("'", ''), max=1) + Literal("'").suppress())

    #suffix for define constant- a comma-delimited list of constants
    dc_suffix = Group(constant + ZeroOrMore(Literal(',').suppress() + constant)).setResultsName('defined')

    #
    # Core parsing definitions
    #

    #comments start at a semicolon, and emcompass the rest of the line
    comment = Literal(';') + restOfLine

    #core instruction definition
    non_dc_instruction = mnemonic.setResultsName('mnemonic') + Optional(bit_suffix | branch_suffix | indexed_suffix | immediate_suffix | stack_suffix | direct_suffix )

    #define constant definition
    dc_instruction = dc_pseudo_op.setResultsName('mnemonic') + dc_suffix

    #allow a line to contain either a normal instruction _or_ a define constant instruction
    instruction = dc_instruction | non_dc_instruction

    #a normal line of ASM, which may include labels or comments
    asm_line = (Optional(label).setResultsName('label') + Optional(instruction) + Optional(comment).suppress()) | comment.suppress()

    #definition for a whole ASM file
    asm_file = asm_line | comment.suppress()


    @classmethod
    def tokenize(cls, asm):
        """
            Breaks a singe line of ASM into its component tokens, for parsing.
            Returns a dictionary of token type => token value.
        """

        #if the given line is blank, return an empty dictionary
        if not asm.strip():
            return {}

        #parse the given string using pyparsing
        try:
            return cls.asm_line.parseString(asm, parseAll=True).asDict()
        except ParseException as e:
            raise InvalidSyntaxException('A syntax error exists in your code, near: ' + e.markInputline('--!--') + '.')

class Assembler(object):

    def __init__(self, flash_memory):
        """
            Initializes a new Assembler object.
        """

        #initialize the location counter to the start of flash
        self.location = min(flash_memory)

        #create a new symbol table
        self.symbols = {}

        #create a new dictionary which will store any not-yet-defined reference
        #for possible replacement during a second-pass
        self.undefined = {}

        #store a referece to the flash memory in quesiton
        self.flash = flash_memory

        #initialize the blacklists, whitelist, and required list
        self.blacklist = []
        self.whitelist = []
        self.requiredlist = {}


    @staticmethod
    def _parse_operand_list(oplist):

        #switch the list to upper case
        oplist = oplist.upper()

        #replace all commas with whitespace, so we can accept comma-delimited lists:
        oplist.replace(',', ' ')

        #and split the list into names of operands
        return oplist.split()


    def set_blacklist(self, blacklist):
        """
            Establishes a list of instructions not allowed in the user program.

            Blacklist should be a space or comma-delimited list of instruction class names.
        """

        self.whitelist = []
        self.blacklist = self._parse_operand_list(blacklist)


    def set_whitelist(self, whitelist):
        """
            Establishes a list of instructions allowed in the user program; all other instructions will be disallowed.

            Whitelist should be a space or comma-delimited list of instruction class names.
        """

        self.blacklist = []
        self.whitelist = self._parse_operand_list(whitelist)

    def set_required(self, required):
        """
            Establishes a list of instructions which must be used by the user program.
            All of the given instructions _must_ be used. Resets the existing required list when called.

            Required list should be a space or comma-delimited list of instruction class names.
        """

        #parse the required list
        required_list = self._parse_operand_list(required)

        #and use it create a dictionary of required elements
        self.requiredlist = {i : False for i in required_list}


    def process_code(self, code, terminate_with_stop=False, enforce_required=False):
        """

        """

        #if we were passed the code listing as a string, split it into lines
        if isinstance(code, str):
            code = code.split('\n')

        #first pass: syntax analysis- breaks the code into sytax "tokens"
        code = [Tokens.tokenize(line) for line in code]

        #second pass: generate machine code from the assembly, with a few labels left unresolved
        for line in code:
            self.process_line(line)

        #third pass: resolve all of the symbols into concrete numeric values, so what's left is true machine code
        self.resolve_symbols(self.flash)

        #if requested, add a STOP operation, to prevent execution from "flowing" past the end of the program
        if terminate_with_stop:
            self.add_stop()

        #if we've been asked to enfore the requiredlist, do so:
        if enforce_required:

            #if any of the required instructions have not been used, raise an exception
            for instr in self.requiredlist:
                if not self.requiredlist[instr]:
                    raise RequiredInstructionMissing('Your instructor has specified that you must use the ' + repr(instr) + ' instruction, which you did not use.')


    def add_stop(self):
        """
            Adds a STOP operand at the current location, which indicates to the CPU that execution should be halted.
        """

        self.flash[self.location] = Instructions.STOP.machine_codes['inh'][0]
        self.location += 1


    def resolve_symbols(self, program):
        """
            Resolves any unknown symbols in a given block of program memory (a dictionary).
        """

        #for each byte in the program memory
        for byte in program:

            try:

                #if the given value is a placeholder for a single-byte symbol
                if isinstance(program[byte], str):

                    #try and use the value from the symbols table
                    program[byte] = self.symbols[program[byte]]

                #if the given value is placeholder for part of a mutli-byte symbol
                elif isinstance(program[byte], tuple):

                    #extract the symbol name and byte number
                    symbol_name, byte_num = program[byte]

                    #if we have a relative placeholder, then compute the correct offset
                    if isinstance(byte_num, tuple):

                        #unpack the relative base, which indicates which address the offset should be calculated with respect to
                        _, base = byte_num

                        #replace the placeholder with the amount that would need to be added to base to reach the correct value
                        program[byte] = (self.symbols[symbol_name] - base)

                        #if we're trying to resolve a branch to an address which is out of range, raise an exception
                        if program[byte] > 127 or program[byte] < -128:
                            raise InvalidBranchException('Could not branch to the label ' + symbol_name + 'from address ' + hex(base) + ', as it was out of branch range. See your textbook for more information.')

                        #otherwise, format the value to fit within a byte by placing the value in two's compliment notation
                        else:
                            program[byte] = program[byte] % 256


                    #otherwise, we have a placeholder which repesents part of a multi-byte value
                    else:

                        #and try to use the appropriate section from a multi-byte value specified in the symbols table
                        program[byte] = (self.symbols[symbol_name] >> (8 * byte_num)) & 0xFF;

            except KeyError as e:
                raise UnresolvedSymbolException('Could not determine the value of the symbol ' + repr(e.args[0]) + '. Are you sure you defined it?')


    def process_line(self, tokens):
        """
            Processes a single line of HCS08 assembly code, performing a first-pass assembly.
        """

        #if the line contains no code (i.e. was a line of comments, or a blank line), we don't need to do anything
        if not tokens:
            return

        #if the token has a label, add the current location to the symbol table
        if 'label' in tokens:
            self.symbols[tokens['label'][0]] = self.location

        #if there's no (pseudo)operation on the line, don't process the rest of it
        if not 'mnemonic' in tokens:
            return;

        #get the correct operation class by mnemonic
        op = get_operation_by_mnemonic(tokens['mnemonic'])

        #get a quick reference to the operation's shorthand name
        shorthand = op.shorthand()

        #if the operation has been blacklisted (or isn't whitelisted, if a whitelist was provided) raise an exception
        if shorthand in self.blacklist or (self.whitelist and shorthand not in self.whitelist):
            raise DisallowedInstructionException('Your instructor has chosen to disallow the ' + shorthand + ' instruction. You will need to solve this problem in a different way.')

        #if the operation is in the requiredlist, mark it as used
        if shorthand in self.requiredlist:
            self.requiredlist[shorthand] = True

        #if we didn't get a valid operation, throw an error
        if op is None or not issubclass(op, Instructions.HCS08_Operation):
            raise InvalidMnemonicException('Mnemonic ' + tokens['mnemonic'] + ' does not exist, or is unsupported.')

        # Perform any state modifications required for this particular operation.
        #
        # This is mostly used by psuedo-operations, to perform tasks like adding to the symbol table, including files, or adjusting
        # the location counter.
        #
        op.modify_state(self, tokens)

        # Perform the core assembly, itself. Returns the machine code for the assembled instruction.
        machine_code = op.assemble(tokens, self.symbols, self)

        #for each byte in the machine code
        for byte in machine_code:

            #write the given byte to flash at the location indicated by the location counter
            self.flash[self.location] = byte

            #then, increase the location counter
            self.location += 1






def get_operation_by_mnemonic(mnemonic):

    #for each defined operation
    for _, instruction in inspect.getmembers(Instructions, lambda x : inspect.isclass(x)):

        #if the given instruction is a HCS08 operation represented by the given mnemonic
        if issubclass(instruction, Instructions.HCS08_Operation) and mnemonic.lower() in instruction.mnemonics:

            #return the class
            return instruction

