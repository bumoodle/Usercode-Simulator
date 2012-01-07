"""
    UserCode simulator
    by Kyle Temkin <ktemkin@binghamton.edu>

    HSC08 Assembly Language definition & helper routines

    (c) Binghamton University, 2012
    Licensed under the GPL, the GNU Public License, V3.0

"""

#debug
import pprint


import inspect, string
import Instructions

from pyparsing import alphas, alphanums, Word, Optional, Literal, oneOf, nums, Group, restOfLine, CaselessLiteral, dblQuotedString, delimitedList, ParseException

class InvalidMnemonicException(Exception): #FIXME: should extend UserCodeException
    pass

def get_all_mnemonics(predicate = lambda : True):
    """
        Returns all valid assembly mnemonics, as defined in the instruciton set.
    """

    #start with an empty collection of mnemonics
    mnemonics = ''

    #for each class in the Instructions module
    for _, instruction in inspect.getmembers(Instructions, lambda x : inspect.isclass(x)):

        if issubclass(instruction, Instructions.HCS08_Operation) and predicate:

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
    true_mnemonic = oneOf(get_all_mnemonics(), caseless=True)

    #define the core assembler psueod-ops
    dc_pseudo_op = oneOf('dc dc.b dc.w', caseless=True)
    ds_pseudo_op = oneOf('ds ds.b ds.w', caseless=True)
    pseudo_op = CaselessLiteral('equ') | CaselessLiteral('org') | dc_pseudo_op | ds_pseudo_op

    #generic mnemonic
    mnemonic = true_mnemonic | pseudo_op

    #
    # Basic assembler token definitions
    #

    hexdigits = nums + "abcdefABCDEF"

    #number, in one of the supported formats, (hex, binary, octal)
    number = Group(Optional(oneOf('$ % @')) + Word(hexdigits + '-', hexdigits))

    #numbers which can be used as an argument to BCLR and similar
    bitNumber = oneOf('0 1 2 3 4 5 6 7')

    #assembler labels
    label = ~(mnemonic) +  Word(alphas, alphanums + '_') + Optional(Literal(':')).suppress()

    #reference to a previously defined label
    reference = Word(alphas, alphanums + '_')

    #todo: allow processing of numeric literals
    operand = number ^ reference


    #
    # Instruction Suffixes, which specify the acceptable arguments to the various instruction classes
    #

    #immediate addressing
    immediate_suffix = Literal('#').suppress() + operand.setResultsName('immediate')

    #immediate and extended addressing
    direct_suffix = operand.setResultsName('direct')

    #indexed (and indexed offset) addressing
    indexed_suffix = Optional(operand).setResultsName('index_offset') + Literal(',').suppress() + (CaselessLiteral('X+').setResultsName('index') | CaselessLiteral('X').setResultsName('index'))

    #stack offset addressing
    stack_suffix = operand.setResultsName('stack_offset') + Literal(',').suppress() + CaselessLiteral('SP').suppress();

    #numbered bit suffix
    bit_suffix = bitNumber.setResultsName('bit') + Literal(',').suppress() + operand.setResultsName('direct')

    #loop primitive
    branch_suffix = (indexed_suffix | immediate_suffix | direct_suffix | stack_suffix | bit_suffix) + Literal(',').suppress() + operand.setResultsName('target')

    #
    # Pseudo-Op Suffix
    #

    #constants, as allowed by the assembler: numbers, double-quoted strings, and single-quoted characters
    constant = number | Group(dblQuotedString) | Group(Literal("'") + Word(string.printable, max=1) + Literal("'").suppress())

    #suffix for define constant- a comma-delimited list of constants
    dc_suffix = delimitedList(constant).setResultsName('defined')

    #
    # Core parsing definitions
    #

    #comments start at a semicolon, and emcompass the rest of the line
    comment = Literal(';') + restOfLine

    #core instruction definition
    instruction = mnemonic.setResultsName('mnemonic') + Optional(branch_suffix | indexed_suffix | immediate_suffix | direct_suffix | stack_suffix | bit_suffix | dc_suffix)

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
            print "Invalid syntax!"
            print e.markInputline('--!--');
            #raise new InvalidSyntaxError()

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


    def process_code(self, code):

        #if we were passed the code listing as a string, split it into lines
        if isinstance(code, str):
            code = code.split('\n')

        #first pass: syntax analysis- breaks the code into sytax "tokens"
        code = [Tokens.tokenize(line) for line in code]

        #second pass: generate machine code from the assembly, with a few labels left unresolved
        for line in code:
            self.process_line(line)

        #third pass: resolve all of the symbols into concrete numeric values, so what's left is true machine code
        #TODO


        #DEBUG

        print "SYMBOLS:"

        for i in self.symbols:
            print i + '\t\t', hex(self.symbols[i])

        print "\n\nFLASH:"

        for i in self.flash:
            if self.flash[i] != 0:
                print hex(i) + '\t\t', hex(self.flash[i])



    def process_line(self, tokens):
        """
            Processes a single line of HCS08 assembly code, performing a first-pass assembly.
        """

        #break the line into tokens
        #tokens = Tokens.tokenize(line)

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




def assemble_into_flash(flash_memory, code):
    """
        Assembles a given code listing (string, or list of lines) into HCS08 assembly, and populates the flash memory (address => value dictionary) provided.
    """

    #convert the code into tokens
    code = [Tokens.tokenize(line) for line in code]

    #create a new assembler object
    asm = Assembler(flash_memory)




def get_operation_by_mnemonic(mnemonic):

    #for each defined operation
    for _, instruction in inspect.getmembers(Instructions, lambda x : inspect.isclass(x)):

        #if the given instruction is a HCS08 operation represented by the given mnemonic
        if issubclass(instruction, Instructions.HCS08_Operation) and mnemonic.lower() in instruction.mnemonics:

            #return the class
            return instruction
