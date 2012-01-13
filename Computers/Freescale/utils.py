"""
    Simple utility methods for the HCS08 assembler.

    Copyright (c) 2012, Binghamton University
    Kyle Temkin <ktemkin@binghamton.edu>

"""

import operator, collections
from pyparsing import ParseResults

class ExpressionEvaluator:
    """
        Helper class for evaluating constant expressions embedded in ASM code.
    """

    #right-associative unary operators
    unary_operators = \
        {
            '-': operator.neg,
            '~': operator.invert
        }

    #binary operators
    binary_operators = \
        {
            '^':  operator.xor,
            '*':  operator.mul,
            '/':  operator.floordiv,
            '+':  operator.add,
            '-':  operator.sub,
            '%':  operator.mod,
            '<<': operator.lshift,
            '>>': operator.rshift
        }

    @classmethod
    def unary_op(cls, operator, operand):
        """
            Convenience method which applies the appropriate unary operator from the dictionary above.
        """
        return cls.unary_operators[operator](operand)

    @classmethod
    def binary_op(cls, operator, a, b):
        """
            Convenience method which applies the appropriate binary operator from the dictionary above.
        """
        return cls.binary_operators[operator](a, b)

    @classmethod
    def requisites_known(cls, expr, symbol_list):

        #if the subexpression is numeric, it has no requisites; continue
        if is_numeric(expr) or isinstance(expr, int):
            return True

        #if the subexpression is an operator, it has no requisites
        if expr in cls.binary_operators or expr in cls.unary_operators:
            return True

        #if we have a list
        if (isinstance(expr, list) or isinstance(expr, ParseResults)):

            #check each of the sub-items
            for subexpr in expr:
                if not cls.requisites_known(subexpr, symbol_list):
                    return False

            #if none of them failed, we know  all of the requisites
            return True

        #if we have non-list element, check to see if it is in the symbol table
        if expr in symbol_list:
            return True

        #if we failed all of the checks above, some outstanding information is required
        return False



    @classmethod
    def eval(cls, expr, symbol_list):
        """
            Parse a given expression as much as is possible.
            If successful, yields an integer. If not, yields the input array, for later parsing.
        """

        #base case: expression is an element from the symbols list; return the value from the list
        if isinstance(expr, collections.Hashable) and  expr in symbol_list:
            return symbol_list[expr]



        #base case: expression is already fully reduced; return it
        if not isinstance(expr, ParseResults) and not isinstance(expr, list):
            return expr

        #base case: expression is a list-encoded number; parse it, and return
        if is_numeric(expr):
            return parse_number(expr)

        #base case: we lack the requisite symbols to parse this; so return it whole
        if not cls.requisites_known(expr, symbol_list):
            return expr

        #recursive case: we have a list with only a single element- parse the interior
        if len(expr) == 1:
            return cls.eval(expr[0])

        #recursive case: we have a unary operator
        if len(expr) > 1 and expr[0] in cls.unary_operators:

            #recurse, evaluating the target of the operator
            operand = cls.eval(expr[1], symbol_list)

            #then, apply the operator itself
            return cls.unary_op(expr[0], operand)

        #recursive case: we have a binary operator
        if len(expr) > 2 and expr[1] in cls.binary_operators:

            #rercuse, evaluating the _both_ targets of the operator
            lhs, rhs = cls.eval(expr[0], symbol_list), cls.eval(expr[2], symbol_list)

            #then, apply the operator to the result
            return cls.binary_op(expr[1], lhs, rhs)

        #base case: we weren't able to process the expression any further, so it must be as reduced as possible
        return expr


def parse_number(n):
    """
        Interprets a tokenized number as raw, decimal value.
    """

    #the HCS08 base prefixes
    base = {'$': 16, '%': 2, '@':8 }

    #if the number has a base prefix, then interpret the number given the appropriate base
    if len(n) == 2:
        return int(n[1], base[n[0]])

    #otherwise, assume it's decimal
    else:
        return int(n[0])

def is_numeric(tokens):

    """ Quick function to determine if a given (valid) token represents a number. """

    try:
        return (len(tokens) == 2 and tokens[0] in ('$', '%', '@')) or (len(tokens) == 1 and int(tokens[0]) is not None)

    except ValueError:
        return False
    except TypeError:
        return False



def sized_operation(operand, size, operation):

    #if we don't have a size to fit to, return the operand
    if size is None:
        return operand

    #otherwise, create an empty array of bytes
    raw_bytes = []

    #for each requested byte, in big-endian order
    for i in range(size, 0, -1):

        #add an integer composed of only the relevant bits
        raw_bytes.append(operation(operand, i))

    #return the raw byte count
    return raw_bytes


def fit_to_size(operand, size=None):
    """
        Fits a given number into a list of size bytes, where size is an integer.
        Will pad or truncate if necessary.

        For example, fit_to_size(65535, 3) would return [0, 255, 255], and fit_to_size(65535, 1) would return [255].
        fit_to_size(n, 0) always returns an empty list; fit_to_size(n, None) returns n.

    """
    return sized_operation(operand, size, lambda o, i : (o >> (i-1)*8) & 0xFF)

def sized_placeholder(operand, size=None):
    """
        Returns a placeholder which is size elements long, or operand is size is None.
    """
    return sized_operation(operand, size, lambda o, i : (o, i - 1))



def parse_operand(op, symbols_list, size=None):
    """
        Interprets an operand, which could be a reference, or expression.

        If a size N is provided, the returned result will be a list of size bytes.

    """

    #if the operand is in the form of an expression, reduce it as much as possible
    #(this usually reduces to solid number)
    op = ExpressionEvaluator.eval(op, symbols_list)

    #if the operand reduced to a number, fit it to the desired size, and return it
    if isinstance(op, int):
        return fit_to_size(op, size)

    #otherwise, generate a suitibly sized placeholder
    else:
        return sized_placeholder(op, size)


def split_word(number):
    """
        Convenience method which calls fit_to_size, with a size of 2.

        Splits a given word into two bytes, discarding any bits past the 16th.
        Bytes are returned in big-endian order [msB, lsB].
    """

    return fit_to_size(number, 2)

def merge_word(msb, lsb):
    """
        Merges two bytes into a 16-bit word.
    """

    return msb << 8 | lsb;

def sign_extend(lsb):
    """
        Sign-extends a given byte into a 16-bit word, so signed numbers retain their value. For example, -1 (0xFF) would remain -1 (0xFFFF).
    """

    #if the number is negative, one-extend it
    if lsb > 128:
        return 0xFF00 | lsb;

    #otherwise, zero-extend it
    else:
        return lsb;

