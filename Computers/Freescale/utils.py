"""
    Simple utility methods for the HCS08 assembler.

    Copyright (c) 2012, Binghamton University
    Kyle Temkin <ktemkin@binghamton.edu>

"""


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


def parse_expression(expr, symbols_list):
    """
        Parse a given expression as much as is possible.
        If successful, yields an integer. If not, yields the input array, for later parsing.
    """

    #base case: expression is an element from the symbols list; return the value from the list
    if expr in symbols_list:
        return symbols_list[expr]

    #base case: expression is already fully reduced; return it
    if not isinstance(expr, list):
        return expr

    #base case: expression is a list-encoded number; parse it, and return
    if is_numeric(expr):
        return parse_number(expr)




def parse_operand(op, symbols_list, size=None):
    """
        Interprets an operand, which could be a reference, or expression.

        If a size N is provided, the returned result will be a list of size bytes.

        TODO: handle expressions
    """

    #if the operand is in the form of an expression, reduce it as much as possible
    #(this usually reduces to solid number)
    op = parse_expression(op, symbols_list)

    #if the operand reduced to a number, fit it to the desired size, and return it
    if isinstance(op, int):
        return fit_to_size(op, size)

    #otherwise, generate a suitibly sized placeholder
    else:
        return sized_placeholder(op, size)

    ##UNREACHED: old code

    #if the operand is numeric, return its value
    if is_numeric(op):
        return fit_to_size(parse_number(op), size)

    #otherwise, look for it in the symbols list
    elif op in symbols_list:
        return fit_to_size(symbols_list[op], size)

    #if it's not there, return the operand name
    else:
        return sized_placeholder(op, size)


def split_word(number):
    """
        Convenience method which calls fit_to_size, with a size of 2.

        Splits a given word into two bytes, discarding any bits past the 16th.
        Bytes are returned in big-endian order [msB, lsB].
    """

    return fit_to_size(number, 2)


