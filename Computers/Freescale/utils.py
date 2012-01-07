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



def parse_operand(op, symbols_list):
    """
        Interprets an operand, which could be a reference, or expression.

        TODO: handle expressions
    """

    #if the operand is numeric, return its value
    if is_numeric(op):
        return parse_number(op)

    #otherwise, look for it in the symbols list
    elif op in symbols_list:
        return symbols_list[op]

    #if it's not there, return the operand name
    else:
        return op


def split_word(number):
    """
        Splits a given word into two bytes, discarding any bits past the 16th.
        Bytes are returned in big-endian order [msB, lsB].
    """

    #retrieve the most and least significant bytes
    msb = number >> 16 & 0xFF;
    lsb = number & 0xFF;

    #returns a new list
    return [msb, lsb]

