#!/usr/bin/env python2
"""
    UserCode simulator
    by Kyle Temkin <ktemkin@binghamton.edu>

    (c) Binghamton University, 2012
    Licensed under the GPL, the GNU Public License, V3.0

"""

import sys
import argparse

#debug:
from pyparsing import ParseException
from Computers.Freescale.HCS08 import MC9S08QG8;

def main():
    """

    """

    #create a new argument parser, which parses the command line arguments
    parser = argparse.ArgumentParser(description='Runs abstract pieces of a user code on one of various turing-equivalent machiness.')

    #if an input file is specified, use it; otherwise, default to the standard input
    parser.add_argument('codefile', metavar='codefile', nargs='?', type=argparse.FileType('r'), default=sys.stdin, help='The file containing the user code to be executed.')
    parser.add_argument('--system', '-s', type=str, default='hcs08', help='The system on which the user code will be executed.')

    #parse this argument's processes
    args = parser.parse_args()

    #get the user code as a list of of lines
    code = args.codefile.read()

    #if a generic HCS08 is specified, choose the MC9S08QG8
    if args.system in ('hcs08', 'MC9S08QG8'):
        system = MC9S08QG8(code);

    print system

    system.step()
    print system

    system.step()
    print system

    system.step()
    print system



#if this is being called as a library, call main
if __name__ == '__main__':
    main()
