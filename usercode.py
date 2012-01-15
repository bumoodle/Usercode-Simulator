#!/usr/bin/env python2
"""
    UserCode simulator
    by Kyle Temkin <ktemkin@binghamton.edu>

    (c) Binghamton University, 2012
    Licensed under the GPL, the GNU Public License, V3.0

"""

import sys
import argparse
import readline

#debug:
from pyparsing import ParseException
from Computers.Freescale.HCS08 import MC9S08QG8;

active = ''
prompt = '{active}>'
""" Interactive mode command prompt. """

def main():
    """

    """

    global active

    #create a new argument parser, which parses the command line arguments
    parser = argparse.ArgumentParser(description='Runs abstract pieces of a user code on one of various turing-equivalent machiness.')

    #if an input file is specified, use it; otherwise, default to the standard input
    #parser.add_argument('codefile', metavar='codefile', nargs='?', type=argparse.FileType('r'), default=sys.stdin, help='The file containing the user code to be executed.')
    parser.add_argument('--system', '-s', type=str, default='hcs08', help='The system on which the user code will be executed.')

    #parse this argument's processes
    args = parser.parse_args()

    #get the user code as a list of of lines
    #code = args.codefile.read()

    #if a generic HCS08 is specified, choose the MC9S08QG8
    if args.system in ('hcs08', 'MC9S08QG8'):
        system = MC9S08QG8();

    #set the active system title
    active = system.shorthand()

    #run the mainloop indefinitely
    mainloop(system)

def mainloop(system):

    #initialize the blacklist, whitelist, and requiredlist
    blacklist = whitelist = requiredlist = None

    #until we're manually interrupted, run interactive mode indefinitely
    while True:

        #get a command from the user
        command, argument = get_command()

        #if the q command was offered, quit the program
        if command in ('q', 'quit'):
            quit()

        #upon the 'code' command, read in the user code
        elif command in ('c', 'code'):
            read_program(system, argument, blacklist, whitelist, requiredlist)

        #step, when requested
        elif command in ('s', 'step'):
            system.step()

        #print a serialized version of the system's state
        elif command in ('g', 'getstate'):
            print system.serialize_state()

        elif command in ('l', 'loadstate'):
            system.unserialize_state(argument)

        #reset the system, when requested
        elif command in ('r', 'reset'):
            system.reset()

        #set the system blacklist (interpreted by the system)
        elif command in ('bl', 'blacklist'):
            blacklist = argument

        #set the system whitelist (interpeted by the system)
        elif command in ('wl', 'whitelist'):
            whitelist = argument

        #set the system require-list (interpreted by the system)
        elif command in ('rl', 'require'):
            requiredlist = argument



def read_program(system, terminator, blacklist=None, whitelist=None, requiredlist=None):
    """
        Handles reading a program from the standard input, then passes the code to the system, to process.

        Program is considered over when the 'terminator' sequence appears on a line by itself. The terminator should be chosen so it does not appear in the program.
    """

    #create an empty buffer, which will store lines of code
    program = []

    #and keep track of the most recently entered line
    last = None

    #read until break
    while True:

        #read a single line from the standard input
        last = raw_input()

        #if we've recieved the terminating line, stop reading
        if last == terminator:
            break;

        #otherwise add the line to the program, and continue
        program.append(last)

    #pass the program code into the system
    system.load_program(program, blacklist, whitelist, requiredlist)




def get_command():
    """
        Recieves a command from the user or IPC socket.
    """

    #evaluate the prompt string to get the current prompt
    current_prompt = prompt.format(**globals())

    #prompt the user for input using the current prompt
    command = raw_input(current_prompt)

    #partition the prompt into a command and (optional) arguments
    command = command.partition(' ')

    #and return the command and arguments
    return command[0], command[2]





#if this is being called as a library, call main
if __name__ == '__main__':
    main()
