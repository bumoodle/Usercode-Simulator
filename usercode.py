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

from Computers.Exceptions import UserCodeException
from Computers.Freescale.HCS08 import MC9S08QG8;

active = ''
prompt = '{active}>'
""" Interactive mode command prompt. """

def main():
    """

    """

    global active, prompt

    #create a new argument parser, which parses the command line arguments
    parser = argparse.ArgumentParser(description='Runs abstract pieces of a user code on one of various turing-equivalent machiness.')

    #if an input file is specified, use it; otherwise, default to the standard input
    #parser.add_argument('codefile', metavar='codefile', nargs='?', type=argparse.FileType('r'), default=sys.stdin, help='The file containing the user code to be executed.')
    parser.add_argument('--system', '-s', type=str, default='hcs08', help='The system on which the user code will be executed.')
    parser.add_argument('--prompt', '-p', type=str, help='Specifies the prompt string. Global variables (such as "active") can be referenced using python formatting notation.')
    parser.add_argument('--noprompt', action='store_true', help='If set, the prompt will be an empty string.')


    #parse this argument's processes
    args = parser.parse_args()

    #if the user has specified a prompt, use it
    if args.prompt:
        prompt = args.prompt

    if args.noprompt:
        prompt = ''

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

        try:

            #get a command from the user
            command, argument = get_command()

            #if the q command was offered, quit the program
            if command in ('q', 'quit', 'x', 'exit'):
                quit()

            #upon the 'code' command, read in the user code
            elif command in ('c', 'code'):
                read_program(system, argument, blacklist, whitelist, requiredlist)
                print "Loaded."

            #step, when requested
            elif command in ('s', 'step'):

                #if the system isn't halted, step
                if not system.halted():
                    system.step()
                    print "Stepped."

                #if the system is halted (which may have occurred as a result of the step)
                if system.halted():
                    print 'Halted.'

            elif command in ('cc', 'cont', 'continue'):

                #TODO: handle breakpoints
                breakpoint = lambda x: False

                system.run_until(breakpoint)
                print "Halted."

            #print a serialized version of the system's state
            elif command in ('g', 'getstate'):
                print system.serialize_state()

            elif command in ('l', 'loadstate'):
                system.unserialize_state(argument)
                print "Loaded."

            #reset the system, when requested
            elif command in ('r', 'reset'):
                system.reset()

            #set the system blacklist (interpreted by the system)
            elif command in ('bl', 'blacklist'):
                blacklist = argument
                print "Updated."

            #set the system whitelist (interpeted by the system)
            elif command in ('wl', 'whitelist'):
                whitelist = argument
                print "Updated."

            #set the system require-list (interpreted by the system)
            elif command in ('rl', 'require'):
                requiredlist = argument
                print "Updated."

            #pass system specific commands to the target system
            elif command in ('sys', 'system'):
                system.handle_system_command(argument)

            #pass through runtime limits
            elif command in ('rtl', 'runlimit'):

                try:
                    limit = int(argument)
                    system.limit_runtime(limit)
                    print 'Limited.'

                except TypeError as e:
                    print 'Not understood; limit unchanged.'

            #simple handshaking command
            elif command in ('ping', 'pn'):
                    print "Pong.";

            #flush the stdout
            sys.stdout.flush()

        #catch any errors in the user code, and display them nicely, here
        except UserCodeException as e:
            print 'ERROR:', e.message
            sys.stdout.flush()



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
