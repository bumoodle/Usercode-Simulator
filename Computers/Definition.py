"""
    Computer class:

    ~Kyle Temkin <ktemkin@binghamton.edu>

"""

#import metainformation for Abstract Base Class objects
from abc import ABCMeta, abstractmethod

class Computer(object):
    """
        Abstract class which represents a turing-equivalent computer.
    """

    #mark this as an abstract base class
    __metaclass__ = ABCMeta

