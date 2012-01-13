"""
    Collection of UserCode exceptions, which can be extended to mark expressions which can be exposed to the user.
"""

class UserCodeException(Exception):
    """
        Base exception for errors involving user code. These messages are typically meant for the end user whose code was submitted.
    """
    pass

class UserCodeSyntaxException(UserCodeException):
    """
        Standard class for a generic syntax error in user code.
    """
    pass

class ExecutionException(UserCodeException):
    """
        Base for exceptions which occur during program execution.
    """
    pass


