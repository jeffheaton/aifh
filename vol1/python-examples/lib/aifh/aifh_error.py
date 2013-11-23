__author__ = 'jheaton'



class AIFHError(Exception):
    """An error was raised. This is used for several purposes, see individual error messages."""

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return repr(self.value)