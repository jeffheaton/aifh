import math
from aifh_error import *

class Node:
    def __init__(self,opcode):
        self.child = []
        self.const_values = []
        self.type = opcode


class GeneticProgram:
    ADD = 0
    SUBTRACT = 1
    DIVIDE = 2
    MULTIPLY = 3
    NEGATE = 4
    SQRT = 5
    VAR_CONST = 6

    def __init__(self,var_count,const_count,const_low,const_high):
        self.var_values = 1
        self.var_values = [0] * var_count
        self.const_values = []
        for i in range(0,const_count):
            self.const_values.append()

    def eval(self,node):
        # First, handle regular opcodes, these are binary (two arguments)
        if node.type == GeneticProgram.ADD:
            return eval(node.child[0]) + eval(node.child[1])
        elif node.type == GeneticProgram.SUBTRACT:
            return eval(node.child[0]) - eval(node.child[1])
        elif node.type == GeneticProgram.DIVIDE:
            return eval(node.child[0]) / eval(node.child[1])
        elif node.type == GeneticProgram.MULTIPLY:
            return eval(node.child[0]) * eval(node.child[1])
        # Now, handle unary (single argument)
        elif node.type == GeneticProgram.NEGATE:
            return -eval(node.child[0])
        elif node.type == GeneticProgram.SQRT:
            return math.sqrt(eval(node.child[0]))
        else:
        # Now, handle variable and constant opcodes,
        # these are terminal (no arguments)
            index = node.type - GeneticProgram.VAR_CONST
            if index >= len(self.const_values) + len(self.var_values):
                raise(AIFHError("Invalid opcode: " + node.type) )
            if index < len(self.var_values):
                return self.var_values[index]
            else:
                return self.const_values[index - len(self.var_values)]
