"""
    Artificial Intelligence for Humans
    Volume 2: Nature-Inspired Algorithms
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com

    Code repository:
    https://github.com/jeffheaton/aifh

    Copyright 2014 by Jeff Heaton

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

    For more information on Heaton Research copyrights, licenses
    and trademarks visit:
    http://www.heatonresearch.com/copyright
    ============================================================================================================
    This example shows how to use genetic programming to find a function that approximates data stored in a CSV
    file.  The CSV file contains data from the equation 4x^2+6x+2.  The exact function is not found by the program
    but a close approximation is usually found.  This program is only a very simple introduction to genetic
    programming.  For more advanced genetic programming tasks in Python you might consider using full scale framework
    such as deep (https://code.google.com/p/deap/).


    This example was influenced from code found in the following examples:
    http://cswww.essex.ac.uk/staff/rpoli/TinyGP/
    http://zhanggw.wordpress.com/2009/11/08/a-simple-genetic-programming-in-python-4/

Sample output:

Generation #0, best score=12720.1411766
Generation #1, best score=12720.1411766
Generation #2, best score=10352.1103796
Generation #3, best score=10352.1103796
Generation #4, best score=10352.1103796
Generation #5, best score=10352.1103796
Generation #6, best score=10352.1103796
Generation #7, best score=10352.1103796
Generation #8, best score=10352.1103796
Generation #9, best score=10352.1103796
Generation #10, best score=10352.1103796
Generation #11, best score=10352.1103796
Generation #12, best score=4760.92294456
Generation #13, best score=1324.0
Generation #14, best score=1324.0
Generation #15, best score=1324.0
Generation #16, best score=1324.0
Generation #17, best score=1324.0
Generation #18, best score=1324.0
Generation #19, best score=1324.0
Generation #20, best score=1324.0
Generation #21, best score=1324.0
Generation #22, best score=1324.0
Generation #23, best score=1324.0
...
Generation #89, best score=52.7362001915
Generation #90, best score=52.7362001915
Generation #91, best score=52.7362001915
Generation #92, best score=52.7362001915
Generation #93, best score=52.7362001915
Generation #94, best score=52.7362001915
Generation #95, best score=52.7362001915
Generation #96, best score=52.7362001915
Generation #97, best score=12.2930539351
Generation #98, best score=12.2930539351
Generation #99, best score=12.2930539351
((x+x)*((x-(-2.42355201966*(x/(((-2.42355201966*x)*(((-5.58918290567/-6.00102217712)--2.42355201966)/1.9928565674))/-4.65011284939))))+x))





"""
# Find the AIFH core files
import os
import sys
import numpy as np
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)


from random import random, randint, choice
from copy import deepcopy
from normalize import Normalize
from error import ErrorCalculation
from random import *

class FunctionWrapper:
  def __init__(self, function, child_count, name):
    self.function = function
    self.child_count = child_count
    self.name = name

class Variable:
  def __init__(self, var, value=0):
    self.var = var
    self.value = value
    self.name = str(var)
    self.type = "variable"

  def evaluate(self):
    return self.varvalue

  def setvar(self, value):
    self.value = value

  def display(self):
    return (self.var)

class Constant:
  def __init__(self, value):
    self.value = value
    self.name = str(value)
    self.type = "constant"

  def evaluate(self):
    return self.value

  def display(self):
    return(self.value)

class Node:
  def __init__(self, type, children, function_wrapper, var=None, const=None):
    self.type = type
    self.children = children
    self.funwrap = function_wrapper
    self.variable = var
    self.const = const
    self.depth = self.refresh_depth()
    self.value = 0
    self.fitness = 0

  def eval(self):
    if self.type == "variable":
      return self.variable.value
    elif self.type == "constant":
      return self.const.value
    else:
      for c in self.children:
        result = [c.eval() for c in self.children]
      return self.funwrap.function(result)

  def set_variable_value(self, var_names, values):
    if self.type == "variable":
      idx = var_names.index(self.variable.var)
      if idx!=-1:
        self.variable.setvar(values[idx])
      else:
        print("There is no value for variable:", self.variable.var)
        return
    if self.type == "constant":
      pass
    if self.children:#function node
      for child in self.children:
        child.set_variable_value(var_names,values)

  def refresh_depth(self):
    if self.type == "constant" or self.type == "variable":
      return 0
    else:
      depth = []
      for c in self.children:
        depth.append(c.refresh_depth())
      return max(depth) + 1

  def display(self):
    if self.type == "function":
      return( "(" + self.children[0].display() + self.funwrap.name + self.children[1].display() + ")")
    elif self.type == "variable":
      return (self.variable.name)
    elif self.type == "constant":
      return (self.const.name)

class Population:
  def __init__(self, wrapper_list, variable_list, constant_list, score_function, \
               goal="min", population=None, size=10, max_depth=10, \
               max_generations=100, cross_rate=0.9, mutation_rate=0.1, new_birth_rate=0.6):
    self.wrapper_list = wrapper_list
    self.variable_list = variable_list
    self.constant_list = constant_list
    self.score_function = score_function
    self.goal = goal
    self.max_depth = max_depth
    self.population = population or self._makepopulation(size)
    self.size = size
    self.max_generations = max_generations
    self.cross_rate = cross_rate
    self.mutation_rate = mutation_rate
    self.new_birth_rate = new_birth_rate

    self.best_tree = self.population[0]
    for i in range(0, self.size):
      self.population[i].depth=self.population[i].refresh_depth()
      self.population[i].fitness = self.score_function(self.population[i])
      if self.goal == "min":
        if self.population[i].fitness < self.best_tree.fitness:
          self.best_tree = self.population[i]
      elif self.goal == "max":
        if self.population[i].fitness > self.best_tree.fitness:
          self.best_tree = self.population[i]

  def _makepopulation(self, popsize):
    return [self._maketree(0) for i in range(0, popsize)]

  def _maketree(self, startdepth):
    if startdepth == 0:
      #make a new tree
      nodepattern = 0#function
    elif startdepth == self.max_depth:
      nodepattern = 1#variable or constant
    else:
      nodepattern = randint(0, 1)
    if nodepattern == 0:
      childlist = []
      selectedfun = randint(0, len(self.wrapper_list) - 1)
      for i in range(0, self.wrapper_list[selectedfun].child_count):
        child = self._maketree(startdepth + 1)
        childlist.append(child)
      return Node("function", childlist, self.wrapper_list[selectedfun])
    else:
      if randint(0, 1) == 0:#variable
        selectedvariable = randint(0, len(self.variable_list) - 1)
        return Node("variable", None, None, \
               Variable(self.variable_list[selectedvariable]), None)
      else:
        selectedconstant = randint(0, len(self.constant_list) - 1)
        return Node("constant", None, None, None,\
               Constant(self.constant_list[selectedconstant]))

  def mutate(self, tree, probchange=0.1, startdepth=0):
    if random() < probchange:
      return self._maketree(startdepth)
    else:
      result = deepcopy(tree)
      if result.type == "function":
        result.children = [self.mutate(c, probchange, startdepth + 1) \
                           for c in tree.children]
    return result

  def crossover(self, tree1, tree2, probswap=0.8, top=1):
    if random() < probswap and not top:
      return deepcopy(tree2)
    else:
      result = deepcopy(tree1)
      if tree1.type == "function" and tree2.type == "function":
        result.children = [self.crossover(c, choice(tree2.children),
                           probswap, 0) for c in tree1.children]
    return result

  def envolve(self, maxgen=100, crossrate=0.9, mutationrate=0.1):
    for i in range(0, maxgen):
      child = []
      for j in range(0, int(self.size * self.new_birth_rate / 2)):
        parent1, p1 = self.roulette_wheel_select()
        parent2, p2 = self.roulette_wheel_select()
        new_child = self.crossover(parent1, parent2)
        child.append(new_child)#generate new tree
        parent, p3 = self.roulette_wheel_select()
        new_child = self.mutate(parent, mutationrate)
        child.append(new_child)
      #refresh all tree's fitness
      for j in range(0, int(self.size * self.new_birth_rate)):
        replacedtree, replacedindex = self.roulette_wheel_select(reverse=True)
        #replace bad tree with child
        self.population[replacedindex] = child[j]

      for k in range(0, self.size):
        self.population[k].fitness = self.score_function(self.population[k])
        self.population[k].depth=self.population[k].refresh_depth()
        if self.goal == "min":
          if self.population[k].fitness < self.best_tree.fitness:
            self.best_tree = self.population[k]
        elif self.goal == "max":
          if self.population[k].fitness > self.best_tree.fitness:
            self.best_tree = self.population[k]
      print("Generation #" + str(i) + ", best score=" + str(self.best_tree.fitness))
    print(self.best_tree.display())

  def gettoptree(self, choosebest=0.9, reverse=False):
    if self.goal == "min":
      self.population.sort()
    elif self.goal == "max":
      self.population.sort(reverse=True)

    if reverse == False:
      if random() < choosebest:
        i = randint(0, self.size * self.new_birth_rate)
        return self.population[i], i
      else:
        i = randint(self.size * self.new_birth_rate, self.size - 1)
        return self.population[i], i
    else:
      if random() < choosebest:
        i = self.size - randint(0, self.size * self.new_birth_rate) - 1
        return self.population[i], i
      else:
        i = self.size - randint(self.size * self.new_birth_rate,\
            self.size - 1)
        return self.population[i], i

  def roulette_wheel_select(self, reverse=False):
    if reverse == False:
      all_fitness = 0
      for i in range(0, self.size):
        all_fitness += self.population[i].fitness
      random_num = random()*(self.size - 1)
      check = 0
      for i in range(0, self.size):
        check += (1.0 - self.population[i].fitness / all_fitness)
        if check >= random_num:
          return self.population[i], i
    if reverse == True:
      all_fitness = 0
      for i in range(0, self.size):
        all_fitness += self.population[i].fitness
      random_num = random()
      check = 0
      for i in range(0, self.size):
        check += self.population[i].fitness * 1.0 / all_fitness
        if check >= random_num:
          return self.population[i], i

#############################################################

def add(args):
    sum_total = 0
    for val in args:
      sum_total = sum_total + val
    return sum_total

def sub(args):
    return args[0] - args[1]

def mul(args):
    return args[0] * args[1]

def div(args):
    if args[1] == 0:
        return 1
    return args[0] / args[1]

add_wrapper = FunctionWrapper(add, 2, "+")
sub_wrapper = FunctionWrapper(sub, 2, "-")
mul_wrapper = FunctionWrapper(mul, 2, "*")
div_wrapper = FunctionWrapper(div, 2, "/")

# find the Iris data set
polyFile = os.path.dirname(os.path.realpath(__file__))
polyFile = os.path.abspath(polyFile + "../../datasets/simple-poly.csv")

# Read the Iris data set.
print('Reading CSV file: ' + polyFile)
norm = Normalize()
poly_work = norm.load_csv(polyFile)
norm.make_col_numeric(poly_work,0)
norm.make_col_numeric(poly_work,1)

# Prepare training data.  Separate into input and ideal.
training = np.array(poly_work)
training_input = training[:, 0:1]
training_ideal = training[:, 1:2]

# Calculate the error with MSE.
def score_function(genome):
    # Loop over the training set and calculate the output for each.
    actual_output = []
    for input_data in training_input:
        genome.set_variable_value(["x"], input_data)
        output_data = genome.eval()
        actual_output.append([output_data])
    result = ErrorCalculation.mse(np.array(actual_output), training_ideal)
    return result

const_pool = [uniform(-10,10) for i in range(10)]

env = Population([add_wrapper, sub_wrapper, mul_wrapper, div_wrapper], ["x"],
                  const_pool, score_function)
env.envolve()