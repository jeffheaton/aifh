#!/usr/bin/env python
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
"""
import numpy as np
from random import *
import math
import copy
import operator
from aifh_error import *

# The initial value of the pheromone trails.
INITIAL_PHEROMONE = 1.0

# Sigma constant. Minimum standard deviation.
CONST_SIGMA = 0.1

# Q constant.  Weighting exponent factor.
CONST_Q = 0.08

class DiscreteAnt:
    def __init__(self,l):
        # The path.
        self.path = [0] * l

        # The nodes visited.
        self.visited = [0] * l

    def visit(self,current_index, node):
        self.path[current_index] = node
        self.visited[node] = True

    def calculate_cost(self,current_index, cost):
        length = cost(self.path[current_index - 1], self.path[0])
        for i in range(0,current_index - 1) :
            length += cost(self.path[i], self.path[i + 1])
        return length

    def clear(self):
        for i in range(0,len(self.visited)):
            self.visited[i] = False

class ContinuousAnt:
    def __init__(self,n, should_minimize):
        # The score for this ant.
        self.score = 0

        # The parameters for this ant.
        self.params = [0] * n

        # True, if this ant should minimize.  This value should be the same for all ants.
        self.should_minimize = should_minimize

    def get_score(self):
        return float(self.score)

class DiscreteACO:
    def __init__(self,ant_count,graph_size,cost):
        # The pheromone trails between graph segments.
        self.pheromone = np.zeros([graph_size,graph_size], dtype=float)

        # A graph of costs.
        self.graph_size = graph_size
        self.cost = cost
        self.display_iteration = False
        self.max_stagnant = 10

        # The ants.
        self.ants = []

        # Constant that defines the attractiveness of the pheromone trail.
        self.alpha = 1

        # Constant that defines the attractiveness of better state transitions (from one node to another).
        self.beta = 5

        # Constant that defines how quickly the pheromone path evaporates.
        self.evaporation = 0.5

        # The amount of pheromone that the nodes of a path share for a trip.
        self.q = 500

        # The base probability.
        self.pr = 0.01

        # The current best path.
        self.best_path = [0] * graph_size

        # The cost of the best path.  We are trying to minimize this.
        self.best_cost = float("inf")

        for i in range(0,graph_size):
            for j in range(0,graph_size):
                self.pheromone[i][j] = INITIAL_PHEROMONE

        for i in range(0,ant_count):
            self.ants.append(DiscreteAnt(graph_size))



    def calculate_probability(self,current_index,ant):
        result = [0] * self.graph_size
        i = ant.path[current_index - 1]

        d = 0.0
        for l in range(0,self.graph_size):
            if not ant.visited[l]:
                d = d + math.pow(self.pheromone[i][l], self.alpha) \
                        * math.pow(1.0 / self.cost(i, l), self.beta)


        for j in range(0,self.graph_size):
            if ant.visited[j]:
                result[j] = 0.0
            else:
                n = math.pow(self.pheromone[i][j], self.alpha) \
                        * math.pow(1.0 / self.cost(i, j), self.beta)
                result[j] = n / d

        return result

    def pick_next_node(self,current_index,ant):
        if current_index == 0 or uniform(0,1) < self.pr:
            index = -1

            while index==-1 or ant.visited[index]:
                index = randint(0,self.graph_size-1)
            return index

        prob = self.calculate_probability(current_index, ant)

        r = uniform(0,1)
        sum = 0
        for i in range(0,self.graph_size):
            sum = sum + prob[i]
            if sum >= r:
                return i

        # should not happen!
        raise(AIFHError("Could not pick next ACO node."))

    def update_pheromone(self):
        # Cause evaporation.
        for i in range(0,self.graph_size):
            for j in range(0,self.graph_size):
                self.pheromone[i][j] *= self.evaporation

        # Adjust for ants.
        for a in self.ants:
            d = self.q / a.calculate_cost(self.graph_size, self.cost)
            for i in range(0,self.graph_size-1):
                self.pheromone[a.path[i]][a.path[i + 1]] += d

            self.pheromone[a.path[self.graph_size - 1]][a.path[0]] += d

    def march(self):
        for current_index in range(0,self.graph_size):
            for a in self.ants:
                next = self.pick_next_node(current_index, a)
                a.visit(current_index, next)

    def setup_ants(self):
        for a in self.ants:
            a.clear()


    def update_best(self):
        best_path_found = None

        for a in self.ants:
            cost = a.calculate_cost(self.graph_size, self.cost)
            if cost < self.best_cost:
                best_path_found = a.path
                self.best_cost = cost

        if best_path_found != None:
            self.best_path = copy.copy(best_path_found)

    def iteration(self):
        self.setup_ants()
        self.march()
        self.update_pheromone()
        self.update_best()

    def train(self,max_iterations=100):
        iteration = 1
        stagnant = 0
        last_best = None

        # Loop through training
        while iteration<=max_iterations and stagnant<self.max_stagnant:
            self.iteration()

            if self.display_iteration:
                print("Iteration #" + str(iteration) + ", Score=" + str(self.best_cost) + ", stagnant=" + str(stagnant))

            iteration = iteration + 1

            if last_best != None and math.fabs(last_best-self.best_cost)<0.001:
                stagnant = stagnant + 1
            else:
                stagnant = 0

            last_best = self.best_cost

class ContinuousACO:
    def __init__(self,x0,score_function,population_size):

        # The population of ants.
        self.population = [None] * population_size * 2

        # The population size.
        self.population_size = population_size

        # The parameter count.
        self.param_count = len(x0)

        # The weighting of each ant.
        self.weighting = [0] * population_size

        # The sum of the weighting.
        self.sum_weighting = 0


        # Epsilon, learning rate.
        self.epsilon = .75

        # The score function.
        self.score_function = score_function
        self.goal_minimize = True
        self.display_iteration = False
        self.max_stagnant = 10

        # What we are optimizing
        self.x0 = x0


        for i in range(0,len(self.population)):
            self.population[i] = ContinuousAnt(self.param_count, self.goal_minimize)
            for j in range(0,self.param_count):
                self.population[i].params[j] = uniform(-1, 1)


        self.update_score()
        self.perform_sort()
        self.compute_weighting()
        self.sample_solutions()
        self.perform_sort()

    def perform_sort(self):
        self.population.sort(key=operator.methodcaller("get_score"), reverse=not self.goal_minimize)

    def update_score(self):
        for a in self.population:
            a.score = self.score_function(a.params)

    def compute_weighting(self):
        self.sum_weighting = 0

        for i in range(0,self.population_size):
            exponent = (i * i) / (2 * CONST_Q * CONST_Q * self.population_size * self.population_size)
            self.weighting[i] = (1 / (0.1 * math.sqrt(2 * math.pi))) * math.pow(math.e, -exponent)
            self.sum_weighting += self.weighting[i]


    def compute_sd(self,x,l):
        sum = 0.0
        for i in range(0,self.population_size):
            sum += math.fabs(self.population[i].params[x] - self.population[l].params[x]) / (self.population_size - 1)

        if sum < 0.00001:
            return CONST_SIGMA

        return (self.epsilon * sum)


    def select_pdf(self):
        l = 0
        temp = 0

        r = uniform(0,1)
        for i in range(0,self.population_size):
            temp += self.weighting[i] / self.sum_weighting
            if r < temp:
                l = i
                break

        return l

    def sample_solutions(self):
        for i in range(self.population_size,self.population_size*2):
            pdf = self.select_pdf()
            for j in range(0,self.param_count):
                sigma = self.compute_sd(j, pdf)
                mu = self.population[pdf].params[j]
                d = (uniform(0,1) * sigma) + mu
                self.population[i].params[j] = d

    def iteration(self):
        self.compute_weighting()
        self.sample_solutions()
        self.update_score()
        self.perform_sort()

    def train(self,max_iterations=100):
        iteration = 1
        stagnant = 0
        last_best = None

        # Loop through training
        while iteration<=max_iterations and stagnant<self.max_stagnant:
            self.iteration()
            score = self.population[0].score

            if self.display_iteration:
                print("Iteration #" + str(iteration) + ", Score=" + str(score) + ", stagnant=" + str(stagnant))

            iteration = iteration + 1

            if last_best != None and math.fabs(last_best-score)<0.001:
                stagnant = stagnant + 1
            else:
                stagnant = 0

            last_best = score

        # Copy back
        for i in range(0,len(self.x0)):
            self.x0[i] = self.population[0].params[i]