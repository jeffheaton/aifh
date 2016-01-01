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
__author__ = 'jheaton'

from random import *
import math
from aifh_error import *


class Population:
    def __init__(self):
        self.selection = TournamentSelection()
        self.species = []
        self.goal_maximize = False
        self.best_genome = None
        self.display_iteration = True
        self.max_gen = 1000000
        self.max_stagnant = 10
        self.cut_len = 5
        self.perturb_amount = 0.1
        self.function_crossover = crossover_splice
        self.function_mutate = mutate_perturb
        self.mutate_percent = .2

    def better_than(self,g1,g2):
        if self.goal_maximize:
            return g1.score > g2.score
        else:
            return g1.score < g2.score

    def add_child(self,gen,genes):
        child = Genome()
        child.genes = genes
        child.score = self.score_function(child.genes)
        gen.append(child)

        if self.best_genome == None or self.better_than(child,self.best_genome):
            self.best_genome = child

    def perform_crossover(self,next_generation,p1,p2):
        off = [
            [0] * len(p1.genes),
            [0] * len(p1.genes)]
        self.function_crossover(self,p1.genes,p2.genes,off)
        self.add_child(next_generation,off[0])
        self.add_child(next_generation,off[1])

    def perform_mutation(self,next_generation,p1):
        off = [0] * len(p1.genes)
        self.function_mutate(self,p1.genes,off)
        self.add_child(next_generation,off)

    def iteration(self):
        next_generation = []

        while len(next_generation)<len(self.species[0].members):
            if uniform(0,1) > self.mutate_percent:
                p1 = self.selection.select(self.species[0])
                p2 = self.selection.select(self.species[0])
                self.perform_crossover(next_generation,p1,p2)
            else:
                p1 = self.selection.select(self.species[0])
                self.perform_mutation(next_generation,p1)

        self.species[0].members = next_generation

    def create_population(self,size,pop_size=1000,low=-1,high=1,discrete=False,no_repeat=False):

        if not discrete and no_repeat:
            raise(AIFHError("Can't create a non-discrete (continuous) population without repeats."))

        self.species.append( Species(self))
        for i in range(0,pop_size):
            genome = Genome()

            # Generate random genome.
            if discrete:
                while len(genome.genes)<size:
                    r = randint(low,high)
                    if not no_repeat or r not in genome.genes:
                        genome.genes.append(r)
            else:
                for j in range(0,size):
                    genome.genes.append(uniform(low,high))

            # Add genome to species.
            self.species[0].members.append(genome)


    def train(self,score_funct):
        self.score_function = score_funct
        generation = 1
        stagnant = 0
        last_best = None

        # Do we need to generate a population?
        if len(self.species) == 0:
            raise(AIFHError("There are no species."))

        if len(self.species[0].members) == 0:
            raise(AIFHError("Population is empty."))

        # Score the population
        for genome in self.species[0].members:
            genome.score = score_funct(genome.genes)

        # Loop through training
        while generation<=self.max_gen and stagnant<self.max_stagnant:
            self.iteration()

            if self.display_iteration:
                print("Generaton #" + str(generation) + ", Score=" + str(self.best_genome.score) + ", stagnant=" + str(stagnant))

            generation = generation + 1

            if last_best != None and math.fabs(last_best.score-self.best_genome.score)<0.001:
                stagnant = stagnant + 1
            else:
                stagnant = 0

            last_best = self.best_genome


class Species:
    members = []
    population = None

    def __init__(self,population):
        self.population = population

class Genome:
    """
    A genome is the basic blueprint for creating an phenome (organism).
    Some genomes also function as phenomes.
    """

    def __init__(self):
        self.score = 0
        self.genes = []

class TournamentSelection:
    """
    Tournament select can be used to select a fit (or unfit) genome from a
    species. The selection is run a set number of rounds. Each round two random
    participants are chosen. The more fit participant continues to the next
    round.

    http://en.wikipedia.org/wiki/Tournament_selection
    """
    rounds = 5
    population = None

    def select(self, species):
        idx = randint(0,len(species.members)-1)
        best = species.members[idx]

        for i in range(0,self.rounds):
            idx = randint(0,len(species.members)-1)
            competitor = species.members[idx]

            # only evaluate valid genomes
            if competitor.score != None and not math.isnan(competitor.score):
                if species.population.better_than (competitor, best) :
                    best = competitor

        return best


def get_not_taken(source,taken):
    for trial in source:
        if not trial in taken:
            taken.add(trial)
            return trial

    raise(AIFHError("Ran out of integers to select."))



def crossover_splice(pop,p1,p2,off) :
    off[0] = [0] * len(p1)
    off[1] = [0] * len(p1)

    # the chromosome must be cut at two positions, determine them
    cutpoint1 = randint(0,len(p1) - pop.cut_len)
    cutpoint2 = cutpoint1 + pop.cut_len

    # handle cut section
    for i in range(0,len(p1)):
        if not ((i < cutpoint1) or (i > cutpoint2)) :
            off[0][i] = p1[i]
            off[1][i] = p2[i]

    # handle outer sections
    for i in range(0,len(p1)):
        if (i < cutpoint1) or (i > cutpoint2):
            off[0][i] = p2[i]
            off[1][i] = p1[i]

def crossover_splice_no_repeat(pop,p1,p2,off) :
    off[0] = [0] * len(p1)
    off[1] = [0] * len(p1)

    taken1 = set()
    taken2 = set()

    # the chromosome must be cut at two positions, determine them
    cutpoint1 = randint(0,len(p1) - pop.cut_len)
    cutpoint2 = cutpoint1 + pop.cut_len

    # handle cut section
    for i in range(0,len(p1)):
        if not ((i < cutpoint1) or (i > cutpoint2)) :
            off[0][i] = p1[i]
            off[1][i] = p2[i]
            taken1.add(p1[i])
            taken2.add(p2[i])

    # handle outer sections
    for i in range(0,len(p1)):
        if (i < cutpoint1) or (i > cutpoint2):
            off[0][i] = get_not_taken(p2,taken1)
            off[1][i] = get_not_taken(p1,taken2)

def mutate_shuffle(pop,p1,off):

    # Clone the parent
    for i in range(0,len(p1)):
        off[i] = p1[i]

    # Choose two swap points
    iswap1 = randint(0,len(p1)-1)
    iswap2 = randint(0,len(p1)-1)

    # can't be equal
    if iswap1 == iswap2:
        # move to the next, but
        # don't go out of bounds
        if iswap1 > 0:
            iswap1 = iswap1 - 1
        else:
            iswap1 = iswap1 + 1

    temp = off[iswap1]
    off[iswap1] = off[iswap2]
    off[iswap2] = temp

def mutate_perturb(pop,p1,off):

    # Clone the parent
    for i in range(0,len(p1)):
        value = p1[i]
        value = value + (value * (pop.perturb_amount - (uniform(0,1) * pop.perturb_amount * 2)))
        off[i] = value

