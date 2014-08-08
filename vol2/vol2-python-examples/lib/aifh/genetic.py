__author__ = 'jheaton'

from random import *
import math


class Population:
    discrete = False
    allow_repeats = True
    species = []
    goal_maximize = True
    selection = None
    best_genome = None
    display_iteration = True
    max_gen = 1000000
    max_stagnant = 10


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

    def iteration(self):
        next_generation = []

        while len(next_generation)<len(self.species[0].members):
            if uniform(0,1) > 0.5:
                p1 = self.selection.select(self.species[0])
                p2 = self.selection.select(self.species[0])
                off = [
                    [0] * len(p1.genes),
                    [0] * len(p1.genes)]
                crossover_splice_no_repeat(p1.genes,p2.genes,5,off)
                self.add_child(next_generation,off[0])
                self.add_child(next_generation,off[1])
            else:
                p1 = self.selection.select(self.species[0])
                off = [0] * len(p1.genes)
                mutate_shuffle(p1.genes,off)
                self.add_child(next_generation,off)

        self.species[0].members = next_generation

    def train(self,x0,score_funct,pop_size=1000,low=1,high=-1):
        generation = 1
        stagnant = 0
        last_best = None

        if not self.discrete and not self.allow_repeats:
            raise("Non-discrete (continuous) problems must allow repeats, please set allow_repeats=true.")

        # Do we need to generate a population?
        if len(self.species[0].members) == 0:
            for i in range(1,pop_size):
                genome = Genome()

                if self.discrete:
                    for j in range(0,len(x0)):
                        genome.genes.append(randint(low,high))
                else:
                    for j in range(0,len(x0)):
                        genome.genes.append(uniform(low,high))

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

        # Copy winner
        for i in range(0,len(x0)-1):
            x0[i] = self.best_genome.genes[i]


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

    score = 0
    genes = []

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

    raise("Ran out of integers to select.")



def crossover_splice(p1,p2,cut_len,off) :
    off[0] = [0] * len(p1)
    off[1] = [0] * len(p1)

    # the chromosome must be cut at two positions, determine them
    cutpoint1 = randint(0,len(p1) - cut_len)
    cutpoint2 = cutpoint1 + cut_len

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

def crossover_splice_no_repeat(p1,p2,cut_len,off) :
    off[0] = [0] * len(p1)
    off[1] = [0] * len(p1)

    taken1 = set()
    taken2 = set()

    # the chromosome must be cut at two positions, determine them
    cutpoint1 = randint(0,len(p1) - cut_len)
    cutpoint2 = cutpoint1 + cut_len

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

def mutate_shuffle(p1,off):

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

def mutate_perturb(p1,perturb_amount,off):

    # Clone the parent
    for i in range(0,len(p1)):
        value = p1[i]
        value = value + (value * (perturb_amount - (uniform(0,1) * perturb_amount * 2)))
        off[i] = value
