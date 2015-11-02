#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 3: Deep Learning and Neural Networks
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com
    Code repository:
    https://github.com/jeffheaton/aifh
    Copyright 2015 by Jeff Heaton
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

import sys

import numpy as np


class Train(object):
    """ Basic training class.  Allows for either minimization or maximization, though all implementations may not
    support both.
    """
    def __init__(self, goal_minimize=True):
        self.max_iterations = 100000
        self.position = []
        self.best_score = 0
        self.goal_minimize = goal_minimize
        self.display_final = True
        self.display_iteration = False
        self.stop_score = None

    def better_than(self, is_this, than_that):
        """Determine if one score is better than the other, based on minimization settings.
        @param is_this: The first score to compare.
        @param than_that: The second score to compare.
        @return: True, if the first score is better than the second.
        """
        if self.goal_minimize:
            return is_this < than_that
        else:
            return is_this > than_that

    def should_stop(self, iteration, best_score):
        """ Determine if we should stop.
        @param iteration: The current iteration.
        @param best_score: The current best score.
        @return: True, if we should stop.
        """
        if iteration > self.max_iterations:
            return True

        if self.stop_score is not None:
            if self.better_than(best_score, self.stop_score):
                return True

        return False


class TrainGreedRandom(Train):
    """
    The Greedy Random learning algorithm is a very primitive random-walk algorithm that only takes steps that serve
    to move the Machine Learning algorithm to a more optimal position.  This learning algorithm essentially chooses
    random locations for the long term memory until a better set is found.

    http://en.wikipedia.org/wiki/Random_walk
    """
    def __init__(self, low, high, goal_minimize=True):
        """
        Construct a greedy random trainer.
        @param low: The low end of random numbers to generate.
        @param high: The high end of random numbers to generate.
        @param goal_minimize: Is the goal to minimize?
        """
        self.high = low
        self.low = high
        Train.__init__(self, goal_minimize)

    def train(self, x0, funct):
        """
        Train with the specified score function.
        @param x0: The initial vector for long-term memory.
        @param funct: The score function.  We attempt to minimize or maximize this.
        @return: The trained long-term memory vector.
        """
        iteration_number = 1
        self.position = list(x0)
        self.best_score = funct(self.position)

        while not self.should_stop(iteration_number, self.best_score):
            # Clone current position, create a new array of same size.
            trial_position = list(self.position)
            # Randomize trial position.
            self.perform_randomization(trial_position)
            # Obtain new trial score.
            trial_score = funct(trial_position)

            if self.better_than(trial_score, self.best_score):
                self.best_score = trial_score
                self.position = trial_position

            current = funct(self.position)

            if self.display_iteration:
                print("Iteration #" + str(iteration_number) + ", Score: " + str(self.best_score))
            iteration_number += 1

        if self.display_final:
            print("Finished after " + str(iteration_number) + " iterations, final score is " + str(self.best_score))
        return self.position

    def perform_randomization(self, vec):
        for i in xrange(0, len(vec)):
            vec[i] = np.random.uniform(self.low, self.high)


class TrainHillClimb(Train):
    """
        Train using hill climbing.  Hill climbing can be used to optimize the long term memory of a Machine Learning
        Algorithm. This is done by moving the current long term memory values to a new location if that new location
        gives a better score from the scoring function.

        http://en.wikipedia.org/wiki/Hill_climbing
    """
    def __init__(self, goal_minimize=True):
        Train.__init__(self, goal_minimize)

    def train(self, x0, funct, acceleration=1.2, step_size=1.0):
        """
        Train up to the specified maximum number of iterations using hill climbing.
        @param x0: The initial vector for long-term memory.
        @param funct: The score function.  We attempt to minimize or maximize this.
        @param acceleration: The acceleration (default=1.2)
        @param step_size: The step size (default=1.0)
        @return: The trained long-term memory vector.
        """
        iteration_number = 1
        self.position = list(x0)
        self.best_score = funct(self.position)

        step_size = [step_size] * len(x0)

        candidate = [0] * 5
        candidate[0] = -acceleration
        candidate[1] = -1 / acceleration
        candidate[2] = 0
        candidate[3] = 1 / acceleration
        candidate[4] = acceleration

        while not self.should_stop(iteration_number, self.best_score):
            if self.goal_minimize:
                best_step_score = sys.float_info.max
            else:
                best_step_score = sys.float_info.min

            for dimension in xrange(0, len(self.position)):
                best = -1
                for i in xrange(0, len(candidate)):
                    # Take a step
                    self.position[dimension] += candidate[i] * step_size[dimension]
                    # Obtain new trial score.
                    trial_score = funct(self.position)
                    # Step back, we only want to try movement in one dimension.
                    self.position[dimension] -= candidate[i] * step_size[dimension]

                    # Record best step taken
                    if self.better_than(trial_score, best_step_score):
                        best_step_score = trial_score
                        best = i

                if best != -1:
                    self.best_score = best_step_score
                    self.position[dimension] += candidate[best] * step_size[dimension]
                    step_size[dimension] += candidate[best]

            if self.display_iteration:
                print("Iteration #" + str(iteration_number) + ", Score: " + str(self.best_score))
            iteration_number += 1

        if self.display_final:
            print("Finished after " + str(iteration_number) + " iterations, final score is " + str(self.best_score))
        return self.position


class TrainAnneal(Train):
    """
        Train a Machine Learning Algorithm using Simulated Annealing.  Simulated Annealing is a Monte Carlo algorithm
        that is based on annealing in metallurgy, a technique involving heating and controlled cooling of a
        material to increase the size of its crystals and reduce their defects, both are attributes of the material
        that depend on its thermodynamic free energy.

        The Simulated Annealing algorithm works by randomly changing a vector of doubles.  This is the long term memory
        of the Machine Learning algorithm.  While this happens a temperature is slowly decreased.  When this
        temperature is higher, the Simulated Annealing algorithm is more likely to accept changes that have a higher
        error (or energy) than the current state.

        There are several important components to any Simulated Learning Algorithm:

        First, the randomization technique.  This is performed by the method performRandomize.  To randomize
        differently, override this method.

        Secondly, the cooling schedule.  This determines how quickly the current temperature will fall.  This is
        controlled by the coolingSchedule.  To define a different cooling schedule, override this method.

        Finally, the probability of accepting a higher-error (energy) solution.  This is defined by a Probability
        Distribution Function (PDF) contained in calcProbability.  To define a different PDF, override this method.

        http://en.wikipedia.org/wiki/Simulated_annealing
    """
    def __init__(self, max_iterations=100, starting_temperature=400, ending_temperature=0.0001):
        """
        Create a simulated annealing trainer.
        @param max_iterations: The maximum number of iterations.
        @param starting_temperature: The starting temperature.
        @param ending_temperature: The ending temperature.
        """
        Train.__init__(self, True)
        self.max_iterations = max_iterations
        self.starting_temperature = starting_temperature
        self.ending_temperature = ending_temperature
        self.cycles = 100
        self.last_probability = 0

    def train(self, x0, funct):
        """
        Train for the specified number of iterations using simulated annealing.  The temperature will be lowered
        between the specified range at each iteration.  You can also use the cycles property to set how many cycles
        are executed at each iteration.  Simulated annealing can only be used to minimize the score function.
        @param x0: The initial long-term memory.
        @param funct: The score function.
        @return: The trained long-term memory.
        """
        iteration_number = 1
        self.position = list(x0)
        self.best_score = funct(self.position)
        current_score = self.best_score
        current_position = list(x0)

        while not self.should_stop(iteration_number, self.best_score):
            # Clone current position, create a new array of same size.
            current_temperature = self.cooling_schedule(iteration_number)

            for c in range(0, self.cycles):
                trial_position = list(current_position)
                # Randomize trial position.
                self.perform_randomization(trial_position)
                # Obtain new trial score.
                trial_score = funct(trial_position)

                keep = False
                if self.better_than(trial_score, current_score):
                    keep = True
                else:
                    self.last_probability = self.calc_probability(current_score, trial_score, current_temperature)
                    if self.last_probability > np.random.uniform():
                        keep = True

                if keep:
                    current_score = trial_score
                    current_position = list(trial_position)
                    if self.better_than(current_score, self.best_score):
                        self.best_score = current_score
                        self.position = list(current_position)

            if self.display_iteration:
                print("Iteration #" + str(iteration_number) + ", Score: " + str(self.best_score)
                      + ",k=" + str(iteration_number)
                      + ",kMax=" + str(self.max_iterations)
                      + ",t=" + str(current_temperature) + ",prob=" + str(self.last_probability) + ","
                      + str(current_score))
                iteration_number += 1

        if self.display_final:
            print("Finished after " + str(iteration_number) + " iterations, final score is " + str(self.best_score))
        return self.position

    def calc_probability(self, error_current, error_new, t):
        """
        Calculate the probability of accepting a worse position.  This can be overriden to provide other
        implementations.
        @param error_current: The current error (score).
        @param error_new: The new error (score)
        @param t: The temperature.
        @return: The probability of accepting the worse score.
        """
        return np.exp(-(np.abs(error_new - error_current) / t))

    def cooling_schedule(self, current_iteration):
        """
        Determine the temperature for the specified iteration.  This method can be overriden to provide other cooling
        schedules.
        @param current_iteration: The iteration number.
        @return: The temperature.
        """
        ex = float(current_iteration) / float(self.max_iterations)
        return self.starting_temperature * (self.ending_temperature / self.starting_temperature) ** ex

    def perform_randomization(self, vec):
        """
        Randomize the provided position to move to a neighbor position.  The provided method perterbs each vector
        element by one tenth of a normally distributed random number.  This works for many continuous problems,
        however, a different method must be used for discrete problems.
        @param vec:
        @return:
        """
        for i in range(0, len(vec)):
            d = np.random.randn() / 10
            vec[i] += d