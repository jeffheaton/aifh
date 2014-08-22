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
from random import *
import numpy as np

class TrainPSO:
    def __init__(self,particle_count,param_count,score_function):
        # The current error.
        self.current_error = 0

        # The scoring function, this determines the energy (error) of the current solution.
        self.score_function = score_function
        self.goal_minimize = True
        self.display_iteration = True

        # Swarm state and memories.
        self.param_count = param_count
        self.particles = np.zeros([particle_count,param_count], dtype=float)
        self.velocities = np.zeros([particle_count,param_count], dtype=float)
        self.best_vectors = np.zeros([particle_count,param_count], dtype=float)
        self.best_scores = np.zeros([particle_count], dtype=float)
        self.best_vector_index = -1

        # Determines the size of the search space.
        # The position components of particle will be bounded to
        # [-maxPos, maxPos]
        # A well chosen range can improve the performance.
        # -1 is a special value that represents boundless search space.
        self.max_position = -1

        # Maximum change one particle can take during one iteration.
        # Imposes a limit on the maximum absolute value of the velocity
        # components of a particle.
        # Affects the granularity of the search.
        # If too high, particle can fly past optimum solution.
        # If too low, particle can get stuck in local minima.
        # Usually set to a fraction of the dynamic range of the search
        # space (10% was shown to be good for high dimensional problems).
        # -1 is a special value that represents boundless velocities.
        self.max_velocity = 2

        # c1, cognitive learning rate >= 0
        # tendency to return to personal best position
        self.c1 = 2.0

        # c2, social learning rate >= 0
        # tendency to move towards the swarm best position
        self.c2 = 2.0


        # w, inertia weight.
        # Controls global (higher value) vs local exploration
        # of the search space.
        # Analogous to temperature in simulated annealing.
        # Must be chosen carefully or gradually decreased over time.
        # Value usually between 0 and 1.
        self.inertia_weight = 0.4

        # Random particles
        for row in range(0,particle_count):
            # Random position
            for col in range(0,param_count):
                self.particles[row][col] = uniform(-1,1)
            # Random velocity
            for col in range(0,param_count):
                self.velocities[row][col] = uniform(-self.max_velocity,self.max_velocity)

    def clamp_components(self,v,max_value):
        if max_value != -1:
            for i in range(0,len(v)):
                if v[i] > max_value:
                    v[i] = max_value
                if v[i] < -max_value:
                    v[i] = -max_value


    def update_velocity(self,particle_index):
        # Standard PSO formula

        # inertia weight
        for i in range(0,self.param_count):
            self.velocities[particle_index][i]*=self.inertia_weight


        for i in range(0,self.param_count):
            # cognitive term (personal best)
            ct =  (self.best_vectors[particle_index][i] - self.particles[particle_index][i]) * uniform(0,1) * self.c1
            self.velocities[particle_index][i] += ct

            # social term (global best)
            if particle_index != self.best_vector_index:
                st =  (self.best_vectors[self.best_vector_index][i] - self.particles[particle_index][i]) * uniform(0,1) * self.c1
                self.velocities[particle_index][i] += st

    def update_personal_best_position(self,particle_index, particle_position):
        # set the network weights and biases from the vector
        score = self.score_function(self.particles[particle_index])

        # update the best vectors (g and i)
        if self.best_scores[particle_index] == 0 or self.is_score_better(score, self.best_scores[particle_index]):
            self.best_scores[particle_index] = score

        for i in range(0,self.param_count):
            self.best_vectors[particle_index][i] = self.particles[particle_index][i]


    def is_score_better(self,score1,score2):
        return (self.goal_minimize and (score1 < score2)) \
            or (not self.goal_minimize and (score1 > score2))

    def update_particle(self,particle_index):
        self.update_velocity(particle_index)

        # velocity clamping
        self.clamp_components(self.velocities[particle_index], self.max_velocity)

        # new position (Xt = Xt-1 + Vt)
        for i in range(0,self.param_count):
            self.particles[particle_index][i]+=self.velocities[particle_index][i]

        # pin the particle against the boundary of the search space.
        # (only for the components exceeding maxPosition)
        self.clamp_components(self.particles[particle_index], self.max_position)

        self.update_personal_best_position(particle_index, self.particles[particle_index])

    def update_global_best_position(self):
        for i in range(0,len(self.particles)):
            if self.best_vector_index == -1 \
                or self.is_score_better(self.best_scores[i], self.best_scores[self.best_vector_index]):
                self.best_vector_index = i


    def iteration(self):
        for i in range(0,len(self.particles)):
            self.update_particle(i)

        self.update_global_best_position()

    def get_best(self):
        return self.best_vectors[self.best_vector_index]

    def get_best_score(self):
        return self.best_scores[self.best_vector_index]

    def copy_best(self,target):
        b = self.get_best()
        for i in range(len(b)):
            target[i] = b[i]

    def train(self,max_iterations=100):
        for i in range(0,max_iterations):
            self.iteration()
            if self.display_iteration:
                print("Iteration #"+str(i) + ", best score=" + str(self.best_scores[self.best_vector_index]))