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

try:
    # for Python2
    from Tkinter import *
    from Tkinter import _flatten
except ImportError:
    # for Python3
    from tkinter import *
    from tkinter import _flatten

import random
import math


# The number of particles.
PARTICLE_COUNT = 25

# The size of each particle.
PARTICLE_SIZE = 10

# The constant for cohesion.
COHESION = 0.01

# The constant for alignment.
ALIGNMENT = 0.5

# The constant for separation.
SEPARATION = 0.25

CANVAS_HEIGHT = 400
CANVAS_WIDTH = 400

class Particle:
    def __init__(self):
        self.location = [0] * 2
        self.velocity = [0] * 2
        self.poly = None

class App():
    """
    Flocking.


    """
    def __init__(self):
        self.root = Tk()
        self.c = Canvas(self.root,width=CANVAS_WIDTH, height=CANVAS_HEIGHT)
        self.c.pack()

        self.particles = []

        self.c.create_rectangle(0, 0, CANVAS_WIDTH, CANVAS_HEIGHT, outline="black", fill="black")

        for i in range(0, PARTICLE_COUNT) :
            p = Particle()
            p.location = [0] * 2
            p.velocity = [0] * 2
            p.location[0] = random.randint(0,CANVAS_WIDTH)
            p.location[1] = random.randint(0,CANVAS_HEIGHT)
            p.velocity[0] = 3
            p.velocity[1] = random.uniform(0,2.0*math.pi)

            p.poly = self.c.create_polygon([0,0,0,0,0,0],fill='white')

            self.particles.append(p)

        self.update_clock()
        self.root.mainloop()

    def max_index(self,data):
        result = -1
        for i in range(0,len(data)):
            if result==-1 or data[i] > data[result]:
                result = i
        return result

    def particle_location_mean(self,particles,dimension):
        sum = 0
        count = 0
        for p in particles:
            sum = sum + p.location[dimension]
            count = count + 1
        return sum / count

    def particle_velocity_mean(self,particles,dimension):
        sum = 0
        count = 0
        for p in particles:
            sum = sum + p.velocity[dimension]
            count = count + 1
        return sum / count

    def find_nearest(self,target,particles,k,max_dist):
        result = []
        temp_dist = [0] * k
        worst_index = -1

        for particle in particles:
            if particle!=target:
                # Euclidean distance
                d = math.sqrt(
                    math.pow(particle.location[0] - target.location[0],2) +
                    math.pow(particle.location[1] - target.location[1],2) )

                if d<=max_dist:
                    if len(result) < k:
                        temp_dist[len(result)] = d
                        result.append(particle)
                        worst_index = self.max_index(temp_dist)
                    elif d<temp_dist[worst_index]:
                        temp_dist[worst_index] = d
                        worst_index = self.max_index(temp_dist)
        return result

    def flock(self):
        for particle in self.particles:
            ###############################################################
            ## Begin implementation of three very basic laws of flocking.
            ###############################################################
            neighbors = self.find_nearest(particle, self.particles, 5, sys.float_info.max)
            nearest = self.find_nearest(particle, self.particles, 5, 10)

            # 1. Separation - avoid crowding neighbors (short range repulsion)
            separation = 0
            if len(nearest) > 0:
                meanX = self.particle_location_mean(nearest, 0)
                meanY = self.particle_location_mean(nearest, 1)
                dx = meanX - particle.location[0]
                dy = meanY - particle.location[1]
                separation = math.atan2(dx, dy) - particle.velocity[1]
                separation += math.pi

            # 2. Alignment - steer towards average heading of neighbors
            alignment = 0

            if len(neighbors) > 0:
                alignment = self.particle_velocity_mean(neighbors, 1) - particle.velocity[1]

            # 3. Cohesion - steer towards average position of neighbors (long range attraction)
            cohesion = 0

            if len(neighbors):
                meanX = self.particle_location_mean(self.particles, 0)
                meanY = self.particle_location_mean(self.particles, 1)
                dx = meanX - particle.location[0]
                dy = meanY - particle.location[1]
                cohesion = math.atan2(dx, dy) - particle.velocity[1]

            # perform the turn
            # The degree to which each of the three laws is applied is configurable.
            # The three default ratios that I provide work well.
            turnAmount = (cohesion *  COHESION) + (alignment * ALIGNMENT) + (separation * SEPARATION)

            particle.velocity[1] += turnAmount

            ###############################################################
            ## End implementation of three very basic laws of flocking.
            ###############################################################


    def update_clock(self):


        # render the particles
        points = [0] * 6

        for p in self.particles:
            points[0] = p.location[0]
            points[1] = p.location[1]

            r = p.velocity[1] + (math.pi * 5.0) / 12.0
            points[2] = points[0] - (int) (math.cos(r) * PARTICLE_SIZE)
            points[3] = points[1] - (int) (math.sin(r) * PARTICLE_SIZE)

            r2 = p.velocity[1] + (math.pi * 7.0) / 12.0
            points[4] = points[0] - (int) (math.cos(r2) * PARTICLE_SIZE)
            points[5] = points[1] - (int) (math.sin(r2) * PARTICLE_SIZE)

            self.c.coords(p.poly,_flatten(points))

            # move the particle
            dx = math.cos(r)
            dy = math.sin(r)
            p.location[0] = p.location[0] + (dx * p.velocity[0])
            p.location[1] = p.location[1] + (dy * p.velocity[0])

            # handle wraps
            if p.location[0] < 0:
                    p.location[0] = CANVAS_WIDTH

            if p.location[1] < 0:
                    p.location[1] = CANVAS_HEIGHT

            if p.location[0] > CANVAS_WIDTH:
                    p.location[0] = 0

            if p.location[1] > CANVAS_HEIGHT:
                    p.location[1] = 0

            self.flock()

        # Next frame.
        self.root.after(100, self.update_clock)

app=App()