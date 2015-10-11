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
import os
import sys

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from tkinter import *
import numpy as np
from som import *
from neighborhood import *

TILES_WIDTH = 50
TILES_HEIGHT = 50
TILE_SCREEN_SIZE = 10

class DisplayColors:
    def __init__(self,root,samples):
        # Build the grid display
        canvas_width = TILES_WIDTH * TILE_SCREEN_SIZE
        canvas_height = TILES_HEIGHT * TILE_SCREEN_SIZE

        self.samples = samples
        self.root = root
        self.c = Canvas(self.root,width=canvas_width, height=canvas_height)
        self.c.pack()

        self.grid_rects = [[None for j in range(TILES_WIDTH)]
            for i in range(TILES_HEIGHT)]

        for row in range(TILES_HEIGHT):
            for col in range(TILES_WIDTH):
                x = col * TILE_SCREEN_SIZE
                y = row * TILE_SCREEN_SIZE
                r = self.c.create_rectangle(x, y, x+TILE_SCREEN_SIZE,y+TILE_SCREEN_SIZE, fill="white")
                self.grid_rects[row][col] = r

        self.som = SelfOrganizingMap(3,TILES_WIDTH * TILES_HEIGHT)
        self.som.reset()

        self.gaussian = NeighborhoodRBF(NeighborhoodRBF.TYPE_GAUSSIAN,[TILES_WIDTH,TILES_HEIGHT])
        self.train = BasicTrainSOM(self.som, 0.01, None, self.gaussian)
        self.train.force_winner = False
        self.train.set_auto_decay(1000, 0.8, 0.003, 30, 5)
        self.iteration = 1

    def RGBToHTMLColor(self, rgb_tuple):
        hexcolor = '#%02x%02x%02x' % rgb_tuple
        return hexcolor

    def convert_color(self, d):
        result = 128*d
        result+= 128
        result = min(result, 255)
        result = max(result, 0)
        return result

    def update(self, som):
        for row in range(TILES_HEIGHT):
            for col in range(TILES_WIDTH):
                index = (row*TILES_WIDTH)+col
                color = (
                    self.convert_color(som.weights[index][0]),
                    self.convert_color(som.weights[index][1]),
                    self.convert_color(som.weights[index][2]))
                r = self.grid_rects[row][col]
                self.c.itemconfig(r, fill=self.RGBToHTMLColor(color))
                self.c.itemconfig(r, outline=self.RGBToHTMLColor(color))

    def update_clock(self):
        idx = np.random.randint(len(samples))
        c = self.samples[idx]

        self.train.train_single_pattern(c)
        self.train.auto_decay()
        self.update(self.som)
        print("Iteration {}, {}".format(self.iteration,self.train.get_status()))
        self.iteration+=1
        if self.iteration<=1000:
            self.root.after(1, self.update_clock)




samples = np.zeros([15,3])
for i in range(15):
    samples[i][0] = np.random.uniform(-1,1)
    samples[i][1] = np.random.uniform(-1,1)
    samples[i][2] = np.random.uniform(-1,1)

root = Tk()
display = DisplayColors(root, samples)
display.update_clock()
root.mainloop()
