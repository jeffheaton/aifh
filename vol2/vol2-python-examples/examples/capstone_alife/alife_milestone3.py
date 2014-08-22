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
from alife_milestone1 import *
from alife_milestone2 import *

def calculate_score(genome):
    universe = PlantUniverse()
    universe.reset()
    physics = PlantPhysics()
    growth = PlantGrowth()

    # Run the generations.
    for i in range(PlantUniverse.EVALUATION_CYCLES):
        physics.run_physics(universe)
        growth.run_growth(universe, genome.data)


    # Count the amount of green.
    count = 0
    sum = 0

    for row in range(0,PlantUniverse.UNIVERSE_HEIGHT):
        for col in range(0,PlantUniverse.UNIVERSE_WIDTH):
            cell = universe.grid[row][col]
            if cell.is_alive():
                if row >= PlantUniverse.GROUND_LINE:
                    sum += 0.5
                else:
                    sum += cell.leafyness

            count = count + 1

    return sum / count

class PlantBoxMilestone3:
    def __init__(self):
        # Setup the seed.
        universe = PlantUniverse()
        universe.reset()

        # Init TK
        self.root = Tk()
        self.display = DisplayPlant(self.root,universe)
        self.display.update()
        self.root.mainloop()

