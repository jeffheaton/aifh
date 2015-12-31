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
import math

try:
    # for Python2
    from Tkinter import *
except ImportError:
    # for Python3
    from tkinter import *

class PlantCell:
    def __init__(self):
        # How green (leaf) or brown (trunk) is the cell.  1.0 is fully leaf, 0.0 is fully trunk.
        self.leafyness = 0

        # The amount of energy between [0,1].
        self.energy = 0

        # The amount of nourishment between [0,1].
        self.nourishment = 0

        # The calculated sunlight exposure.
        self.calculated_sunlight = 0

        # The calculated water exposure.
        self.calculated_water = 0

    def is_alive(self):
        return self.energy > 0.000001


class PlantUniverse:
    # The width of the universe, in terms of cells.
    # Any actual "on screen" display is scaled from this.
    UNIVERSE_WIDTH = 50

    # The height of the universe, in terms of cells.
    # Any actual "on screen" display is scaled from this.
    UNIVERSE_HEIGHT = 100

    # The location of the ground line.  Anything >= to this is underground.
    GROUND_LINE = int(UNIVERSE_HEIGHT - (UNIVERSE_HEIGHT / 3))

    # The size of a cell "info vector".  This vector identifies a cell's state, and is used to encode instructions
    # in the genome.  All of these are normalized to [0,1].  There are currently four elements:
    # 0: The row that the cell is in.
    # 1: The amount of sunlight the cell is exposed to.
    # 2: The degree of crowding, from neighbors.
    # 3: The amount of nourishment the cell is exposed to.
    CELL_VECTOR_LENGTH = 4

    # The size of a GENOME vector.  A genome vector is made up of four "info vectors".  These give instructions
    # on how to grow a plant.
    # Vector 0: Growth template #1
    # Vector 1: Growth template #2
    # Vector 2: Leaf template
    # Vector 3: Stem template
    # For more information on how these are used, refer to the PlantGrowth class.
    GENOME_SIZE = CELL_VECTOR_LENGTH * 4

    # The rate that sunlight decays based on shade, or
    # the rate that nourishment decays based on roots absorbing.
    DECAY = 0.1

    # The rate at which leafy material turns to wooden stem.
    STEM_TRANSITION = 0.8

    # The threshold to allow growth.
    GROWTH_THRESHOLD = 0.25

    # The minimum distance to a genome template to execute that instruction.
    # Used to control how growth happens.
    MIN_GROWTH_DIST = 0.9

    # The minimum energy that a living cell is allowed to drop to.
    MIN_LIVING_ENERGY = 0.1

    # The population size for the genetic algorithm.
    POPULATION_SIZE = 1000

    # How many cycles should we allow the plant to grow for?
    EVALUATION_CYCLES = 100

    def __init__(self):
        # The actual grid, that holds the universe.
        self.grid = [[PlantCell() for j in range(PlantUniverse.UNIVERSE_WIDTH)] for i in
                     range(PlantUniverse.UNIVERSE_HEIGHT)]


        # The amount of nourishment that is held inside of the roots.  This is used to calculate the
        # root ratio, that limits growth.
        self.root_count = 0

        # The amount of leafy material above the surface.  This is used to calculate the root ratio,
        # that limits growth.
        self.surface_count = 0


    def calculate_crowd(self, row, col):
        """
        Calculate the degree of crowding in a cell. Leafy cells
        produce more crowding than stem.
        @param row The row.
        @param col The column.
        @return The crowd imposed by this cell.
        """
        if not self.is_valid(row, col):
            return 0

        cell = self.grid[row][col]

        if not cell.is_alive():
            return 0

        return cell.leafyness


    def calculate_mean_neighbors_crowd(self, row, col):
        """
        Calculate the degree of crowding around a cell.
        This is the mean crowding of the
        @param row The row.
        @param col The column.
        @return The mean crowdedness of the cell.
        """
        sum = 0
        sum += self.calculate_crowd(row - 1, col - 1)
        sum += self.calculate_crowd(row - 1, col)
        sum += self.calculate_crowd(row - 1, col + 1)

        sum += self.calculate_crowd(row, col - 1)
        sum += self.calculate_crowd(row, col + 1)

        sum += self.calculate_crowd(row + 1, col - 1)
        sum += self.calculate_crowd(row + 1, col)
        sum += self.calculate_crowd(row + 1, col + 1)

        return sum / 8.0


    def get_cell_info_vector(self, row, col):
        """
        Return an info vector about a cell.  This allows cells to be identified by instructions in the genome.
        The vector contains four doubles.  All doubles range from [0,1].
        Element 0: The height of the cell. 1.0 for the last row and 0.0 for the first row.
        Element 1: The amount of sunlight (for surface cells) or water (for underground cells) exposure for this cell.
        Element 2: Crowding by neighbors.
        Element 3: Nourishment for this cell.
        @param row The row.
        @param col The column.
        @return The info vector.
        """
        result = [0] * PlantUniverse.CELL_VECTOR_LENGTH
        cell = self.grid[row][col]

        # Height
        result[0] = row / PlantUniverse.UNIVERSE_HEIGHT
        # Sunlight
        if row < PlantUniverse.GROUND_LINE:
            result[1] = cell.calculated_sunlight
        else:
            result[1] = cell.calculated_water

        # Crowd
        result[2] = self.calculate_mean_neighbors_crowd(row, col)
        # Nourishment
        result[3] = cell.nourishment

        return result

    def reset(self):
        """
        Reset the entire grid to a single seed.
        """
        for row in range(0, PlantUniverse.UNIVERSE_HEIGHT):
            for col in range(0, PlantUniverse.UNIVERSE_WIDTH):
                cell = self.grid[row][col]
                cell.leafyness = 0
                cell.energy = 0
                cell.nourishment = 0

        center = int(PlantUniverse.UNIVERSE_WIDTH / 2)
        ground_level = PlantUniverse.GROUND_LINE

        # root
        self.grid[ground_level][center].leafyness = 0
        self.grid[ground_level][center].nourishment = 1
        self.grid[ground_level][center].energy = 1

        # stem
        self.grid[ground_level - 1][center].leafyness = 0.5
        self.grid[ground_level - 1][center].nourishment = 1
        self.grid[ground_level - 1][center].energy = 1

        # leaf
        self.grid[ground_level - 2][center].leafyness = 1
        self.grid[ground_level - 2][center].nourishment = 1
        self.grid[ground_level - 2][center].energy = 1


    def is_valid(self, row, col):
        """
        Returns true if a cell is valid. Invalid cells are off the bounds of a grid.
        @param row The row.
        @param col The column.
        @return True, if valid.
        """
        if row < 0 or col < 0:
            return False

        if row >= PlantUniverse.UNIVERSE_HEIGHT:
            return False

        if col >= PlantUniverse.UNIVERSE_WIDTH:
            return False

        return True

    def calculate_energy(self, row, col):
        """
        Calculate the energy for a cell.
        @param row The row.
        @param col The column.
        @return The info vector.
        """
        if not self.is_valid(row, col):
            return 0

        return self.grid[row][col].energy

    def calculate_transfer_energy(self, row, col):
        """
        Calculate the transfer energy for a cell.  This is the amount of energy transferred into a cell.
        @param row The row.
        @param col The column.
        @return The amount of energy transferred in.
        """
        result = 0
        result = max(result, self.calculate_energy(row - 1, col - 1))
        result = max(result, self.calculate_energy(row - 1, col))
        result = max(result, self.calculate_energy(row - 1, col + 1))
        return result


    def calculate_transfer_nourishment(self, row, col):
        """
        Calculate the transfer nourishment for a cell.  This is the amount of nourishment transferred into a cell.
        @param row The row.
        @param col The column.
        @return The amount of energy transferred in.
        """
        result = 0
        result = max(result, self.calculate_energy(row + 1, col - 1))
        result = max(result, self.calculate_energy(row + 1, col))
        result = max(result, self.calculate_energy(row + 1, col + 1))
        return result


    def count_neighbors(self, row, col):
        """
        Count the number of live cells as neighbors to a cell.
        @param row The row.
        @param col The column.
        @return The neighbor count.
        """
        sum = 0

        if self.is_alive(row - 1, col):
            sum = sum + 1

        if self.is_alive(row + 1, col):
            sum = sum + 1

        if self.is_alive(row, col - 1):
            sum = sum + 1

        if self.is_alive(row, col + 1):
            sum = sum + 1

        if self.is_alive(row - 1, col - 1):
            sum = sum + 1

        if self.is_alive(row + 1, col + 1):
            sum = sum + 1

        if self.is_alive(row - 1, col + 1):
            sum = sum + 1

        if self.is_alive(row + 1, col - 1):
            sum = sum + 1

        return sum;


    def can_grow(self, row, col):
        """
        Returns true, if the specified cell can grow.
        @param row The row.
        @param col The column.
        @return True, if the specified cell is allowed to grow.
        """
        cell = self.grid[row][col]
        if cell.is_alive():
            if row >= PlantUniverse.GROUND_LINE:
                return self.count_neighbors(row, col) < 4
            else:
                return cell.energy > PlantUniverse.GROWTH_THRESHOLD and cell.nourishment > PlantUniverse.GROWTH_THRESHOLD

        return False

    def is_alive(self, row, col):
        """
        Returns true, if the specified cell is alive. Alive cells have energy.
        @param row The row.
        @param col The column.
        @return True, if the specified cell is alive to grow.
        """
        return self.is_valid(row, col) and (self.grid[row][col].is_alive())


class DisplayPlant:
    # The most green color that a plant can take on.
    FULL_GREEN_TUPLE = (0, 255, 0)
    FULL_GREEN = '#%02x%02x%02x' % FULL_GREEN_TUPLE

    # The most brown color that a plant can take on.
    FULL_BROWN_TUPLE = (165, 42, 42)
    FULL_BROWN = '#%02x%02x%02x' % FULL_BROWN_TUPLE

    # The color of the sky.
    SKY_COLOR = '#%02x%02x%02x' % (135, 206, 250)

    # The color of the dirt.
    DIRT_COLOR = '#%02x%02x%02x' % (96, 96, 96)

    def __init__(self,root,universe):
        self.universe = universe
        # Build the grid display
        canvas_width = PlantUniverse.UNIVERSE_WIDTH * 3
        canvas_height = PlantUniverse.UNIVERSE_HEIGHT * 3

        self.root = root
        self.c = Canvas(self.root,width=canvas_width, height=canvas_height)
        self.c.pack()

        self.grid_rects = [[None for j in range(PlantUniverse.UNIVERSE_WIDTH)]
            for i in range(PlantUniverse.UNIVERSE_HEIGHT)]

        for row in range(0,PlantUniverse.UNIVERSE_HEIGHT):
            for col in range(0,PlantUniverse.UNIVERSE_WIDTH):
                x = col * 3
                y = row * 3
                r = self.c.create_rectangle(x, y, x+3,y+3, fill="white")
                self.grid_rects[row][col] = r

        # Create gradient
        gradent_range_red = DisplayPlant.FULL_GREEN_TUPLE[0] - DisplayPlant.FULL_BROWN_TUPLE[0]
        gradent_range_green = DisplayPlant.FULL_GREEN_TUPLE[1] - DisplayPlant.FULL_BROWN_TUPLE[1]
        gradent_range_blue = DisplayPlant.FULL_GREEN_TUPLE[2] - DisplayPlant.FULL_BROWN_TUPLE[2]

        max_range = max(max(
                math.fabs(gradent_range_red), math.fabs(gradent_range_green)), math.fabs(gradent_range_blue))

        scale_red = float( gradent_range_red / float(max_range))
        scale_green = float( gradent_range_green / float(max_range))
        scale_blue = float( gradent_range_blue / float(max_range))

        self.gradient = []

        for i in range(0,int(max_range)):
            self.gradient.append( '#%02x%02x%02x' % (
                    int(DisplayPlant.FULL_BROWN_TUPLE[0] + (i * scale_red)),
                    int(DisplayPlant.FULL_BROWN_TUPLE[1] + (i * scale_green)),
                    int(DisplayPlant.FULL_BROWN_TUPLE[2] + (i * scale_blue))))


    def update(self):
        # Display the plant
        for row in range(0,PlantUniverse.UNIVERSE_HEIGHT):
            for col in range(0,PlantUniverse.UNIVERSE_WIDTH):
                cell = self.universe.grid[row][col]


                # For empty cells display either the ground or sky color.
                # Roots are always white.
                if row >= PlantUniverse.GROUND_LINE:
                    if cell.is_alive():
                        color = "white"
                    else:
                        color = DisplayPlant.DIRT_COLOR

                else:
                    if cell.is_alive():
                        idx = (int) ((len(self.gradient) - 1) * cell.leafyness)
                        color = self.gradient[idx]
                    else:
                        color = DisplayPlant.SKY_COLOR

                r = self.grid_rects[row][col]
                self.c.itemconfig(r, fill=color)
                self.c.itemconfig(r, outline=color)


class PlantBoxMilestone1:
    def __init__(self):
        # Setup the seed.
        universe = PlantUniverse()
        universe.reset()

        # Init TK
        self.root = Tk()
        self.display = DisplayPlant(self.root,universe)
        self.display.update()
        self.root.mainloop()

