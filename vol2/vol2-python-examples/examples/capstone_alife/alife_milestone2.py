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


class PlantGrowth:
    # Transformations to move from a cell to the 9 neighboring cells.
    # These are the column values.
    col_transform = [0, 0, -1, 1, -1, 1, 1, -1]

    # Transformations to move from a cell to the 9 neighboring cells.
    # These are the row values.
    row_transform = [-1, 1, 0, 0, -1, 1, -1, 1]

    def __init__(self):
        # Used to hold the new cells that have grown.
        self.new_composition = [[False for j in range(PlantUniverse.UNIVERSE_HEIGHT)]
                                for i in range(PlantUniverse.UNIVERSE_HEIGHT)]

    def calc_distance(self, v1, v1_start, v2, v2_start, l):
        sum = 0
        for i in range(0, l):
            d = v1[v1_start + i] - v2[v2_start + i]
            sum = sum + (d * d)

        return math.sqrt(sum)

    def get_growth_potential(self, universe, row, col, genome):
        """
        Calculate the growth potential for a candidate cell. Evaluates the distance between the candidate cell's info
        vector and the two growth vectors in the genome.  The minimum of these two vectors will be returned if
        it is below a specified minimum threshold.

        @param universe The universe to evaluate.
        @param row      The row to evaluate.
        @param col      The column to evaluate.
        @param genome   The genome.
        @return The minimum distance.
        """
        cellVec = universe.get_cell_info_vector(row, col)
        d1 = self.calc_distance(cellVec, 0, genome, PlantUniverse.CELL_VECTOR_LENGTH * 2,
                                PlantUniverse.CELL_VECTOR_LENGTH)
        d2 = self.calc_distance(cellVec, 0, genome, PlantUniverse.CELL_VECTOR_LENGTH * 3,
                                PlantUniverse.CELL_VECTOR_LENGTH)

        result = min(d1, d2)
        if result > PlantUniverse.MIN_GROWTH_DIST:
            result = -1

        return result

    def evaluate_neighbors(self, universe, row, col, genome, allow_root, allow_surface):
        """
        Evaluate neighbors to see where to grow into.
        @param universe     The universe.
        @param row          The row.
        @param col          The column.
        @param genome       The genome.
        @param allowRoot    Are roots allowed?
        * @param allowSurface Is surface growth allowed.
        """
        growth_target_row = row
        growth_target_col = col
        growth_target_score = float("inf")

        for i in range(0, len(PlantGrowth.col_transform)):
            eval_col = col + PlantGrowth.col_transform[i]
            eval_row = row + PlantGrowth.row_transform[i]

            if not allow_root and eval_row >= PlantUniverse.GROUND_LINE:
                continue

            if not allow_surface and eval_row < PlantUniverse.GROUND_LINE:
                continue

            if universe.is_valid(eval_row, eval_col):
                p = self.get_growth_potential(universe, eval_row, eval_col, genome)
                if p > 0:
                    if p < growth_target_score:
                        growth_target_score = p
                        growth_target_row = eval_row
                        growth_target_col = eval_col


        # Grow new cell, if requested, did we ever set target row & col to anything?
        if growth_target_row != row or growth_target_col != col:
            self.new_composition[growth_target_row][growth_target_col] = True

    def run_growth(self, universe, genome):
        """
        Run a growth cycle for the universe.
        @param universe The universe.
        @param genome   The genome.
        """
        # Does this plant have enough roots to grow?
        if universe.surface_count == 0:
            return

        # The amount of leafy material per root nourishment.  A higher number indicates
        # more root nourishment than leafs.
        root_ratio = universe.root_count / universe.surface_count

        allow_root = root_ratio < 0.5
        allow_surface = root_ratio > 0.5

        # Reset the new composition to be the composition of the current universe
        for row in range(0, PlantUniverse.UNIVERSE_HEIGHT):
            for col in range(0, PlantUniverse.UNIVERSE_WIDTH):
                self.new_composition[row][col] = False

        for row in range(0, PlantUniverse.UNIVERSE_HEIGHT):
            for col in range(0, PlantUniverse.UNIVERSE_WIDTH):
                cell = universe.grid[row][col]

                # see if we want to change the composition
                if row < PlantUniverse.GROUND_LINE:
                    cell_vec = universe.get_cell_info_vector(row, col)
                    d1 = self.calc_distance(cell_vec, 0, genome, 0, PlantUniverse.CELL_VECTOR_LENGTH)
                    d2 = self.calc_distance(cell_vec, 0, genome, PlantUniverse.CELL_VECTOR_LENGTH,
                                            PlantUniverse.CELL_VECTOR_LENGTH)

                    if d1 < d2:
                        cell.leafyness = cell.leafyness * PlantUniverse.STEM_TRANSITION

                # Evaluate growth into each neighbor cell
                if universe.can_grow(row, col):
                    self.evaluate_neighbors(universe, row, col, genome, allow_root, allow_surface)

        # Copy the new composition back to the universe
        for row in range(0, PlantUniverse.UNIVERSE_HEIGHT):
            for col in range(0, PlantUniverse.UNIVERSE_WIDTH):
                cell = universe.grid[row][col]

                if self.new_composition[row][col]:
                    if row >= PlantUniverse.GROUND_LINE:
                        # Roots are always 100% stem for transfer.
                        cell.leafyness = 0
                    else:
                        cell.leafyness = 1.0

                    cell.energy = 1.0
                    cell.nourishment = 1.0


class PlantPhysics:
    def distribute_energy(self, universe):
        """
        Distribute the sunlight energy in the universe.
        @param universe The universe.
        """
        # Distribute sun energy downward
        sunlight = [0] * PlantUniverse.UNIVERSE_WIDTH
        for i in range(0, len(sunlight)):
            sunlight[i] = 1.0

        for row in range(0, PlantUniverse.UNIVERSE_HEIGHT):
            for col in range(0, PlantUniverse.UNIVERSE_WIDTH):
                # no sun underground
                if row >= PlantUniverse.GROUND_LINE:
                    # blocked
                    decay = 0
                else:
                    # no decay until otherwise calculated
                    decay = 1

                cell = universe.grid[row][col]
                cell.calculated_sunlight = sunlight[col]

                # Collect resources for live cells
                if cell.is_alive():
                    # Live cells cause the sunlight to decay (shade)
                    decay *= PlantUniverse.DECAY * cell.leafyness

                    # Set the energy based on sunlight level and composition of the live cell
                    my_energy = cell.calculated_sunlight * cell.leafyness
                    trans_energy = universe.calculate_transfer_energy(row, col) * (1.0 - cell.leafyness)
                    e = max(my_energy, trans_energy)
                    e = max(PlantUniverse.MIN_LIVING_ENERGY, e)
                    cell.energy = e

                sunlight[col] = sunlight[col] * decay

    def distribute_nourishment(self, universe):
        """
        Distribute nourishment in the universe.
        @param universe The universe.
        """
        root_count = 0
        surface_count = 0

        # Distribute sun energy downward
        water_table = [1.0] * PlantUniverse.UNIVERSE_WIDTH

        for row in range(PlantUniverse.UNIVERSE_HEIGHT - 1, -1, -1):
            for col in range(0, PlantUniverse.UNIVERSE_WIDTH):
                # no water above ground
                if row < PlantUniverse.GROUND_LINE:
                    # blocked
                    decay = 0
                else:
                    # no decay until otherwise calculated
                    decay = 1

                cell = universe.grid[row][col]
                cell.calculated_water = water_table[col]

                # Collect resources for live cells
                if cell.is_alive():
                    # Live cells cause the water to decay (roots collect)
                    decay *= PlantUniverse.DECAY

                    # Set the energy based on sunlight level and composition of the live cell
                    my_water = cell.calculated_water * cell.leafyness
                    trans_water = universe.calculate_transfer_nourishment(row, col) * (1.0 - cell.leafyness)
                    n = max(my_water, trans_water)
                    n = max(PlantUniverse.MIN_LIVING_ENERGY, n)
                    cell.nourishment = n

                    # update the root and surface counts
                    if row >= PlantUniverse.GROUND_LINE:
                        root_count += cell.nourishment
                    else:
                        surface_count += cell.leafyness

                water_table[col] = water_table[col] * decay

        universe.root_count = root_count
        universe.surface_count = surface_count

    def run_physics(self, universe):
        self.distribute_energy(universe)
        self.distribute_nourishment(universe)


class PlantBoxMilestone2:
    SAMPLE_PLANT = [
        0.08414097456375995, 0.11845586131703176, 0.1868971940834313, 0.4346911204161327,
        0.024190631402031804, 0.5773526701833149, 0.8997253827355136, 0.9267311086327318,
        0.04639229538493471, 0.8190692654645835, 0.06531672676605614, 0.026431639742068264,
        0.31497914852215286, 1.0276526539348398, 0.03303133293309127, 0.35946010922382937]

    def __init__(self):
        # Setup the seed.
        self.universe = PlantUniverse()
        self.universe.reset()
        self.physics = PlantPhysics()
        self.growth = PlantGrowth()
        self.cycle = 0

        # Init TK
        self.root = Tk()

        # A sample plant that we will animate.
        self.display = DisplayPlant(self.root, self.universe)
        self.display.update()
        self.update_clock()

        self.root.mainloop()


    def update_clock(self):
        self.physics.run_physics(self.universe)
        self.growth.run_growth(self.universe, PlantBoxMilestone2.SAMPLE_PLANT)
        self.display.update()

        self.cycle = self.cycle + 1
        if self.cycle < PlantUniverse.EVALUATION_CYCLES:
            self.root.after(100, self.update_clock)
