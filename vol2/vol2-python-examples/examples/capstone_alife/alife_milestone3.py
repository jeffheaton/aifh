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

