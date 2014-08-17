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
    GROUND_LINE = UNIVERSE_HEIGHT - (UNIVERSE_HEIGHT / 3)

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
        self.grid = [[PlantCell() for j in range(PlantUniverse.UNIVERSE_WIDTH)] for i in range(PlantUniverse.UNIVERSE_HEIGHT)]


        # The amount of nourishment that is held inside of the roots.  This is used to calculate the
        # root ratio, that limits growth.
        self.root_count = 0

        # The amount of leafy material above the surface.  This is used to calculate the root ratio,
        # that limits growth.
        self.surface_count
