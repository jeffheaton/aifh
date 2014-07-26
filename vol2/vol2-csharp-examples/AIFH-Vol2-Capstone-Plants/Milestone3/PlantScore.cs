using AIFH_Vol2.Core.Genetic.Genome;
using AIFH_Vol2.Core.Learning;
using AIFH_Vol2.Core.Learning.Score;
using AIFH_Vol2_Capstone_Plants.Milestone1;
using AIFH_Vol2_Capstone_Plants.Milestone2;

namespace AIFH_Vol2_Capstone_Plants.Milestone3
{
    /// <summary>
    ///     This class is used to score the plant.  Plants are scored for how green they are after a specified
    ///     number of iterations.
    /// </summary>
    public class PlantScore : IScoreFunction
    {
        /// <inheritdoc />
        public double CalculateScore(IMLMethod algo)
        {
            var genome = (DoubleArrayGenome) algo;
            var universe = new PlantUniverse();
            universe.Reset();
            var physics = new PlantPhysics();
            var growth = new PlantGrowth();

            // Run the generations.
            for (int i = 0; i < PlantUniverse.EvaluationCycles; i++)
            {
                physics.RunPhysics(universe);
                growth.RunGrowth(universe, genome.Data);
            }

            // Count the amount of green.
            int count = 0;
            double sum = 0;
            for (int row = 0; row < PlantUniverse.UniverseHeight; row++)
            {
                for (int col = 0; col < PlantUniverse.UniverseWidth; col++)
                {
                    PlantUniverseCell cell = universe.GetCell(row, col);
                    if (cell.IsAlive)
                    {
                        if (row >= PlantUniverse.GroundLine)
                        {
                            sum += 0.5;
                        }
                        else
                        {
                            sum += cell.Leafyness;
                        }
                    }
                    count++;
                }
            }
            return sum/count;
        }

        /// <inheritdoc />
        public bool ShouldMinimize
        {
            get { return false; //To change body of implemented methods use File | Settings | File Templates.
            }
        }
    }
}