using AIFH_Vol2.Core.Genetic.Genome;
using AIFH_Vol2.Core.Learning;
using AIFH_Vol2.Core.Learning.Score;
using AIFH_Vol2.Examples.GA.TSP;

namespace AIFH_Vol2.Examples.GA.TSP
{
    /// <summary>
    ///     Calculate a score for the TSP.
    /// </summary>
    public class TSPScore : IScoreFunction
    {
        /// <summary>
        ///     The path of cities to visit.
        /// </summary>
        private readonly City[] _cities;

        /// <summary>
        ///     The constructor.
        /// </summary>
        /// <param name="cities">The cities.</param>
        public TSPScore(City[] cities)
        {
            _cities = cities;
        }

        /// <inheritdoc />
        public double CalculateScore(IMLMethod phenotype)
        {
            double result = 0.0;
            var genome = (IntegerArrayGenome) phenotype;
            int[] path = genome.Data;

            for (int i = 0; i < _cities.Length - 1; i++)
            {
                City city1 = _cities[path[i]];
                City city2 = _cities[path[i + 1]];

                double dist = city1.Proximity(city2);
                result += dist;
            }

            return result;
        }

        /// <inheritdoc />
        public bool ShouldMinimize
        {
            get { return true; }
        }
    }
}