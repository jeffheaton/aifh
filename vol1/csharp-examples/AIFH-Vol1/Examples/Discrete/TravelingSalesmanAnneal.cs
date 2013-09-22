using System;
using AIFH_Vol1.Core.Discrete;
using AIFH_Vol1.Core.Distance;
using AIFH_Vol1.Core.General;
using AIFH_Vol1.Core.Randomize;

namespace AIFH_Vol1.Examples.Discrete
{
    /// <summary>
    ///     Use simulated annealing with the Traveling Salesman Problem (TSP).  The cities are placed
    ///     in a circle, so the ideal path is known.  Because the cities are in a circle they should be
    ///     visited in order for the absolute optimal path.
    ///     http://en.wikipedia.org/wiki/Traveling_salesman_problem
    /// </summary>
    public class TravelingSalesmanAnneal : DiscreteAnneal
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Traveling Salesman with Simulated Annealing";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 9;

        /// <summary>
        ///     The size of the map.
        /// </summary>
        public const double MapSize = 10;

        /// <summary>
        ///     The city count.
        /// </summary>
        public const int CityCount = 50;

        /// <summary>
        ///     The distance calculator.
        /// </summary>
        private readonly ICalculateDistance _distance = new EuclideanDistance();

        /// <summary>
        ///     A random number generator.
        /// </summary>
        private readonly IGenerateRandom _rnd = new MersenneTwisterGenerateRandom();

        /// <summary>
        ///     The backup path, in case the current is not kept.
        /// </summary>
        private int[] _backupPath;

        /// <summary>
        ///     The best path yet.
        /// </summary>
        private int[] _bestPath;

        /// <summary>
        ///     The city coordinates.
        /// </summary>
        private double[][] _cities;

        /// <summary>
        ///     The current path being evaluated.
        /// </summary>
        private int[] _currentPath;

        /// <summary>
        ///     Construct the object.
        /// </summary>
        public TravelingSalesmanAnneal() : base(1000, 400, 0.001)
        {
        }

        /// <summary>
        ///     Run the example.
        /// </summary>
        public void Run()
        {
            _cities = new double[CityCount][];
            _currentPath = new int[CityCount];
            _backupPath = new int[CityCount];
            _bestPath = new int[CityCount];

            // place the cities in a circle
            double ratio = (2*Math.PI)/_cities.Length;

            for (int cityNumber = 0; cityNumber < _cities.Length; cityNumber++)
            {
                _cities[cityNumber] = new double[2];
                _cities[cityNumber][0] = (int) (Math.Cos(ratio*cityNumber)*(MapSize/2) + (MapSize/2));
                _cities[cityNumber][1] = (int) (Math.Sin(ratio*cityNumber)*(MapSize/2) + (MapSize/2));
            }

            // pick a random city order
            _currentPath = new int[CityCount];
            for (int i = 0; i < _currentPath.Length; i++)
            {
                int city;
                bool foundCity;

                do
                {
                    city = _rnd.NextInt(CityCount);
                    foundCity = false;
                    for (int j = 0; j < i; j++)
                    {
                        if (city == _currentPath[j])
                        {
                            foundCity = true;
                        }
                    }
                } while (foundCity);

                _currentPath[i] = city;
            }

            // now begin main loop, and find a minimum
            while (!Done)
            {
                Iteration();
                Console.WriteLine("Iteration #" + K + ", Best Score=" + BestScore + "," + Status);
            }

            foreach (var item in _bestPath)
                Console.Write(item + " ");
            Console.WriteLine();
        }

        /// <inheritdoc/>
        public override void BackupState()
        {
            Array.Copy(_currentPath, 0, _backupPath, 0, _currentPath.Length);
        }

        /// <inheritdoc/>
        public override void RestoreState()
        {
            Array.Copy(_backupPath, 0, _currentPath, 0, _currentPath.Length);
        }

        /// <inheritdoc/>
        public override void FoundNewBest()
        {
            Array.Copy(_currentPath, 0, _bestPath, 0, _currentPath.Length);
        }

        /// <inheritdoc/>
        public override void MoveToNeighbor()
        {
            // pick the first point to swap
            int pt1 = _rnd.NextInt(_currentPath.Length);

            // pick the second point to swap, can't be the same as the first
            int pt2;

            do
            {
                pt2 = _rnd.NextInt(_currentPath.Length);
            } while (pt1 == pt2);

            // swap them
            int temp = _currentPath[pt1];
            _currentPath[pt1] = _currentPath[pt2];
            _currentPath[pt2] = temp;
        }

        /// <inheritdoc/>
        public override double Evaluate()
        {
            double result = 0;
            for (int i = 0; i < (_cities.Length - 1); i++)
            {
                // find current and next city
                double[] city1 = _cities[_currentPath[i]];
                double[] city2 = _cities[_currentPath[i + 1]];
                result += _distance.Calculate(city1, city2);
            }

            return result;
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new TravelingSalesmanAnneal();
            prg.Run();
        }
    }
}