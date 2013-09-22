using System;
using System.Linq;
using AIFH_Vol1.Core.Discrete;
using AIFH_Vol1.Core.Randomize;

namespace AIFH_Vol1.Examples.Discrete
{
    /// <summary>
    ///     This example program shows how to use discrete simulated annealing to find solutions to
    ///     the Kapsack problem.
    ///     http://en.wikipedia.org/wiki/Knapsack_problem
    /// </summary>
    public class KnapsackAnneal : DiscreteAnneal
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Knapsack optimization with Simulated Annealing";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 9;

        /// <summary>
        ///     Number of items to choose from.
        /// </summary>
        public const int NumItemsToChoose = 25;

        /// <summary>
        ///     The max weight of the knapsack.
        /// </summary>
        public const int KnapsackMaxWeight = 50;

        /// <summary>
        ///     The max weight for an item.
        /// </summary>
        public const int ItemMaxWeight = 20;

        /// <summary>
        ///     The max value for an item.
        /// </summary>
        public const int ItemMaxValue = 1000;

        /// <summary>
        ///     A backup of the items taken, in case we need to revert.
        /// </summary>
        private readonly bool[] _backupTaken;

        /// <summary>
        ///     The best set of items so far.
        /// </summary>
        private readonly bool[] _bestTaken;

        /// <summary>
        ///     The current items taken.
        /// </summary>
        private readonly bool[] _currentTaken;

        /// <summary>
        ///     The profit for each item.
        /// </summary>
        private readonly int[] _profit = new int[NumItemsToChoose + 1];

        /// <summary>
        ///     A random number generator.
        /// </summary>
        private readonly IGenerateRandom _rnd = new MersenneTwisterGenerateRandom();

        /// <summary>
        ///     The weight for each item.
        /// </summary>
        private readonly int[] _weight = new int[NumItemsToChoose + 1];

        /// <summary>
        ///     Construct the object and init.
        /// </summary>
        public KnapsackAnneal()
            : base(1000, 40000, 0.001)
        {
            _currentTaken = new bool[NumItemsToChoose];
            _backupTaken = new bool[NumItemsToChoose];
            _bestTaken = new bool[NumItemsToChoose];

            for (int i = 0; i < _currentTaken.Length; i++)
            {
                _currentTaken[i] = _rnd.NextBoolean();
            }
            Balance();
        }

        /// <summary>
        ///     Run the example.
        /// </summary>
        public void Run()
        {
            // Generate a random set of items.
            for (int n = 0; n < NumItemsToChoose; n++)
            {
                _profit[n] = _rnd.NextInt(ItemMaxValue);
                _weight[n] = _rnd.NextInt(ItemMaxWeight);
            }

            // now begin main loop, and find a minimum
            while (!Done)
            {
                Iteration();
                Console.WriteLine("Iteration #" + K + ", Best Score=" + BestScore + "," + Status);
            }


            // print results
            Console.WriteLine("item" + "\t" + "profit" + "\t" + "weight" + "\t" + "take");
            for (int n = 0; n < NumItemsToChoose; n++)
            {
                Console.WriteLine((n + 1) + "\t" + _profit[n] + "\t" + _weight[n] + "\t" + _bestTaken[n]);
            }
        }

        /// <inheritdoc />
        public override void BackupState()
        {
            Array.Copy(_currentTaken, 0, _backupTaken, 0, _currentTaken.Length);
        }

        /// <inheritdoc />
        public override void RestoreState()
        {
            Array.Copy(_backupTaken, 0, _currentTaken, 0, _currentTaken.Length);
        }

        /// <inheritdoc />
        public override void FoundNewBest()
        {
            Array.Copy(_currentTaken, 0, _bestTaken, 0, _currentTaken.Length);
        }

        /// <inheritdoc />
        public override void MoveToNeighbor()
        {
            // check for strange case where we have everything!
            // This means that the max allowed knapsack weight is greater than the total of grabbing everything.
            // This is kind of pointless, but don't go into an endless loop!
            bool holdingEverythingAlready = _currentTaken.All(aCurrentTaken => aCurrentTaken);

            if (!holdingEverythingAlready)
            {
                // try to add something
                int pt = _rnd.NextInt(_currentTaken.Length); // prime
                while (_currentTaken[pt])
                {
                    pt = _rnd.NextInt(_currentTaken.Length);
                }

                // add the item we found
                _currentTaken[pt] = true;

                // We probably need to drop something now.
                Balance();
            }
        }

        /// <inheritdoc />
        public override double Evaluate()
        {
            if (CalculateTotalWeight() > KnapsackMaxWeight)
            {
                return 0;
            }

            int result = 0;
            for (int i = 0; i < _currentTaken.Length; i++)
            {
                if (_currentTaken[i])
                {
                    result += _profit[i];
                }
            }
            return result;
        }

        /// <summary>
        ///     The total weight.
        /// </summary>
        /// <returns>The total weight.</returns>
        private int CalculateTotalWeight()
        {
            int result = 0;
            for (int i = 0; i < _currentTaken.Length; i++)
            {
                if (_currentTaken[i])
                {
                    result += _weight[i];
                }
            }
            return result;
        }

        /// <summary>
        ///     Balance and keep below max weight.
        /// </summary>
        private void Balance()
        {
            while (CalculateTotalWeight() > KnapsackMaxWeight)
            {
                int remove = _rnd.NextInt(_currentTaken.Length);
                _currentTaken[remove] = false;
            }
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new KnapsackAnneal();
            prg.Run();
        }
    }
}