using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AIFH_Vol3.Core;
using AIFH_Vol3_Core.Core.Energetic;

namespace AIFH_Vol3.Examples.Classic.Boltzmann.TSP
{
    /// <summary>
    /// Use a Boltzmann machine to solve the Traveling Salesman Problem.
    ///
    /// This is based on a an example by Karsten Kutza,
    /// written in C on 1996-01-24. (link now defunct)
    /// http://www.neural-networks-at-your-fingertips.com
    /// </summary>
    public class BoltzTSP

    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Boltzmann Traveling Salesman (TSP).";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 3;

        public const int NUM_CITIES = 4;
        public const int NEURON_COUNT = NUM_CITIES * NUM_CITIES;

        private double gamma = 7;
        private double[][] distance;

        public double sqr(double x)
        {
            return x * x;
        }

        public void CreateCities()
        {
            double x1, x2, y1, y2;
            double alpha1, alpha2;

            this.distance = AIFH.Alloc2D<double>(NUM_CITIES,NUM_CITIES);

            for (int n1 = 0; n1 < NUM_CITIES; n1++)
            {
                for (int n2 = 0; n2 < NUM_CITIES; n2++)
                {
                    alpha1 = ((double)n1 / NUM_CITIES) * 2 * Math.PI;
                    alpha2 = ((double)n2 / NUM_CITIES) * 2 * Math.PI;
                    x1 = Math.Cos(alpha1);
                    y1 = Math.Sin(alpha1);
                    x2 = Math.Cos(alpha2);
                    y2 = Math.Sin(alpha2);
                    distance[n1][n2] = Math.Sqrt(sqr(x1 - x2) + sqr(y1 - y2));
                }
            }
        }

        public bool IsValidTour(double[] data)
        {
            int cities, stops;

            for (int n1 = 0; n1 < NUM_CITIES; n1++)
            {
                cities = 0;
                stops = 0;
                for (int n2 = 0; n2 < NUM_CITIES; n2++)
                {
                    if (data[n1 * NUM_CITIES + n2] > 0)
                    {
                        if (++cities > 1)
                            return false;
                    }
                    if (data[n2 * NUM_CITIES + n1] > 0)
                    {
                        if (++stops > 1)
                            return false;
                    }
                }
                if ((cities != 1) || (stops != 1))
                    return false;
            }
            return true;
        }

        public double LengthOfTour(double[] data)
        {
            double result;
            int n1, n2, n3;

            result = 0;
            for (n1 = 0; n1 < NUM_CITIES; n1++)
            {
                for (n2 = 0; n2 < NUM_CITIES; n2++)
                {
                    if (data[((n1) % NUM_CITIES) * NUM_CITIES + n2] > 0)
                        break;
                }
                for (n3 = 0; n3 < NUM_CITIES; n3++)
                {
                    if (data[((n1 + 1) % NUM_CITIES) * NUM_CITIES + n3] > 0)
                        break;
                }
                result += distance[n2][n3];
            }
            return result;
        }

        String DisplayTour(double[] data)
        {
            StringBuilder result = new StringBuilder();

            int n1, n2;
            bool first;

            for (n1 = 0; n1 < NUM_CITIES; n1++)
            {
                first = true;
                result.Append("[");
                for (n2 = 0; n2 < NUM_CITIES; n2++)
                {
                    if (data[n1 * NUM_CITIES + n2] > 0)
                    {
                        if (first)
                        {
                            first = false;
                            result.Append(n2);
                        }
                        else
                        {
                            result.Append(", ").Append(n2);
                        }
                    }
                }
                result.Append("]");
                if (n1 != NUM_CITIES - 1)
                {
                    result.Append(" -> ");
                }
            }
            return result.ToString();
        }

        public void CalculateWeights(BoltzmannMachine logic)
        {
            for (int sourceTour = 0; sourceTour < NUM_CITIES; sourceTour++)
            {
                for (int sourceCity = 0; sourceCity < NUM_CITIES; sourceCity++)
                {
                    int sourceIndex = sourceTour * NUM_CITIES + sourceCity;
                    for (int targetTour = 0; targetTour < NUM_CITIES; targetTour++)
                    {
                        for (int targetCity = 0; targetCity < NUM_CITIES; targetCity++)
                        {
                            int targetIndex = targetTour * NUM_CITIES + targetCity;
                            double weight = 0;

                            if (sourceIndex != targetIndex)
                            {
                                int predTargetTour = (targetTour == 0 ? NUM_CITIES - 1 : targetTour - 1);
                                int succTargetTour = (targetTour == NUM_CITIES - 1 ? 0 : targetTour + 1);
                                if ((sourceTour == targetTour) || (sourceCity == targetCity))
                                {
                                    weight = -gamma;
                                }
                                else if ((sourceTour == predTargetTour) || (sourceTour == succTargetTour))
                                {
                                    weight = -distance[sourceCity][targetCity];
                                }
                            }
                            logic.SetWeight(sourceIndex, targetIndex, weight);
                        }
                    }
                    logic.Threshold[sourceIndex] = -gamma / 2;
                }
            }
        }


        public void run()
        {
            BoltzmannMachine boltz = new BoltzmannMachine(NEURON_COUNT);

            CreateCities();
            CalculateWeights(boltz);

            boltz.Temperature = 100;
            do
            {
                boltz.EstablishEquilibrium();
                Console.WriteLine(boltz.Temperature + " : " + DisplayTour(boltz.CurrentState));
                boltz.DecreaseTemperature(0.99);
            } while (!IsValidTour(boltz.CurrentState));

            Console.WriteLine("Final Length: " + this.LengthOfTour(boltz.CurrentState));
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            BoltzTSP program = new BoltzTSP();
            program.run();
        }
    }
}
