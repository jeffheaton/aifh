// Artificial Intelligence for Humans
// Volume 3: Deep Learning and Neural Networks
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2015 by Jeff Heaton
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// For more information on Heaton Research copyrights, licenses
// and trademarks visit:
// http://www.heatonresearch.com/copyright
//

using System;
using System.Text;
using AIFH_Vol3.Core;
using AIFH_Vol3_Core.Core.Energetic;

namespace AIFH_Vol3.Examples.Classic.Boltzmann.TSP
{
    /// <summary>
    ///     Use a Boltzmann machine to solve the Traveling Salesman Problem.
    ///     This is based on a an example by Karsten Kutza,
    ///     written in C on 1996-01-24. (link now defunct)
    ///     http://www.neural-networks-at-your-fingertips.com
    /// </summary>
    public class BoltzTSP

    {
        public const int NUM_CITIES = 4;
        public const int NEURON_COUNT = NUM_CITIES*NUM_CITIES;

        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Boltzmann Traveling Salesman (TSP).";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 3;

        private double[][] distance;

        private readonly double gamma = 7;

        public double sqr(double x)
        {
            return x*x;
        }

        public void CreateCities()
        {
            double x1, x2, y1, y2;
            double alpha1, alpha2;

            distance = AIFH.Alloc2D<double>(NUM_CITIES, NUM_CITIES);

            for (var n1 = 0; n1 < NUM_CITIES; n1++)
            {
                for (var n2 = 0; n2 < NUM_CITIES; n2++)
                {
                    alpha1 = (double) n1/NUM_CITIES*2*Math.PI;
                    alpha2 = (double) n2/NUM_CITIES*2*Math.PI;
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

            for (var n1 = 0; n1 < NUM_CITIES; n1++)
            {
                cities = 0;
                stops = 0;
                for (var n2 = 0; n2 < NUM_CITIES; n2++)
                {
                    if (data[n1*NUM_CITIES + n2] > 0)
                    {
                        if (++cities > 1)
                            return false;
                    }
                    if (data[n2*NUM_CITIES + n1] > 0)
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
                    if (data[n1%NUM_CITIES*NUM_CITIES + n2] > 0)
                        break;
                }
                for (n3 = 0; n3 < NUM_CITIES; n3++)
                {
                    if (data[(n1 + 1)%NUM_CITIES*NUM_CITIES + n3] > 0)
                        break;
                }
                result += distance[n2][n3];
            }
            return result;
        }

        private string DisplayTour(double[] data)
        {
            var result = new StringBuilder();

            int n1, n2;
            bool first;

            for (n1 = 0; n1 < NUM_CITIES; n1++)
            {
                first = true;
                result.Append("[");
                for (n2 = 0; n2 < NUM_CITIES; n2++)
                {
                    if (data[n1*NUM_CITIES + n2] > 0)
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
            for (var sourceTour = 0; sourceTour < NUM_CITIES; sourceTour++)
            {
                for (var sourceCity = 0; sourceCity < NUM_CITIES; sourceCity++)
                {
                    var sourceIndex = sourceTour*NUM_CITIES + sourceCity;
                    for (var targetTour = 0; targetTour < NUM_CITIES; targetTour++)
                    {
                        for (var targetCity = 0; targetCity < NUM_CITIES; targetCity++)
                        {
                            var targetIndex = targetTour*NUM_CITIES + targetCity;
                            double weight = 0;

                            if (sourceIndex != targetIndex)
                            {
                                var predTargetTour = targetTour == 0 ? NUM_CITIES - 1 : targetTour - 1;
                                var succTargetTour = targetTour == NUM_CITIES - 1 ? 0 : targetTour + 1;
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
                    logic.Threshold[sourceIndex] = -gamma/2;
                }
            }
        }


        public void run()
        {
            var boltz = new BoltzmannMachine(NEURON_COUNT);

            CreateCities();
            CalculateWeights(boltz);

            boltz.Temperature = 100;
            do
            {
                boltz.EstablishEquilibrium();
                Console.WriteLine(boltz.Temperature + " : " + DisplayTour(boltz.CurrentState));
                boltz.DecreaseTemperature(0.99);
            } while (!IsValidTour(boltz.CurrentState));

            Console.WriteLine("Final Length: " + LengthOfTour(boltz.CurrentState));
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var program = new BoltzTSP();
            program.run();
        }
    }
}