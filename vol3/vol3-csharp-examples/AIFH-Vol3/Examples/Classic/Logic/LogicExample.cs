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
using System.Collections.Generic;

namespace AIFH_Vol3.Examples.Classic.Logic
{
    /// <summary>
    ///     Create a hard-wired (weights directly set) neural network for the logic gates: and, or, not & xor.
    /// </summary>
    public class LogicExample
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Simple hard-wired neural network for logic gates.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 1;

        /// <summary>
        ///     Display a truth table and query the neural network.
        /// </summary>
        /// <param name="inputs">The inputs.</param>
        /// <param name="output">The output.</param>
        public static void TruthTable(IList<InputNeuron> inputs, RegularNeuron output)
        {
            var v = new double[inputs.Count];
            var done = false;

            while (!done)
            {
                for (var i = 0; i < inputs.Count; i++)
                {
                    inputs[i].Value = v[i];
                }

                var o = output.Compute();
                Console.WriteLine(v + " : " + o);

                // Roll forward to next row

                var i2 = 0;
                while (i2 < v.Length)
                {
                    v[i2] += 1;
                    if (v[i2] > 1)
                    {
                        v[i2] = 0;
                        i2 += 1;
                    }
                    else
                    {
                        break;
                    }
                }

                if (i2 == v.Length)
                {
                    done = true;
                }
            }
        }

        /// <summary>
        ///     Create a neural network for AND.
        /// </summary>
        public static void ProcessAnd()
        {
            Console.WriteLine("Boolean AND");
            var inputs = new List<InputNeuron>();
            inputs.Add(new InputNeuron());
            inputs.Add(new InputNeuron());


            var output = new RegularNeuron(-1.5);
            output.Parents.Add(new Connection(1, inputs[0]));
            output.Parents.Add(new Connection(1, inputs[1]));
            TruthTable(inputs, output);
        }

        /// <summary>
        ///     Create a neural network for OR.
        /// </summary>
        public static void ProcessOr()
        {
            Console.WriteLine("Boolean OR");
            var inputs = new List<InputNeuron>();
            inputs.Add(new InputNeuron());
            inputs.Add(new InputNeuron());

            var output = new RegularNeuron(-0.5);
            output.Parents.Add(new Connection(1, inputs[0]));
            output.Parents.Add(new Connection(1, inputs[1]));
            TruthTable(inputs, output);
        }

        /// <summary>
        ///     Create a neural network for NOT.
        /// </summary>
        public static void ProcessNot()
        {
            Console.WriteLine("Boolean NOT");
            var inputs = new List<InputNeuron>();
            inputs.Add(new InputNeuron());

            var output = new RegularNeuron(0.5);
            output.Parents.Add(new Connection(-1, inputs[0]));
            TruthTable(inputs, output);
        }

        /// <summary>
        ///     Create a neural network for XOR.
        /// </summary>
        public static void ProcessXor()
        {
            Console.WriteLine("Boolean XOR");
            var inputs = new List<InputNeuron>();
            inputs.Add(new InputNeuron());
            inputs.Add(new InputNeuron());

            var hidden1 = new List<RegularNeuron>();
            hidden1.Add(new RegularNeuron(-0.5));
            hidden1.Add(new RegularNeuron(-1.5));
            hidden1[0].Parents.Add(new Connection(1, inputs[0]));
            hidden1[0].Parents.Add(new Connection(1, inputs[1]));
            hidden1[1].Parents.Add(new Connection(1, inputs[0]));
            hidden1[1].Parents.Add(new Connection(1, inputs[1]));

            var hidden2 = new RegularNeuron(0.5);
            hidden2.Parents.Add(new Connection(-1, hidden1[1]));
            var output = new RegularNeuron(-1.5);
            output.Parents.Add(new Connection(1, hidden1[0]));
            output.Parents.Add(new Connection(1, hidden2));
            TruthTable(inputs, output);
        }


        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            ProcessAnd();
            ProcessOr();
            ProcessNot();
            ProcessXor();
        }
    }
}