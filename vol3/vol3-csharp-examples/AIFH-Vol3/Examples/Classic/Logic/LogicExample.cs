using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3.Examples.Classic.Logic
{
    /// <summary>
    /// Create a hard-wired (weights directly set) neural network for the logic gates: and, or, not & xor.
    /// </summary>
    public class LogicExample
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Simple hard-wired neural network for logic gates.";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 1;

        /// <summary>
        /// Display a truth table and query the neural network.
        /// </summary>
        /// <param name="inputs">The inputs.</param>
        /// <param name="output">The output.</param>
        public static void TruthTable(IList<InputNeuron> inputs, RegularNeuron output)
        {
            double[] v = new double[inputs.Count];
            bool done = false;

            while (!done)
            {

                for (int i = 0; i < inputs.Count; i++)
                {
                    inputs[i].Value = v[i];
                }

                double o = output.Compute();
                Console.WriteLine(v.ToString() + " : " + o);

                // Roll forward to next row

                int i2 = 0;
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
        /// Create a neural network for AND.
        /// </summary>
        public static void ProcessAnd()
        {
            Console.WriteLine("Boolean AND");
            List<InputNeuron> inputs = new List<InputNeuron>();
            inputs.Add(new InputNeuron());
            inputs.Add(new InputNeuron());


            RegularNeuron output = new RegularNeuron(-1.5);
            output.Parents.Add(new Connection(1, inputs[0]));
            output.Parents.Add(new Connection(1, inputs[1]));
            TruthTable(inputs, output);
        }

        /// <summary>
        /// Create a neural network for OR.
        /// </summary>
        public static void ProcessOr()
        {
            Console.WriteLine("Boolean OR");
            List<InputNeuron> inputs = new List<InputNeuron>();
            inputs.Add(new InputNeuron());
            inputs.Add(new InputNeuron());

            RegularNeuron output = new RegularNeuron(-0.5);
            output.Parents.Add(new Connection(1, inputs[0]));
            output.Parents.Add(new Connection(1, inputs[1]));
            TruthTable(inputs, output);
        }

        /// <summary>
        /// Create a neural network for NOT.
        /// </summary>
        public static void ProcessNot()
        {
            Console.WriteLine("Boolean NOT");
            List<InputNeuron> inputs = new List<InputNeuron>();
            inputs.Add(new InputNeuron());

            RegularNeuron output = new RegularNeuron(0.5);
            output.Parents.Add(new Connection(-1, inputs[0]));
            TruthTable(inputs, output);
        }

        /// <summary>
        /// Create a neural network for XOR.
        /// </summary>
        public static void ProcessXor()
        {
            Console.WriteLine("Boolean XOR");
            List<InputNeuron> inputs = new List<InputNeuron>();
            inputs.Add(new InputNeuron());
            inputs.Add(new InputNeuron());

            List<RegularNeuron> hidden1 = new List<RegularNeuron>();
            hidden1.Add(new RegularNeuron(-0.5));
            hidden1.Add(new RegularNeuron(-1.5));
            hidden1[0].Parents.Add(new Connection(1, inputs[0]));
            hidden1[0].Parents.Add(new Connection(1, inputs[1]));
            hidden1[1].Parents.Add(new Connection(1, inputs[0]));
            hidden1[1].Parents.Add(new Connection(1, inputs[1]));

            RegularNeuron hidden2 = new RegularNeuron(0.5);
            hidden2.Parents.Add(new Connection(-1, hidden1[1]));
            RegularNeuron output = new RegularNeuron(-1.5);
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
