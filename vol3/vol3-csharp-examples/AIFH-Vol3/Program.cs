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
using System.Linq;
using System.Reflection;

namespace AIFH_Vol3
{
    class Program
    {
        private const string Method = "ExampleMain";
        private readonly IList<ExampleHolder> _examples = new List<ExampleHolder>();

        public Program()
        {
            List<Type> exampleTypes = Assembly
                .GetExecutingAssembly()
                .GetTypes().ToList()
                .Where(t => t.Namespace != null && t.Namespace.StartsWith("AIFH_Vol3.Examples")).ToList();

            exampleTypes
                .ForEach(e =>
                {
                    if (e.GetMembers().Any(m => m.Name.Equals(Method)))
                    {
                        FieldInfo fiExampleName = e.GetField("ExampleName");
                        FieldInfo fiExampleChapter = e.GetField("ExampleChapter");
                        var exampleName = (string)fiExampleName.GetValue(null);
                        var exampleChapter = (int)fiExampleChapter.GetValue(null);
                        _examples.Add(new ExampleHolder(exampleChapter, exampleName, e));
                    }
                });
        }

        public void DisplayHelp()
        {
            Console.WriteLine("AI For Humans, Volume 3: Deep Learning and Neural Networks");
            Console.WriteLine("To run an example, pass its name as the first argument.\n");
            Console.WriteLine("For example: AIFH-Vol3 CSVExample");
            Console.WriteLine();
            Console.WriteLine("Available Examples:");

            DisplayChapter(1,"Neural Network Basics");
            DisplayChapter(2,"Self Organizing Maps");
            DisplayChapter(3,"Hopfield & Boltzmann Machines");
            DisplayChapter(4,"Feedforward Neural Networks");
            DisplayChapter(5,"Training & Evaluation");
            DisplayChapter(6,"Backpropagation Training");
            DisplayChapter(7,"Other Propagation Training");
            DisplayChapter(8,"NEAT, CPPN and HyperNEAT");
            DisplayChapter(9,"Deep Learning");
            DisplayChapter(10,"Convolutional Neural Networks");
            DisplayChapter(11,"Pruning and Model Selection");
            DisplayChapter(12,"Dropout and Regularization");
            DisplayChapter(13,"Time Series and Recurrent Networks");
            DisplayChapter(14,"Architecting Neural Networks");
            DisplayChapter(15,"Visualization");
            DisplayChapter(16,"Modelling with Neural Networks");
        }

        public void DisplayChapter(int chapterNumber, string chapterTitle)
        {
            Console.WriteLine();
            Console.WriteLine("** Chapter " + chapterNumber + ": " + chapterTitle + ", Examples:");
            foreach (ExampleHolder holder in _examples.Where(holder => holder.ExampleChapter == chapterNumber))
            {
                Console.WriteLine(holder.ExampleType.Name + " : " + holder.ExampleName);
            }
        }

        public void RunExample(string name, string[] args)
        {
            foreach (ExampleHolder holder in _examples)
            {
                if (holder.ExampleType.Name.Equals(name))
                {
                    Console.WriteLine("Running example: " + holder.ExampleType.Name);
                    holder.ExampleType.GetMethod(Method).Invoke(null, new object[] { args });
                }
            }
        }

        public void Pause()
        {
            Console.Write("\n\nPress any key to continue...");
            Console.ReadKey();
        }

        public void Run(string[] args)
        {
            int index = 0;
            bool pause = false;

            // process any options

            while (index < args.Length && args[index][0] == '-')
            {
                String option = args[index].Substring(1).ToLower();
                if ("pause".Equals(option))
                    pause = true;
                index++;
            }


            if (index >= args.Length)
            {
                DisplayHelp();
            }
            else
            {
                var args2 = new string[args.Length - index - 1];
                for (int i = 0; i < args2.Length; i++)
                {
                    args2[i] = args[index + i + 1];
                }
                RunExample(args[index], args2);
            }

            if (pause)
            {
                Pause();
            }
        }

        private static void Main(string[] args)
        {
            (new Program()).Run(args);
        }
    }
}
