using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace AIFH_Vol2
{
    public class Program
    {
        private const string Method = "ExampleMain";
        private readonly IList<ExampleHolder> _examples = new List<ExampleHolder>();

        public Program()
        {
            List<Type> exampleTypes = Assembly
                .GetExecutingAssembly()
                .GetTypes().ToList()
                .Where(t => t.Namespace != null && t.Namespace.StartsWith("AIFH_Vol2.Examples")).ToList();

            exampleTypes
                .ForEach(e =>
                {
                    if (e.GetMembers().Any(m => m.Name.Equals(Method)))
                    {
                        FieldInfo fiExampleName = e.GetField("ExampleName");
                        FieldInfo fiExampleChapter = e.GetField("ExampleChapter");
                        var exampleName = (string) fiExampleName.GetValue(null);
                        var exampleChapter = (int) fiExampleChapter.GetValue(null);
                        _examples.Add(new ExampleHolder(exampleChapter, exampleName, e));
                    }
                });
        }

        public void DisplayHelp()
        {
            Console.WriteLine("AI For Humans, Volume 2: Nature-Inspired Algorithms");
            Console.WriteLine("To run an example, pass its name as the first argument.\n");
            Console.WriteLine("For example: AIFH-Vol2 CSVExample");
            Console.WriteLine();
            Console.WriteLine("Available Examples:");

            DisplayChapter(1, "Population and Scoring");
            DisplayChapter(2, "Crossover and Mutation");
            DisplayChapter(3, "Genetic Algorithms");
            DisplayChapter(4, "Genetic Programming");
            DisplayChapter(5, "Speciation");
            DisplayChapter(6, "Particle Swarm Optimization");
            DisplayChapter(7, "Ant Colony Optimization");
            DisplayChapter(8, "Cellular Automation");
            DisplayChapter(9, "Artificial Life");
            DisplayChapter(10, "Modeling Problems");
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
                    holder.ExampleType.GetMethod(Method).Invoke(null, new object[] {args});
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
            bool success = false;

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
                var args2 = new string[args.Length - index-1];
                for (int i = 0; i < args2.Length; i++)
                {
                    args2[i] = args[index + i+1];
                }
                RunExample(args[index],args2);
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