using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol1
{
    
    public class Program
    {
        const string Method = "ExampleMain";
        private IList<ExampleHolder> _examples = new List<ExampleHolder>();

        public Program()
        {
            var exampleTypes = Assembly
                    .GetExecutingAssembly()
                    .GetTypes().ToList()
                    .Where(t => t.Namespace != null && t.Namespace.StartsWith("AIFH_Vol1.Examples")).ToList();

            exampleTypes
                .ForEach(e =>
                {
                    if (e.GetMembers().Any(m => m.Name.Equals(Method)))
                    {
                        FieldInfo fiExampleName = e.GetField("ExampleName");
                        FieldInfo fiExampleChapter = e.GetField("ExampleChapter");
                        string exampleName = (string)fiExampleName.GetValue(null);
                        int exampleChapter = (int)fiExampleChapter.GetValue(null);
                        _examples.Add(new ExampleHolder(exampleChapter,exampleName,e));

                        //var info = e.InvokeMember(method, BindingFlags.Default | BindingFlags.InvokeMethod, null, null, new object[] { });
                        //examples.Add((ExampleInfo)info);
                    }
                });
        }

        public void DisplayHelp()
        {
            Console.WriteLine("AI For Humans, Volume 1: Fundamental Algorithms");
            Console.WriteLine("To run an example, pass its name as the first argument.\n");
            Console.WriteLine("For example: AIFH-Vol1 CSVExample");
            Console.WriteLine();
            Console.WriteLine("Available Examples:");

            DisplayChapter(1, "Introduction to AI");
            DisplayChapter(2, "Normalizing Data");
            DisplayChapter(3, "Distance Metrics");
            DisplayChapter(4, "Random Numbers");
            DisplayChapter(5, "K-Means");
            DisplayChapter(6, "Error Calculation");
            DisplayChapter(7, "Towards Machine Learning");
            DisplayChapter(8, "Optimization Algorithms");
            DisplayChapter(9, "Discrete Optimization");
            DisplayChapter(10, "Linear Regression");

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

        public void RunExample(string name)
        {
            foreach (ExampleHolder holder in _examples)
            {
                if (holder.ExampleType.Name.Equals(name))
                {
                    var args = new string[0];
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
                RunExample(args[index]);
            }

            if (pause)
            {
                Pause();
            }
        }

        static void Main(string[] args)
        {
            (new Program()).Run(args);
        }
    }
}
