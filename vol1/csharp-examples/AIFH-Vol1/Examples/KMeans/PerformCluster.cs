using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using AIFH_Vol1.Core.General.Data;
using AIFH_Vol1.Core.KMeans;
using AIFH_Vol1.Core.Normalize;

namespace AIFH_Vol1.Examples.KMeans
{
    /// <summary>
    /// Try to cluster the iris data set.
    /// </summary>
    public class PerformCluster
    {
        /// <summary>
        /// The name of this example.
        /// </summary>
        public static string ExampleName = "Attempt to cluster the iris data set";

        /// <summary>
        /// The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 5;

        /// <summary>
        /// Perform the example.
        /// </summary>
        public void Run()
        {

            // read the iris data from the resources
            Assembly assembly = Assembly.GetExecutingAssembly();
            var res = assembly.GetManifestResourceStream("AIFH_Vol1.Resources.iris.csv");

            // did we fail to read the resouce
            if (res == null)
            {
                Console.WriteLine("Can't read iris data from embedded resources.");
                return;
            }

            // load the data
            var istream = new StreamReader(res);
            DataSet ds = DataSet.Load(istream);
            istream.Close();

            // perform the cluster
            IList<BasicData> observations = ds.ExtractUnsupervisedLabeled(4);
            KMeansClustering kmeans = new KMeansClustering(3);
            kmeans.InitForgy(observations);
            int iterations = kmeans.Iteration(1000);
            Console.WriteLine("Finished after " + iterations + " iterations.");

            for (int i = 0; i < kmeans.K; i++)
            {
                Cluster cluster = kmeans.Clusters[i];
                Console.WriteLine("* * * Cluster #" + i);
                foreach (BasicData d in cluster.Observations)
                {
                    Console.WriteLine(d.ToString());
                }
            }

        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            var prg = new PerformCluster();
            prg.Run();
        }
    }
}
