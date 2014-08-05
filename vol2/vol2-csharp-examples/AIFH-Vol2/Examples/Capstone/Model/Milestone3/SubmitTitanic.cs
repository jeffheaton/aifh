// Artificial Intelligence for Humans
// Volume 2: Nature-Inspired Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2014 by Jeff Heaton
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
using System.IO;
using AIFH_Vol2.Core.General.Data;
using AIFH_Vol2.Core.Learning;
using AIFH_Vol2.Examples.Capstone.Model.Milestone1;
using AIFH_Vol2.Examples.Capstone.Model.Milestone2;
using CsvHelper;

namespace AIFH_Vol2.Examples.Capstone.Model.Milestone3
{
    public class SubmitTitanic
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Titanic Milestone 3: Submit a dataset to Kaggle.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 10;

        /// <summary>
        ///     Prepare a Kaggle submission for Titanic.
        /// </summary>
        /// <param name="dataPath">The data path.</param>
        /// <param name="bestNetwork">The best network.</param>
        /// <param name="cross">The cross validated data.</param>
        public void Submit(string dataPath, RBFNetwork bestNetwork, CrossValidate cross)
        {
            String now = new DateTime().ToString("yyyyMMddhhmm");
            string trainingPath = Path.Combine(dataPath, TitanicConfig.TrainingFilename);
            string testPath = Path.Combine(dataPath, TitanicConfig.TestFilename);
            var score = (int) (cross.Score*10000);
            string submitPath = Path.Combine(dataPath, "submit-" + now + "_" + score + ".csv");
            string submitInfoPath = Path.Combine(dataPath, "submit-" + now + ".txt");

            using (var file = new StreamWriter(submitInfoPath))
            {
                file.WriteLine("Crossvalidation stats:");
                for (int i = 0; i < cross.Count; i++)
                {
                    CrossValidateFold fold = cross.Folds[i];
                    file.WriteLine("Fold #" + (i + 1) + " : Score: " + fold.Score);
                }
                file.WriteLine("Average Score: " + cross.Score);
                file.WriteLine();
                file.WriteLine(String.Join(",", bestNetwork.LongTermMemory));
            }

            var stats = new TitanicStats();
            NormalizeTitanic.Analyze(stats, trainingPath);
            NormalizeTitanic.Analyze(stats, testPath);

            var ids = new List<String>();
            IList<BasicData> training = NormalizeTitanic.Normalize(stats, testPath, ids,
                TitanicConfig.InputNormalizeLow,
                TitanicConfig.InputNormalizeHigh,
                TitanicConfig.PredictSurvive,
                TitanicConfig.PredictPerish);

            int idx = 0;
            using (var streamWriter = new StreamWriter(submitPath))
            using (var writer = new CsvWriter(streamWriter))
            {
                writer.WriteField("PassengerId");
                writer.WriteField("Survived");
                writer.NextRecord();

                foreach (BasicData data in training)
                {
                    double[] output = bestNetwork.ComputeRegression(data.Input);
                    int survived = output[0] > 0.5 ? 1 : 0;

                    writer.WriteField(ids[idx]);
                    writer.WriteField(survived);
                    writer.NextRecord();
                    idx++;
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
            string filename = "";

            if (args.Length > 0)
            {
                filename = args[0];
                string dataPath = filename;

                var fit = new FitTitanic();
                fit.Process(dataPath);

                RBFNetwork bestNetwork = fit.BestNetwork;

                var submit = new SubmitTitanic();
                submit.Submit(dataPath, bestNetwork, fit.Crossvalidation);
            }
            else
            {
                Console.WriteLine("Please provide your data directory path as the first argument.");
            }
        }
    }
}
