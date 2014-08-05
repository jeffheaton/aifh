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
using System.Globalization;
using System.IO;
using AIFH_Vol2.Core.General.Data;
using CsvHelper;

namespace AIFH_Vol2.Examples.Capstone.Model.Milestone1
{
    /// <summary>
    /// This capstone project shows how to apply some of the techniques in this book to data science.  The data set used
    /// is the Kaggle titanic data set.  You can find that data set here.
    ///
    /// http://www.kaggle.com/c/titanic-gettingStarted
    ///
    /// There are three parts to this assignment.
    ///
    /// part 1: Obtain and normalize data, extrapolate features
    /// Part 2: Cross validate and select model hyperparameters
    /// Part 3: Build Kaggle submission file
    ///
    /// This is part 1 of the project.
    ///
    /// [age,sex-male,pclass,sibsp,parch,fare,embarked-c,embarked-q,embarked-s,name-mil,
    /// name-nobility,name-dr,name-clergy]
    /// </summary>
    public class NormalizeTitanic
    {
        /// <summary>
        ///     The name of this example.
        /// </summary>
        public static string ExampleName = "Titanic Milestone 1: Normalize the data set.";

        /// <summary>
        ///     The chapter this example is from.
        /// </summary>
        public static int ExampleChapter = 10;


        /// <summary>
        ///     Analyze and generate stats for titanic data.
        /// </summary>
        /// <param name="stats">The stats for titanic.</param>
        /// <param name="filename">The file to analyze.</param>
        /// <returns>The passenger count.</returns>
        public static int Analyze(TitanicStats stats, string filename)
        {
            int count = 0;
            var headerMap = new Dictionary<string, int>();

            using (var reader = new CsvReader(new StreamReader(filename)))
            {
                reader.Read();

                for (int i = 0; i < reader.FieldHeaders.Length; i++)
                {
                    headerMap[reader.FieldHeaders[i].ToLower()] = i;
                }

                int ageIndex = headerMap["age"];
                int nameIndex = headerMap["name"];
                int sexIndex = headerMap["sex"];
                int indexEmbarked = headerMap["embarked"];
                int indexFare = headerMap["fare"];
                int indexPclass = headerMap["pclass"];

                int survivedIndex = -1;

                // test data does not have survived
                if (headerMap.ContainsKey("survived"))
                {
                    survivedIndex = headerMap["survived"];
                }

                do
                {
                    count++;
                    String name = reader[nameIndex];
                    String ageStr = reader[ageIndex];
                    String sexStr = reader[sexIndex];
                    String embarkedStr = reader[indexEmbarked];

                    // test data does not have survived, do not use survived boolean if using test data!
                    bool survived = false;
                    if (survivedIndex != -1)
                    {
                        String survivedStr = reader[survivedIndex];
                        survived = survivedStr.Equals("1");
                    }

                    if (indexEmbarked != -1)
                    {
                        embarkedStr = reader[indexEmbarked];
                    }

                    // calculate average fare per class
                    String strFare = reader[indexFare];
                    if (strFare.Length > 0)
                    {
                        double fare = double.Parse(strFare, CultureInfo.InvariantCulture);
                        string pclass = reader[indexPclass];
                        if (pclass.Equals("1"))
                        {
                            stats.MeanFare1.Update(fare);
                        }
                        else if (pclass.Equals("2"))
                        {
                            stats.MeanFare2.Update(fare);
                        }
                        else if (pclass.Equals("3"))
                        {
                            stats.MeanFare3.Update(fare);
                        }
                    }


                    bool isMale = string.Compare(sexStr, "male", true) == 0;
                    double age;

                    // Only compute survival stats on training data
                    if (survivedIndex != -1)
                    {
                        if (embarkedStr.Equals("Q"))
                        {
                            stats.EmbarkedQ.Update(isMale, survived);
                        }
                        else if (embarkedStr.Equals("S"))
                        {
                            stats.EmbarkedS.Update(isMale, survived);
                        }
                        else if (embarkedStr.Equals("C"))
                        {
                            stats.EmbarkedC.Update(isMale, survived);
                        }
                    }

                    stats.EmbarkedHisto.Update(embarkedStr);

                    // Only compute survival stats on training data.
                    if (survivedIndex != -1)
                    {
                        stats.SurvivalTotal.Update(isMale, survived);
                    }

                    if (survivedIndex != -1)
                    {
                        if (name.Contains("Master."))
                        {
                            stats.SurvivalMaster.Update(isMale, survived);
                        }
                        else if (name.Contains("Mr."))
                        {
                            stats.SurvivalMr.Update(isMale, survived);
                        }
                        else if (name.Contains("Miss.") || name.Contains("Mlle."))
                        {
                            stats.SurvivalMiss.Update(isMale, survived);
                        }
                        else if (name.Contains("Mrs.") || name.Contains("Mme."))
                        {
                            stats.SurvivalMrs.Update(isMale, survived);
                        }
                        else if (name.Contains("Col.") || name.Contains("Capt.") || name.Contains("Major."))
                        {
                            stats.SurvivalMilitary.Update(isMale, survived);
                        }
                        else if (name.Contains("Countess.") || name.Contains("Lady.") || name.Contains("Sir.") ||
                                 name.Contains("Don.") || name.Contains("Dona.") || name.Contains("Jonkheer."))
                        {
                            stats.SurvivalNobility.Update(isMale, survived);
                        }
                        else if (name.Contains("Dr."))
                        {
                            stats.SurvivalDr.Update(isMale, survived);
                        }
                        else if (name.Contains("Rev."))
                        {
                            stats.SurvivalClergy.Update(isMale, survived);
                        }
                    }

                    if (ageStr.Length > 0)
                    {
                        age = double.Parse(ageStr, CultureInfo.InvariantCulture);

                        // Update general mean age for male/female
                        if (isMale)
                        {
                            stats.MeanMale.Update(age);
                        }
                        else
                        {
                            stats.MeanFemale.Update(age);
                        }

                        // Update the total average age
                        stats.MeanTotal.Update(age);

                        if (name.Contains("Master."))
                        {
                            stats.MeanMaster.Update(age);
                            // Only compute survival stats on training data.
                            if (survivedIndex != -1)
                            {
                                stats.SurvivalMaster.Update(isMale, survived);
                            }
                        }
                        else if (name.Contains("Mr."))
                        {
                            stats.MeanMr.Update(age);
                            // Only compute survival stats on training data.
                            if (survivedIndex != -1)
                            {
                                stats.SurvivalMr.Update(isMale, survived);
                            }
                        }
                        else if (name.Contains("Miss.") || name.Contains("Mlle."))
                        {
                            stats.MeanMiss.Update(age);
                            // Only compute survival stats on training data.
                            if (survivedIndex != -1)
                            {
                                stats.SurvivalMiss.Update(isMale, survived);
                            }
                        }
                        else if (name.Contains("Mrs.") || name.Contains("Mme."))
                        {
                            stats.MeanMrs.Update(age);
                            // Only compute survival stats on training data.
                            if (survivedIndex != -1)
                            {
                                stats.SurvivalMrs.Update(isMale, survived);
                            }
                        }
                        else if (name.Contains("Col.") || name.Contains("Capt.") || name.Contains("Major."))
                        {
                            stats.MeanMilitary.Update(age);
                            // Only compute survival stats on training data.
                            if (survivedIndex != -1)
                            {
                                stats.SurvivalMilitary.Update(isMale, survived);
                            }
                        }
                        else if (name.Contains("Countess.") || name.Contains("Lady.") || name.Contains("Sir.") ||
                                 name.Contains("Don.") || name.Contains("Dona.") || name.Contains("Jonkheer."))
                        {
                            stats.MeanNobility.Update(age);
                            // Only compute survival stats on training data.
                            if (survivedIndex != -1)
                            {
                                stats.SurvivalNobility.Update(isMale, survived);
                            }
                        }
                        else if (name.Contains("Dr."))
                        {
                            stats.MeanDr.Update(age);
                            // Only compute survival stats on training data.
                            if (survivedIndex != -1)
                            {
                                stats.SurvivalDr.Update(isMale, survived);
                            }
                        }
                        else if (name.Contains("Rev."))
                        {
                            stats.MeanClergy.Update(age);
                            // Only compute survival stats on training data.
                            if (survivedIndex != -1)
                            {
                                stats.SurvivalClergy.Update(isMale, survived);
                            }
                        }
                    }
                } while (reader.Read());
                return count;
            }
        }

        /// <summary>
        ///     Normalize to a range.
        /// </summary>
        /// <param name="x">The value to normalize.</param>
        /// <param name="dataLow">The low end of the range of the data.</param>
        /// <param name="dataHigh">The high end of the range of the data.</param>
        /// <param name="normalizedLow">The normalized low end of the range of data.</param>
        /// <param name="normalizedHigh">The normalized high end of the range of data.</param>
        /// <returns>The normalized value.</returns>
        public static double RangeNormalize(double x, double dataLow, double dataHigh, double normalizedLow,
            double normalizedHigh)
        {
            return ((x - dataLow)
                    / (dataHigh - dataLow))
                   * (normalizedHigh - normalizedLow) + normalizedLow;
        }

        public static IList<BasicData> Normalize(TitanicStats stats, string filename, List<String> ids,
            double inputLow, double inputHigh,
            double predictSurvive, double predictPerish)
        {
            IList<BasicData> result = new List<BasicData>();

            var headerMap = new Dictionary<string, int>();

            using (var reader = new CsvReader(new StreamReader(filename)))
            {
                reader.Read();

                for (int i = 0; i < reader.FieldHeaders.Length; i++)
                {
                    headerMap[reader.FieldHeaders[i].ToLower()] = i;
                }


                int ageIndex = headerMap["age"];
                int nameIndex = headerMap["name"];
                int sexIndex = headerMap["sex"];
                int indexEmbarked = headerMap["embarked"];
                int indexPclass = headerMap["pclass"];
                int indexSibsp = headerMap["sibsp"];
                int indexParch = headerMap["parch"];
                int indexFare = headerMap["fare"];
                int indexId = headerMap["passengerid"];
                int survivedIndex = -1;

                // test data does not have survived
                if (headerMap.ContainsKey("survived"))
                {
                    survivedIndex = headerMap["survived"];
                }


                do
                {
                    var data = new BasicData(TitanicConfig.InputFeatureCount, 1);

                    String name = reader[nameIndex];
                    String sex = reader[sexIndex];
                    String embarked = reader[indexEmbarked];
                    String id = reader[indexId];

                    // Add record the passenger id, if requested
                    if (ids != null)
                    {
                        ids.Add(id);
                    }

                    bool isMale = string.Compare(sex, "male", true) == 0;


                    // age
                    double age;

                    // do we have an age for this person?
                    if (reader[ageIndex].Length == 0)
                    {
                        // age is missing, interpolate using name
                        if (name.Contains("Master."))
                        {
                            age = stats.MeanMaster.Calculate();
                        }
                        else if (name.Contains("Mr."))
                        {
                            age = stats.MeanMr.Calculate();
                        }
                        else if (name.Contains("Miss.") || name.Contains("Mlle."))
                        {
                            age = stats.MeanMiss.Calculate();
                        }
                        else if (name.Contains("Mrs.") || name.Contains("Mme."))
                        {
                            age = stats.MeanMrs.Calculate();
                        }
                        else if (name.Contains("Col.") || name.Contains("Capt.") || name.Contains("Major."))
                        {
                            age = stats.MeanMiss.Calculate();
                        }
                        else if (name.Contains("Countess.") || name.Contains("Lady.") || name.Contains("Sir.") ||
                                 name.Contains("Don.") || name.Contains("Dona.") || name.Contains("Jonkheer."))
                        {
                            age = stats.MeanNobility.Calculate();
                        }
                        else if (name.Contains("Dr."))
                        {
                            age = stats.MeanDr.Calculate();
                        }
                        else if (name.Contains("Rev."))
                        {
                            age = stats.MeanClergy.Calculate();
                        }
                        else
                        {
                            if (isMale)
                            {
                                age = stats.MeanMale.Calculate();
                            }
                            else
                            {
                                age = stats.MeanFemale.Calculate();
                            }
                        }
                    }
                    else
                    {
                        age = Double.Parse(reader[ageIndex], CultureInfo.InvariantCulture);
                    }
                    data.Input[0] = RangeNormalize(age, 0, 100, inputLow, inputHigh);

                    // sex-male
                    data.Input[1] = isMale ? inputHigh : inputLow;

                    // pclass
                    double pclass = double.Parse(reader[indexPclass], CultureInfo.InvariantCulture);
                    data.Input[2] = RangeNormalize(pclass, 1, 3, inputLow, inputHigh);

                    // sibsp
                    double sibsp = double.Parse(reader[indexSibsp], CultureInfo.InvariantCulture);
                    data.Input[3] = RangeNormalize(sibsp, 0, 10, inputLow, inputHigh);

                    // parch
                    double parch = double.Parse(reader[indexParch], CultureInfo.InvariantCulture);
                    data.Input[4] = RangeNormalize(parch, 0, 10, inputLow, inputHigh);

                    // fare
                    String strFare = reader[indexFare];
                    double fare;

                    if (strFare.Length == 0)
                    {
                        if (((int) pclass) == 1)
                        {
                            fare = stats.MeanFare1.Calculate();
                        }
                        else if (((int) pclass) == 2)
                        {
                            fare = stats.MeanFare2.Calculate();
                        }
                        else if (((int) pclass) == 3)
                        {
                            fare = stats.MeanFare3.Calculate();
                        }
                        else
                        {
                            // should not happen, we would have a class other than 1,2,3.
                            // however, if that DID happen, use the median class (2).
                            fare = stats.MeanFare2.Calculate();
                        }
                    }
                    else
                    {
                        fare = Double.Parse(reader[indexFare], CultureInfo.InvariantCulture);
                    }
                    data.Input[5] = RangeNormalize(fare, 0, 500, inputLow, inputHigh);

                    // embarked-c
                    data.Input[6] = string.Compare(embarked.Trim(), "c", true) == 0 ? inputHigh : inputLow;

                    // embarked-q
                    data.Input[7] = string.Compare(embarked.Trim(), "q", true) == 0 ? inputHigh : inputLow;

                    // embarked-s
                    data.Input[8] = string.Compare(embarked.Trim(), "s", true) == 0 ? inputHigh : inputLow;

                    // name-mil
                    data.Input[9] = (name.Contains("Col.") || name.Contains("Capt.") || name.Contains("Major."))
                        ? inputHigh
                        : inputLow;

                    // name-nobility
                    data.Input[10] = (name.Contains("Countess.") || name.Contains("Lady.") || name.Contains("Sir.") ||
                                      name.Contains("Don.") || name.Contains("Dona.") || name.Contains("Jonkheer."))
                        ? inputHigh
                        : inputLow;

                    // name-dr
                    data.Input[11] = (name.Contains("Dr.")) ? inputHigh : inputLow;


                    // name-clergy
                    data.Input[12] = (name.Contains("Rev.")) ? inputHigh : inputLow;

                    // add the new row
                    result.Add(data);

                    // add survived, if it exists
                    if (survivedIndex != -1)
                    {
                        int survived = int.Parse(reader[survivedIndex]);
                        data.Ideal[0] = (survived == 1) ? predictSurvive : predictPerish;
                    }
                } while (reader.Read());
            }

            return result;
        }

        /// <summary>
        ///     The entry point for this example.  If you would like to make this example
        ///     stand alone, then add to its own project and rename to Main.
        /// </summary>
        /// <param name="args">Not used.</param>
        public static void ExampleMain(string[] args)
        {
            string filename = "";

            if (args.Length >0 )
            {
                filename = args[0];
                string dataPath = filename;
                string trainingPath = Path.Combine(dataPath, TitanicConfig.TrainingFilename);
                string testPath = Path.Combine(dataPath, TitanicConfig.TestFilename);
                string normalizePath = Path.Combine(dataPath, TitanicConfig.NormDumpFilename);

                var stats = new TitanicStats();
                Analyze(stats, trainingPath);
                Analyze(stats, testPath);
                stats.Dump();

                var ids = new List<String>();
                IList<BasicData> training = Normalize(stats, trainingPath, ids,
                    TitanicConfig.InputNormalizeLow,
                    TitanicConfig.InputNormalizeHigh,
                    TitanicConfig.PredictSurvive,
                    TitanicConfig.PredictPerish);

                using (var streamWriter = new StreamWriter(normalizePath))
                using (var writer = new CsvWriter(streamWriter))
                {
                    writer.WriteField("id");
                    writer.WriteField("age");
                    writer.WriteField("sex-male");
                    writer.WriteField("pclass");
                    writer.WriteField("sibsp");
                    writer.WriteField("parch");
                    writer.WriteField("fare");
                    writer.WriteField("embarked-c");
                    writer.WriteField("embarked-q");
                    writer.WriteField("embarked-s");
                    writer.WriteField("name-mil");
                    writer.WriteField("name-nobility");
                    writer.WriteField("name-dr");
                    writer.WriteField("name-clergy");
                    writer.NextRecord();

                    int idx = 0;
                    foreach (BasicData data in training)
                    {
                        writer.WriteField(ids[idx++]);
                        writer.WriteField(data.Input[1].ToString(CultureInfo.InvariantCulture));
                        writer.WriteField(data.Input[2].ToString(CultureInfo.InvariantCulture));
                        writer.WriteField(data.Input[3].ToString(CultureInfo.InvariantCulture));
                        writer.WriteField(data.Input[4].ToString(CultureInfo.InvariantCulture));
                        writer.WriteField(data.Input[5].ToString(CultureInfo.InvariantCulture));
                        writer.WriteField(data.Input[6].ToString(CultureInfo.InvariantCulture));
                        writer.WriteField(data.Input[7].ToString(CultureInfo.InvariantCulture));
                        writer.WriteField(data.Input[8].ToString(CultureInfo.InvariantCulture));
                        writer.WriteField(data.Input[9].ToString(CultureInfo.InvariantCulture));
                        writer.WriteField(data.Input[10].ToString(CultureInfo.InvariantCulture));
                        writer.WriteField(data.Input[11].ToString(CultureInfo.InvariantCulture));
                        writer.WriteField(data.Input[12].ToString(CultureInfo.InvariantCulture));
                        writer.WriteField(data.Input[0].ToString(CultureInfo.InvariantCulture));
                        writer.NextRecord();
                    }
                }
            }
            else
            {
                Console.WriteLine("Please provide your data directory path as the first argument.");
            }
        }
    }
}
