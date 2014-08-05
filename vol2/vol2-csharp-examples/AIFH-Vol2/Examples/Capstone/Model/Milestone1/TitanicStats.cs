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

namespace AIFH_Vol2.Examples.Capstone.Model.Milestone1
{
    /// <summary>
    ///     Stats that were collected about the titanic passengers to help with normalization, interpolation
    ///     and modeling.
    /// </summary>
    public class TitanicStats
    {
        /// <summary>
        ///     Survival stats for passengers that embarked from Cherbourg, France.
        /// </summary>
        private readonly CalcSurvival _embarkedC = new CalcSurvival();

        /// <summary>
        ///     Histogram of embark locations.
        /// </summary>
        private readonly CalcHistogram _embarkedHisto = new CalcHistogram();

        /// <summary>
        ///     Survival stats for passengers that embarked from Queenstown, England.
        /// </summary>
        private readonly CalcSurvival _embarkedQ = new CalcSurvival();

        /// <summary>
        ///     Survival stats for passengers that embarked from Southampton, England.
        /// </summary>
        private readonly CalcSurvival _embarkedS = new CalcSurvival();

        /// <summary>
        ///     Passengers with the title "rev".
        /// </summary>
        private readonly CalcMean _meanClergy = new CalcMean();

        /// <summary>
        ///     Passengers with the title "dr".
        /// </summary>
        private readonly CalcMean _meanDr = new CalcMean();

        /// <summary>
        ///     Passengers in 1st class, average fare.
        /// </summary>
        private readonly CalcMean _meanFare1 = new CalcMean();

        /// <summary>
        ///     Passengers in 2st class, average fare.
        /// </summary>
        private readonly CalcMean _meanFare2 = new CalcMean();

        /// <summary>
        ///     Passengers in 3rd class, average fare.
        /// </summary>
        private readonly CalcMean _meanFare3 = new CalcMean();

        /// <summary>
        ///     Total female passengers.
        /// </summary>
        private readonly CalcMean _meanFemale = new CalcMean();

        /// <summary>
        ///     Total male passengers.
        /// </summary>
        private readonly CalcMean _meanMale = new CalcMean();

        /// <summary>
        ///     Passengers with the title "master", mean age.
        /// </summary>
        private readonly CalcMean _meanMaster = new CalcMean();

        /// <summary>
        ///     Passengers with a military title, mean age.
        /// </summary>
        private readonly CalcMean _meanMilitary = new CalcMean();

        /// <summary>
        ///     Passengers with the title "miss", mean age.
        /// </summary>
        private readonly CalcMean _meanMiss = new CalcMean();

        /// <summary>
        ///     Passengers with the title "mr", mean age.
        /// </summary>
        private readonly CalcMean _meanMr = new CalcMean();

        /// <summary>
        ///     Passengers with the title "mrs", mean age.
        /// </summary>
        private readonly CalcMean _meanMrs = new CalcMean();

        /// <summary>
        ///     Passengers with a nobility title, mean age.
        /// </summary>
        private readonly CalcMean _meanNobility = new CalcMean();

        /// <summary>
        ///     Total passengers.
        /// </summary>
        private readonly CalcMean _meanTotal = new CalcMean();

        /// <summary>
        ///     Survival stats for passengers with a title of "rev".
        /// </summary>
        private readonly CalcSurvival _survivalClergy = new CalcSurvival();

        /// <summary>
        ///     Survival stats for passengers with a title of "dr".
        /// </summary>
        private readonly CalcSurvival _survivalDr = new CalcSurvival();

        /// <summary>
        ///     Survival stats for passengers with a title of "master".
        /// </summary>
        private readonly CalcSurvival _survivalMaster = new CalcSurvival();

        /// <summary>
        ///     Survival stats for passengers with a military title.
        /// </summary>
        private readonly CalcSurvival _survivalMilitary = new CalcSurvival();

        /// <summary>
        ///     Survival stats for passengers with a title of "miss".
        /// </summary>
        private readonly CalcSurvival _survivalMiss = new CalcSurvival();

        /// <summary>
        ///     Survival stats for passengers with a title of "mr".
        /// </summary>
        private readonly CalcSurvival _survivalMr = new CalcSurvival();

        /// <summary>
        ///     Survival stats for passengers with a title of "mrs".
        /// </summary>
        private readonly CalcSurvival _survivalMrs = new CalcSurvival();

        /// <summary>
        ///     Survival stats for passengers with a nobility title.
        /// </summary>
        private readonly CalcSurvival _survivalNobility = new CalcSurvival();

        /// <summary>
        ///     Survival stats for all passengers.
        /// </summary>
        private readonly CalcSurvival _survivalTotal = new CalcSurvival();

        /// <summary>
        ///     Passengers with the title "master", mean age.
        /// </summary>
        public CalcMean MeanMaster
        {
            get { return _meanMaster; }
        }

        /// <summary>
        ///     Passengers with the title "mr", mean age.
        /// </summary>
        public CalcMean MeanMr
        {
            get { return _meanMr; }
        }

        /// <summary>
        ///     Passengers with the title "miss", mean age.
        /// </summary>
        public CalcMean MeanMiss
        {
            get { return _meanMiss; }
        }

        /// <summary>
        ///     Passengers with the title "mrs", mean age.
        /// </summary>
        public CalcMean MeanMrs
        {
            get { return _meanMrs; }
        }

        /// <summary>
        ///     Passengers with a military title, mean age.
        /// </summary>
        public CalcMean MeanMilitary
        {
            get { return _meanMilitary; }
        }

        /// <summary>
        ///     Passengers with a noble title, mean age.
        /// </summary>
        public CalcMean MeanNobility
        {
            get { return _meanNobility; }
        }

        /// <summary>
        ///     Passengers with the title "dr", mean age.
        /// </summary>
        public CalcMean MeanDr
        {
            get { return _meanDr; }
        }

        /// <summary>
        ///     Passengers with the title "rev", mean age.
        /// </summary>
        public CalcMean MeanClergy
        {
            get { return _meanClergy; }
        }

        /// <summary>
        ///     Mean age for all passengers.
        /// </summary>
        public CalcMean MeanTotal
        {
            get { return _meanTotal; }
        }

        /// <summary>
        ///     Survival stats for passengers with a title of "master".
        /// </summary>
        public CalcSurvival SurvivalMaster
        {
            get { return _survivalMaster; }
        }

        /// <summary>
        ///     Survival stats for passengers with a title of "mr".
        /// </summary>
        public CalcSurvival SurvivalMr
        {
            get { return _survivalMr; }
        }

        /// <summary>
        ///     Survival stats for passengers with a title of "miss".
        /// </summary>
        public CalcSurvival SurvivalMiss
        {
            get { return _survivalMiss; }
        }

        /// <summary>
        ///     Survival stats for passengers with a title of "mrs".
        /// </summary>
        public CalcSurvival SurvivalMrs
        {
            get { return _survivalMrs; }
        }

        /// <summary>
        ///     Survival stats for passengers with a military title.
        /// </summary>
        public CalcSurvival SurvivalMilitary
        {
            get { return _survivalMilitary; }
        }

        /// <summary>
        ///     Survival stats for passengers with a noble title.
        /// </summary>
        public CalcSurvival SurvivalNobility
        {
            get { return _survivalNobility; }
        }

        /// <summary>
        ///     Survival stats for passengers with a title of "dr".
        /// </summary>
        public CalcSurvival SurvivalDr
        {
            get { return _survivalDr; }
        }

        /// <summary>
        ///     Survival stats for passengers with a title of "clergy".
        /// </summary>
        public CalcSurvival SurvivalClergy
        {
            get { return _survivalClergy; }
        }

        /// <summary>
        ///     Survival stats on the total number of passengers.
        /// </summary>
        public CalcSurvival SurvivalTotal
        {
            get { return _survivalTotal; }
        }

        /// <summary>
        ///     Survival stats for passengers that embarked from Southampton, England.
        /// </summary>
        public CalcSurvival EmbarkedS
        {
            get { return _embarkedS; }
        }

        /// <summary>
        ///     Survival stats for passengers that embarked from Cherbourg, France.
        /// </summary>
        public CalcSurvival EmbarkedC
        {
            get { return _embarkedC; }
        }

        /// <summary>
        ///     Survival stats for passengers that embarked from Queenstown, England.
        /// </summary>
        public CalcSurvival EmbarkedQ
        {
            get { return _embarkedQ; }
        }

        /// <summary>
        ///     Histogram of embark locations.
        /// </summary>
        public CalcHistogram EmbarkedHisto
        {
            get { return _embarkedHisto; }
        }

        /// <summary>
        ///     Mean age for male passengers.
        /// </summary>
        public CalcMean MeanMale
        {
            get { return _meanMale; }
        }

        /// <summary>
        ///     Mean age for female passengers.
        /// </summary>
        public CalcMean MeanFemale
        {
            get { return _meanFemale; }
        }

        /// <summary>
        ///     Mean fare for first class.
        /// </summary>
        public CalcMean MeanFare1
        {
            get { return _meanFare1; }
        }

        /// <summary>
        ///     Mean fare for second class.
        /// </summary>
        public CalcMean MeanFare2
        {
            get { return _meanFare2; }
        }

        /// <summary>
        ///     Mean fare for second class.
        /// </summary>
        public CalcMean MeanFare3
        {
            get { return _meanFare3; }
        }

        /// <summary>
        ///     Dump all stats to stdout.
        /// </summary>
        public void Dump()
        {
            Console.WriteLine("Mean Master: Mean Age: " + _meanMaster.Calculate() + " " + _survivalMaster);
            Console.WriteLine("Mr.: Mean Age: " + _meanMr.Calculate() + " " + _survivalMr);
            Console.WriteLine("Miss.: Mean Age: " + _meanMiss.Calculate() + " " + _survivalMiss);
            Console.WriteLine("Mrs.: Mean Age: " + _meanMrs.Calculate() + " " + _survivalMrs);
            Console.WriteLine("Military: Mean Age: " + _meanMrs.Calculate() + " " + _survivalMilitary);
            Console.WriteLine("Clergy: Mean Age: " + _meanClergy.Calculate() + " " + _survivalClergy);
            Console.WriteLine("Nobility: Mean Age: " + _meanNobility.Calculate() + " " + _survivalNobility);
            Console.WriteLine("Dr: Mean Age: " + _meanDr.Calculate() + " " + _survivalDr);
            Console.WriteLine("Total known survival: Mean Age: " + _meanTotal.Calculate() + " " + _survivalTotal);
            Console.WriteLine();
            Console.WriteLine("Embarked Queenstown: Mean Age: " + _embarkedQ);
            Console.WriteLine("Embarked Southampton: Mean Age: " + _embarkedS);
            Console.WriteLine("Embarked Cherbourg: Mean Age: " + _embarkedC);
            Console.WriteLine("Most common embarked: Mean Age: " + _embarkedHisto.Max());
            Console.WriteLine();
            Console.WriteLine("Mean Age Male: " + _meanMale.Calculate());
            Console.WriteLine("Mean Age Female: " + _meanFemale.Calculate());
            Console.WriteLine();
            Console.WriteLine("Mean Fair 1st Class: " + _meanFare1.Calculate());
            Console.WriteLine("Mean Fair 2st Class: " + _meanFare2.Calculate());
            Console.WriteLine("Mean Fair 3st Class: " + _meanFare3.Calculate());
        }
    }
}
