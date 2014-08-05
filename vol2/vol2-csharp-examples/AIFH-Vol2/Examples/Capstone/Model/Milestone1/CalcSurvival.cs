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
using System.Text;

namespace AIFH_Vol2.Examples.Capstone.Model.Milestone1
{
    /// <summary>
    ///     Used to calculate survival by male and female.
    /// </summary>
    public class CalcSurvival
    {
        /// <summary>
        ///     The count of females.
        /// </summary>
        private int _countFemale;

        /// <summary>
        ///     The count of males.
        /// </summary>
        private int _countMale;

        /// <summary>
        ///     The count of female survivors.
        /// </summary>
        private int _femaleSurvive;

        /// <summary>
        ///     The count of male survivors.
        /// </summary>
        private int _maleSurvive;

        /// <summary>
        ///     The number of male survivors.
        /// </summary>
        public int MaleSurvive
        {
            get { return _maleSurvive; }
        }

        /// <summary>
        ///     The number of female survivors.
        /// </summary>
        public int FemaleSurvive
        {
            get { return _femaleSurvive; }
        }

        /// <summary>
        ///     The total count of passengers.
        /// </summary>
        public int Count
        {
            get { return _countFemale + _countMale; }
        }

        /// <summary>
        ///     The number of male passengers.
        /// </summary>
        public int CountMale
        {
            get { return _countMale; }
        }

        /// <summary>
        ///     The number of female passengers.
        /// </summary>
        public int CountFemale
        {
            get { return _countFemale; }
        }

        /// <summary>
        ///     Update for a passenger.
        /// </summary>
        /// <param name="male">True, if passenger was male.</param>
        /// <param name="survived">True, if passenger survived.</param>
        public void Update(bool male, bool survived)
        {
            if (male)
            {
                _countMale++;
            }
            else
            {
                _countFemale++;
            }
            if (survived)
            {
                if (male)
                {
                    _maleSurvive++;
                }
                else
                {
                    _femaleSurvive++;
                }
            }
        }

        /// <inheritdoc />
        public override String ToString()
        {
            var result = new StringBuilder();
            result.Append("(");

            result.Append("Count: ");
            result.Append(Count);

            if (Count > 0)
            {
                double pct = (double) (_femaleSurvive + _maleSurvive)/Count;
                result.Append(", survived: ");
                result.Append(pct);
            }

            if (CountMale > 0)
            {
                double pct = (double) (_maleSurvive)/(_countMale);
                result.Append(", male.survived: ");
                result.Append(pct);
            }

            if (CountFemale > 0)
            {
                double pct = (double) (_femaleSurvive)/(_countFemale);
                result.Append(", female.survived: ");
                result.Append(pct);
            }

            result.Append(")");
            return result.ToString();
        }
    }
}
