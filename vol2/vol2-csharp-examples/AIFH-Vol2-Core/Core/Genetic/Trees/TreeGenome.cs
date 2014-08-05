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
using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Learning;

namespace AIFH_Vol2.Core.Genetic.Trees
{
    /// <summary>
    ///     A tree genome, used for Genetic Programming.
    /// </summary>
    public class TreeGenome : BasicGenome, IRegressionAlgorithm
    {
        private readonly EvaluateTree _evaluator;

        /// <summary>
        ///     Constructor.
        /// </summary>
        /// <param name="theEvaluator">The evaluator.</param>
        public TreeGenome(EvaluateTree theEvaluator)
        {
            _evaluator = theEvaluator;
        }

        public TreeGenomeNode Root { get; set; }

        /// <inheritdoc />
        public override int Count
        {
            get { return Root.Count; }
        }

        public EvaluateTree Evaluator
        {
            get { return _evaluator; }
        }

        /// <inheritdoc />
        public override double[] LongTermMemory
        {
            get { throw new AIFHError("Long term memory not supported, use a genetic trainer."); }
        }

        /// <inheritdoc />
        public double[] ComputeRegression(double[] input)
        {
            var result = new double[1];
            result[0] = _evaluator.Evaluate(Root, input);
            return result;
        }

        /// <inheritdoc />
        public override void Copy(IGenome source)
        {
            Root = ((TreeGenome) source).Root.Copy();
        }
    }
}
