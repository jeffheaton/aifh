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
using AIFH_Vol2.Core.Evolutionary.CODEC;
using AIFH_Vol2.Core.Evolutionary.Genome;
using AIFH_Vol2.Core.Genetic.Genome;

namespace AIFH_Vol2.Core.Learning
{
    /// <summary>
    ///     A CODEC to encode/decode RBF networks.
    /// </summary>
    public class RBFNetworkGenomeCODEC : IGeneticCODEC
    {
        private readonly int _inputCount;

        private readonly int _outputCount;
        private readonly int _rbfCount;
        private readonly int _size;

        public RBFNetworkGenomeCODEC(int inputCount, int rbfCount, int outputCount)
        {
            _inputCount = inputCount;
            _rbfCount = rbfCount;
            _outputCount = outputCount;
            var temp = new RBFNetwork(inputCount, rbfCount, outputCount);
            _size = temp.LongTermMemory.Length;
        }

        public int InputCount
        {
            get { return _inputCount; }
        }

        public int OutputCount
        {
            get { return _outputCount; }
        }

        public int RbfCount
        {
            get { return _rbfCount; }
        }

        public int Size
        {
            get { return _size; }
        }

        public IMLMethod Decode(IGenome genome)
        {
            var result = new RBFNetwork(_inputCount, _rbfCount, _outputCount);
            var dag = (DoubleArrayGenome) genome;
            Array.Copy(dag.Data, 0, result.LongTermMemory, 0, _size);
            return result;
        }


        public IGenome Encode(IMLMethod phenotype)
        {
            var rbfNet = (RBFNetwork) phenotype;
            var result = new DoubleArrayGenome(Size);
            Array.Copy(rbfNet.LongTermMemory, 0, result.Data, 0, _size);
            return result;
        }
    }
}
