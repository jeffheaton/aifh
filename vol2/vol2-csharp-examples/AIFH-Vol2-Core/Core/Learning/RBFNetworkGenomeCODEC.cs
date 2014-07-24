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