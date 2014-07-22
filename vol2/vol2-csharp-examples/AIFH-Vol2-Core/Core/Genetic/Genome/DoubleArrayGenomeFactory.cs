using AIFH_Vol2.Core.Evolutionary.Genome;

namespace AIFH_Vol2.Core.Genetic.Genome
{
    /// <summary>
    ///     A factory that creates DoubleArrayGenome objects of a specific size.
    /// </summary>
    public class DoubleArrayGenomeFactory : IGenomeFactory
    {
        /// <summary>
        ///     The size to create.
        /// </summary>
        private readonly int _size;

        /// <summary>
        ///     Construct the genome factory.
        /// </summary>
        /// <param name="theSize">The size to create genomes of.</param>
        public DoubleArrayGenomeFactory(int theSize)
        {
            _size = theSize;
        }

        /// <inheritdoc />
        public IGenome Factor()
        {
            return new DoubleArrayGenome(_size);
        }

        /// <inheritdoc />
        public IGenome Factor(IGenome other)
        {
            // TODO Auto-generated method stub
            return new DoubleArrayGenome((DoubleArrayGenome) other);
        }
    }
}