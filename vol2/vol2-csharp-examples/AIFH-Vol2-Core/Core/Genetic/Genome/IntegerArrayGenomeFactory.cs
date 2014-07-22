using AIFH_Vol2.Core.Evolutionary.Genome;

namespace AIFH_Vol2.Core.Genetic.Genome
{
    /// <summary>
    ///     A factory to create integer genomes of a specific size.
    /// </summary>
    public class IntegerArrayGenomeFactory : IGenomeFactory
    {
        /// <summary>
        ///     The size of genome to create.
        /// </summary>
        private readonly int _size;

        /// <summary>
        ///     Create the integer genome of a fixed size.
        /// </summary>
        /// <param name="theSize">The size to use.</param>
        public IntegerArrayGenomeFactory(int theSize)
        {
            _size = theSize;
        }

        /// <inheritdoc />
        public IGenome Factor()
        {
            return new IntegerArrayGenome(_size);
        }

        /// <inheritdoc />
        public IGenome Factor(IGenome other)
        {
            return new IntegerArrayGenome(((IntegerArrayGenome) other));
        }
    }
}