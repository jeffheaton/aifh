using AIFH_Vol2.Core.Evolutionary.Genome;

namespace AIFH_Vol2.Core.Genetic.Genome
{
    /// <summary>
    /// An array genome represents an array of "something", this allows array
    /// operators such as crossover and mutate to work on the genome.
    /// </summary>
    public interface IArrayGenome : IGenome
    {
        /// <summary>
        /// Copy elements from another array genome into this one.
        /// </summary>
        /// <param name="source">The source genome.</param>
        /// <param name="sourceIndex">The source index.</param>
        /// <param name="targetIndex">The target index.</param>
        void Copy(IArrayGenome source, int sourceIndex, int targetIndex);
        
        /// <summary>
        /// Swap two elements in this genome. 
        /// </summary>
        /// <param name="iswap1">The first element index to swap.</param>
        /// <param name="iswap2">The second element index to swap.</param>
        void Swap(int iswap1, int iswap2);
    }
}
