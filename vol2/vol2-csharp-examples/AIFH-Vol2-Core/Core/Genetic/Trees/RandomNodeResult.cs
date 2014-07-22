namespace AIFH_Vol2.Core.Genetic.Trees
{
    /// <summary>
    ///     The result of sampling a random node.
    /// </summary>
    public class RandomNodeResult
    {
        /// <summary>
        ///     The parent.
        /// </summary>
        public TreeGenomeNode Parent { get; set; }

        /// <summary>
        ///     The child that was randomly selected.
        /// </summary>
        public TreeGenomeNode Child { get; set; }
    }
}