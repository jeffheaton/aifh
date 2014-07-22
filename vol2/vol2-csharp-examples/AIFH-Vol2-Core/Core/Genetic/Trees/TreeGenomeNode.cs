using System.Collections.Generic;
using System.Linq;

namespace AIFH_Vol2.Core.Genetic.Trees
{
    /// <summary>
    ///     A tree genome, used for Genetic Programming.
    /// </summary>
    public class TreeGenomeNode
    {
        /// <summary>
        ///     The child nodes.
        /// </summary>
        private readonly IList<TreeGenomeNode> _children = new List<TreeGenomeNode>();

        /// <summary>
        ///     The opcode for this node.
        /// </summary>
        private readonly int _opcode;

        /// <summary>
        ///     Constructor.
        /// </summary>
        /// <param name="theOpcode">The opcode for this node.</param>
        public TreeGenomeNode(int theOpcode)
        {
            _opcode = theOpcode;
        }

        /// <summary>
        ///     The opcode for this node.
        /// </summary>
        public int Opcode
        {
            get { return _opcode; }
        }

        /// <summary>
        ///     The children from this node.
        /// </summary>
        public IList<TreeGenomeNode> Children
        {
            get { return _children; }
        }

        /// <summary>
        ///     The size of this node.
        /// </summary>
        public int Count
        {
            get
            {
                return 1 + _children.Sum(child => child.Count);
            }
        }

        /// <summary>
        ///     Create a copy of this node.
        /// </summary>
        /// <returns>A copy (clone) of this node.</returns>
        public TreeGenomeNode Copy()
        {
            var result = new TreeGenomeNode(_opcode);
            foreach (TreeGenomeNode child in _children)
            {
                result.Children.Add(child.Copy());
            }
            return result;
        }
    }
}