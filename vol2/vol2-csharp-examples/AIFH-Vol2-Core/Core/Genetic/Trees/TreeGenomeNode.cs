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
