using AIFH_Vol3.Core.Randomize;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.Selection
{
    /// <summary>
    /// Random model selection.
    /// </summary>
    public class RandomModelSelection : ModelSelection
    {
        private IGenerateRandom _rnd = new MersenneTwisterGenerateRandom();

        public override Object[] Next()
        {
            Object[] result = new Object[SearchAxises.Count];

            for (int i = 0; i < result.Length; i++)
            {
                result[i] = SearchAxises[i].Sample(_rnd);
            }

            return result;
        }

        public IGenerateRandom Random
        {
            get
            {
                return _rnd;
            }
            set
            {
                _rnd = value;
            }
        }
    }
}
