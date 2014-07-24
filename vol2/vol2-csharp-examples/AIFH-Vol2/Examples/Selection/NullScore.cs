using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AIFH_Vol2.Core.Learning;
using AIFH_Vol2.Core.Learning.Score;

namespace AIFH_Vol2.Examples.Selection
{
    public class NullScore: IScoreFunction
    {
        public double CalculateScore(IMLMethod algo)
        {
            return 0;
        }

        public bool ShouldMinimize { get; private set; }
    }
}
