

using AIFH_Vol2.Core;

namespace AIFH_Vol2_Capstone_Plants.Milestone1
{
    /// <summary>
    ///     An individual cell in the plant universe.
    /// </summary>
    public class PlantUniverseCell
    {
        /// <summary>
        ///     How green (leaf) or brown (trunk) is the cell.  1.0 is fully leaf, 0.0 is fully trunk.
        /// </summary>
        public double Leafyness { get; set; }

        /// <summary>
        ///     The amount of energy between [0,1].
        /// </summary>
        public double Energy { get; set; }

        /// <summary>
        ///     The amount of nourishment between [0,1].
        /// </summary>
        public double Nourishment { get; set; }

        /// <summary>
        ///     The calculated sunlight exposure.
        /// </summary>
        public double CalculatedSunlight { get; set; }

        /// <summary>
        ///     The calculated water exposure.
        /// </summary>
        public double CalculatedWater { get; set; }

        /// <summary>
        ///     True, if this cell is alive.
        /// </summary>
        public bool IsAlive
        {
            get { return Energy > AIFH.DefaultPrecision; }
        }
    }
}