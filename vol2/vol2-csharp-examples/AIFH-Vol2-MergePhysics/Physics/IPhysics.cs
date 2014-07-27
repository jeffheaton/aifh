using System;
using AIFH_Vol2_MergePhysics.Universe;

namespace AIFH_Vol2_MergePhysics.Physics
{
    /// <summary>
    ///     Defines a set of "physics" rules to control a cellular automation.
    /// </summary>
    public interface IPhysics
    {
        /// <summary>
        ///     Get the physical constants vector.
        /// </summary>
        double[] Data { get; }

        /// <summary>
        ///     Copy the physics constants vector from another array.
        /// </summary>
        /// <param name="sourceData">The source vector to copy.</param>
        void CopyData(double[] sourceData);

        /// <summary>
        ///     Load the physical constants vector from a text file.
        /// </summary>
        /// <param name="filename">The filename.</param>
        void Load(String filename);

        /// <summary>
        ///     Save the physical constants vector to a text file.
        /// </summary>
        /// <param name="filename">The filename.</param>
        void Save(String filename);


        /// <summary>
        ///     Perform the actual physics.
        /// </summary>
        /// <param name="outputUniverse">The new output universe.</param>
        /// <param name="row">The row of the cell we are processing.</param>
        /// <param name="col">The column of the cell we are processing.</param>
        void ProcessPixel(UniverseHolder outputUniverse, int row, int col);

        /// <summary>
        ///     Randomize the physics to random values.
        /// </summary>
        void Randomize();
    }
}