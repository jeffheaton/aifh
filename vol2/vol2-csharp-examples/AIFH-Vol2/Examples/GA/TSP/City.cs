using System;

namespace AIFH_Vol2.Examples.GA.TSP
{
    /// <summary>
    ///     City: Holds the location of a city for the traveling salesman problem.
    /// </summary>
    public class City
    {
        /// <summary>
        ///     The city's x position.
        /// </summary>
        private readonly int _xpos;

        /// <summary>
        ///     The city's y position.
        /// </summary>
        private readonly int _ypos;

        /// <summary>
        ///     Constructor.
        /// </summary>
        /// <param name="x">The city's x position.</param>
        /// <param name="y">The city's y position.</param>
        public City(int x, int y)
        {
            _xpos = x;
            _ypos = y;
        }

        /// <summary>
        ///     Return the city's x position.
        /// </summary>
        private int X
        {
            get { return _xpos; }
        }

        /// <summary>
        ///     Returns the city's y position.
        /// </summary>
        private int Y
        {
            get { return _ypos; }
        }

        /// <summary>
        ///     Returns how close the city is to another city.
        /// </summary>
        /// <param name="cother">The other city.</param>
        /// <returns>A distance.</returns>
        public int Proximity(City cother)
        {
            return Proximity(cother.X, cother.Y);
        }

        /// <summary>
        ///     Returns how far this city is from a a specific point. This method uses
        ///     the pythagorean theorem to calculate the distance.
        /// </summary>
        /// <param name="x">The x coordinate</param>
        /// <param name="y">The y coordinate</param>
        /// <returns>The distance.</returns>
        private int Proximity(int x, int y)
        {
            int xdiff = _xpos - x;
            int ydiff = _ypos - y;
            return (int) Math.Sqrt(xdiff*xdiff + ydiff*ydiff);
        }
    }
}