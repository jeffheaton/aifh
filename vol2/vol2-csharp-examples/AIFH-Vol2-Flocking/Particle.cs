namespace AIFH_Vol2_Flocking
{
    /// <summary>
    ///     A particle.
    /// </summary>
    public class Particle
    {
        /// <summary>
        ///     The location.
        /// </summary>
        private readonly double[] _location;

        /// <summary>
        ///     The velocity.
        /// </summary>
        private readonly double[] _velocity;

        /// <summary>
        ///     The constructor.
        /// </summary>
        /// <param name="dimensions">The dimensions.</param>
        public Particle(int dimensions)
        {
            _location = new double[dimensions];
            _velocity = new double[dimensions];
        }

        /// <summary>
        ///     The location vector.
        /// </summary>
        public double[] Location
        {
            get { return _location; }
        }

        /// <summary>
        ///     The velocity vector.
        /// </summary>
        public double[] Velocity
        {
            get { return _velocity; }
        }
    }
}