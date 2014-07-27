namespace AIFH_Vol2_MergePhysics.Universe
{
    /// <summary>
    ///     UniverseVisualizer
    /// </summary>
    public class UniverseVisualizer
    {
        /// <summary>
        ///     The universe.
        /// </summary>
        private readonly UniverseHolder _universe;

        /// <summary>
        ///     The zoom.
        /// </summary>
        private readonly int _zoom;

        /// <summary>
        ///     The constructor.
        /// </summary>
        /// <param name="theUniverse">The universe.</param>
        /// <param name="theZoom">The zoom factor.</param>
        public UniverseVisualizer(UniverseHolder theUniverse, int theZoom)
        {
            _universe = theUniverse;
            int width = _universe.Width;
            int height = _universe.Height;

            _zoom = theZoom;
        }


        /// <summary>
        ///     The universe rendered to an image.
        /// </summary>
        public void Visualize()
        {
            int width = _universe.Width;
            int height = _universe.Height;
            int imageSize = width*height;

            var pixels = new int[imageSize*_zoom*_zoom*3];
            int rowSize = width*3*_zoom;

            for (int row = 0; row < height; row++)
            {
                for (int col = 0; col < width; col++)
                {
                    for (int i = 0; i < 3; i++)
                    {
                        double d = (_universe.Get(row, col).Data[i] + 1.0)/2.0;
                        for (int y = 0; y < _zoom; y++)
                        {
                            for (int x = 0; x < _zoom; x++)
                            {
                                int idx = (row*_zoom + y)*rowSize
                                          + (col*_zoom + x)*3;
                                pixels[idx + i] = (int) (d*255.0);
                            }
                        }
                    }
                }
            }
        }
    }
}