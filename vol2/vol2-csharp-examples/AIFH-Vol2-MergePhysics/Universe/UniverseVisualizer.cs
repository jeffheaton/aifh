using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

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
        public void Visualize(Rectangle[][] grid)
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
                    double r = (_universe.Get(row, col).Data[0] + 1.0) / 2.0;
                    double g = (_universe.Get(row, col).Data[1] + 1.0) / 2.0;
                    double b = (_universe.Get(row, col).Data[2] + 1.0) / 2.0;

                    Brush brush = new SolidColorBrush(Color.FromArgb(255, 
                        (byte)(r * 255.0), (byte)(g * 255.0), (byte)(b * 255.0)));

                    grid[row][col].Fill = brush;                    
                }
            }            
        }
    }
}