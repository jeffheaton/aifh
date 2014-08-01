using System.Windows;
using System.Windows.Media.Imaging;

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
        public void Visualize(WriteableBitmap bitmap)
        {
            int width = _universe.Width;
            int height = _universe.Height;

            // I am usually against unsafe code, but this seems to be the
            // only way in XAML to render bit-by-bit and get any degree of 
            // speed.  If anyone knows a better way (that is still reasonably fast), 
            // let me know. --JHeaton.
            bitmap.Lock();
            unsafe
            {
                for (int row = 0; row < height; row++)
                {
                    for (int col = 0; col < width; col++)
                    {
                        double dr = (_universe.Get(row, col).Data[0] + 1.0) / 2.0;
                        double dg = (_universe.Get(row, col).Data[1] + 1.0) / 2.0;
                        double db = (_universe.Get(row, col).Data[2] + 1.0) / 2.0;

                        // Compute the pixel's color. 
                        int color_data = ((byte)(dr * 255.0)) << 16; // R
                        color_data |= ((byte)(dg * 255.0)) << 8;   // G
                        color_data |= ((byte)(db * 255.0)) << 0;   // B



                        for (int y = 0; y < _zoom; y++)
                        {
                            for (int x = 0; x < _zoom; x++)
                            {

                                // Get a pointer to the back buffer. 
                                int pBackBuffer = (int)bitmap.BackBuffer;

                                // Find the address of the pixel to draw.
                                pBackBuffer += (row * _zoom + y)
                                    * bitmap.BackBufferStride
                                    + (col * _zoom + x) * 4;
                                // Assign the color data to the pixel.
                                *((int*)pBackBuffer) = color_data;
                            }
                        }

                    }
                }
            }

            // Specify the area of the bitmap that changed.
            bitmap.AddDirtyRect(new Int32Rect(0, 0, width*_zoom, height*_zoom));
            bitmap.Unlock();
        }
    }
}