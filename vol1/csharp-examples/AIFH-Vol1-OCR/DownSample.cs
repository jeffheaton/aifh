using System.Drawing;

namespace AIFH_Vol1_OCR
{
    public class DownSample
    {
        private readonly Bitmap image;
        private int _downSampleBottom;
        private int _downSampleLeft;
        private int _downSampleRight;
        private int _downSampleTop;
        private double _ratioX;
        private double _ratioY;

        public DownSample(Bitmap image)
        {
            this.image = image;
        }

        public double[] PerformDownSample(int downSampleWidth, int downSampleHeight)
        {
            int size = downSampleWidth*downSampleHeight;
            var result = new double[size];

            FindBounds();

            // now downsample

            _ratioX = (_downSampleRight - _downSampleLeft)
                      /(double) downSampleWidth;
            _ratioY = (_downSampleBottom - _downSampleTop)
                      /(double) downSampleHeight;

            int index = 0;
            for (int y = 0; y < downSampleHeight; y++)
            {
                for (int x = 0; x < downSampleWidth; x++)
                {
                    result[index++] = DownSampleRegion(x, y);
                }
            }

            return result;
        }


        /// <summary>
        ///     Called to downsample a quadrant of the image.
        /// </summary>
        /// <param name="x">The x coordinate of the resulting downsample.</param>
        /// <param name="y">The y coordinate of the resulting downsample.</param>
        /// <returns>Returns true if there were ANY pixels in the specified quadrant.</returns>
        protected double DownSampleRegion(int x, int y)
        {
            var startX = (int) (_downSampleLeft + (x*_ratioX));
            var startY = (int) (_downSampleTop + (y*_ratioY));
            var endX = (int) (startX + _ratioX);
            var endY = (int) (startY + _ratioY);

            for (int yy = startY; yy <= endY; yy++)
            {
                for (int xx = startX; xx <= endX; xx++)
                {
                    Color pixel = image.GetPixel(xx, yy);
                    if (IsBlack(pixel))
                    {
                        return 1;
                    }
                }
            }

            return 0;
        }

        /// <summary>
        ///     This method is called to automatically crop the image so that whitespace
        ///     is removed.
        /// </summary>
        protected void FindBounds()
        {
            int h = image.Height;
            int w = image.Width;

            // top line
            for (int y = 0; y < h; y++)
            {
                if (!HLineClear(y))
                {
                    _downSampleTop = y;
                    break;
                }
            }
            // bottom line
            for (int y = h - 1; y >= 0; y--)
            {
                if (!HLineClear(y))
                {
                    _downSampleBottom = y;
                    break;
                }
            }
            // left line
            for (int x = 0; x < w; x++)
            {
                if (!VLineClear(x))
                {
                    _downSampleLeft = x;
                    break;
                }
            }

            // right line
            for (int x = w - 1; x >= 0; x--)
            {
                if (!VLineClear(x))
                {
                    _downSampleRight = x;
                    break;
                }
            }
        }


        /// <summary>
        ///     This method is called internally to see if there are any pixels in the
        ///     given scan line. This method is used to perform autocropping.
        /// </summary>
        /// <param name="y">The horizontal line to scan.</param>
        /// <returns>True if there were any pixels in this horizontal line.</returns>
        protected bool HLineClear(int y)
        {
            int w = image.Width;
            for (int i = 0; i < w; i++)
            {
                Color pixel = image.GetPixel(i, y);
                if (IsBlack(pixel))
                {
                    return false;
                }
            }
            return true;
        }

        /// <summary>
        ///     This method is called to determine if a vertical line is clear.
        /// </summary>
        /// <param name="x">The vertical line to scan.</param>
        /// <returns>True if there are any pixels in the specified vertical line.</returns>
        protected bool VLineClear(int x)
        {
            int h = image.Height;
            for (int i = 0; i < h; i++)
            {
                Color pixel = image.GetPixel(x, i);
                if (IsBlack(pixel))
                {
                    return false;
                }
            }
            return true;
        }

        protected bool IsBlack(Color pixel)
        {
            return (pixel.R != 255 || pixel.G != 255 || pixel.B != 255);
        }
    }
}