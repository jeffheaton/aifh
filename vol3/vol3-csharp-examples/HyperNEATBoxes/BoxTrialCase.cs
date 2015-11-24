using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HyperNEATBoxes
{
    /// <summary>
    /// One case in the boxes score.  Position the boxes at random locations.
    /// </summary>
    public class BoxTrialCase
    {
        public const int BASE_RESOLUTION = 11;
        public const int BOUNDS = BASE_RESOLUTION - 1;

        private IntPair smallBoxTopLeft;
        private IntPair largeBoxTopLeft;

        private Random rnd;

        public BoxTrialCase(Random theRnd)
        {
            this.rnd = theRnd;
        }

        public IntPair InitTestCase(int largeBoxRelativePos)
        {
            IntPair[] loc = GenerateRandomTestCase(largeBoxRelativePos);
            smallBoxTopLeft = loc[0];
            largeBoxTopLeft = (IntPair)loc[1].Clone();
            largeBoxTopLeft.Add(-1);
            return loc[1];
        }

        public double GetPixel(double x, double y)
        {
            int pixelX = (int)(((x + 1.0) * BoxTrialCase.BASE_RESOLUTION) / 2.0);
            int pixelY = (int)(((y + 1.0) * BoxTrialCase.BASE_RESOLUTION) / 2.0);

            if (smallBoxTopLeft.X == pixelX
                    && smallBoxTopLeft.Y == pixelY)
            {
                return 1.0;
            }

            int deltaX = pixelX - largeBoxTopLeft.X;
            int deltaY = pixelY - largeBoxTopLeft.Y;
            return (deltaX > -1 && deltaX < 3 && deltaY > -1 && deltaY < 3) ? 1.0
                    : 0.0;
        }

        private IntPair[] GenerateRandomTestCase(int largeBoxRelativePos)
        {
            IntPair smallBoxPos = new IntPair(rnd.Next(BoxTrialCase.BASE_RESOLUTION),
                    rnd.Next(BoxTrialCase.BASE_RESOLUTION));

            IntPair largeBoxPos = (IntPair)smallBoxPos.Clone();
            switch (largeBoxRelativePos)
            {
                case 0:
                    largeBoxPos.AddX(5);
                    break;
                case 1:
                    largeBoxPos.AddY(5);
                    break;
                case 2:
                    if (rnd.NextDouble() > 0.5)
                    {
                        largeBoxPos.Add(3, 4);
                    }
                    else
                    {
                        largeBoxPos.Add(4, 3);
                    }
                    break;
            }

            if (largeBoxPos.X > BoxTrialCase.BOUNDS)
            {
                largeBoxPos.AddX(-BoxTrialCase.BASE_RESOLUTION);

                if (0 == largeBoxPos.X)
                {
                    largeBoxPos.Add(1);
                }
            }
            else if (BoxTrialCase.BOUNDS == largeBoxPos.X)
            {
                largeBoxPos.AddX(-1);
            }
            else if (largeBoxPos.X == 0)
            {
                largeBoxPos.AddX(1);
            }

            if (largeBoxPos.Y > BoxTrialCase.BOUNDS)
            {
                largeBoxPos.AddY(-BoxTrialCase.BASE_RESOLUTION);

                if (0 == largeBoxPos.Y)
                {
                    largeBoxPos.AddY(1);
                }
            }
            else if (BoxTrialCase.BOUNDS == largeBoxPos.Y)
            {
                largeBoxPos.AddY(-1);
            }
            else if (0 == largeBoxPos.Y)
            {
                largeBoxPos.AddY(1);
            }
            return new IntPair[] { smallBoxPos, largeBoxPos };
        }
    }
}
