using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HyperNEATBoxes
{
    [Serializable]
    public class IntPair
    {
        public int X { get; set; }
        public int Y { get; set; }

        public IntPair(int theX, int theY)
        {
            X = theX;
            Y = theY;
        }

        public void Add(int v)
        {
            X += v;
            Y += v;
        }

        public void AddX(int v)
        {
            X += v;
        }

        public void AddY(int v)
        {
            Y += v;
        }

        public void Add(int addX, int addY)
        {
            X += addX;
            Y += addY;
        }

        public Object Clone()
        {
            return new IntPair(X, Y);
        }

        public override String ToString()
        {
            StringBuilder result = new StringBuilder();
            result.Append("[IntPair:");
            result.Append(X);
            result.Append(";");
            result.Append(Y);
            result.Append("]");
            return result.ToString();
        }

    }
}
