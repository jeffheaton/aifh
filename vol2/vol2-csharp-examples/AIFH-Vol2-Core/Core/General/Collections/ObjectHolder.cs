using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.General.Collections
{
    /// <summary>
    /// Holds an object of the specified type.  Used with the ObjectChooser.
    /// Associates an object with a probability.
    /// </summary>
    public class ObjectHolder<T>
    {
        /// <summary>
        /// The object to hold.
        /// </summary>
        private readonly T _obj;

        /// <summary>
        /// The probability.
        /// </summary>
        private double _probability;

        public ObjectHolder(T theObj, double probability)
        {
            _obj = theObj;
            _probability = probability;
        }

        /// <summary>
        /// The object.
        /// </summary>
        public T Obj
        {
            get
            {
                return _obj;
            }
        }

        /// <summary>
        /// The probability.
        /// </summary>
        public double Probability
        {
            get
            {
                return _probability;
            }
        }
    }
}
