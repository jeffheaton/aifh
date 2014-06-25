using AIFH_Vol2.Core.Randomize;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol2.Core.General.Collections
{
    /// <summary>
    /// This class is used to choose between several objects with a specified probability.
    /// </summary>
    [Serializable]
    public class ChooseObject<T>
    {
        /// <summary>
        /// The objects that we are choosing from.
        /// </summary>
        private IList<ObjectHolder<T>> _list = new List<ObjectHolder<T>>();

        /// <summary>
        /// The random choose.
        /// </summary>
        private RandomChoice _chooser;

        /// <summary>
        /// Finalize the structure and set the probabilities.
        /// </summary>
        public void FinalizeStructure()
        {
            double[] d = new double[Count];
            for (int i = 0; i < Count; i++)
            {
                d[i] = _list[i].Probability;
            }

            _chooser = new RandomChoice(d);
        }

        /// <summary>
        /// Add an object.
        /// </summary>
        /// <param name="probability">The probability to choose this object.</param>
        /// <param name="opp">The object to add.</param>
        public void Add(double probability, T opp)
        {
            _list.Add(new ObjectHolder<T>(opp, probability));
        }

        /// <summary>
        /// The number of objects added.
        /// </summary>
        public int Count
        {
            get
            {
                return _list.Count;
            }
        }

        /// <summary>
        /// Choose a random object.
        /// </summary>
        /// <param name="theGenerator">Random number generator.</param>
        /// <returns>The random choice.</returns>
        public T Pick(IGenerateRandom theGenerator)
        {
            int index = _chooser.Generate(theGenerator);
            return _list[index].Obj;
        }

        /// <summary>
        /// The object to choose from.
        /// </summary>
        public IList<ObjectHolder<T>> ObjList
        {
            get
            {
                return _list;
            }
        }

        /// <summary>
        /// Clear all objects from the collection.
        /// </summary>
        public void Clear()
        {
            _list.Clear();
        }

        /// <summary>
        /// The first object in the list.
        /// </summary>
        /// <returns></returns>
        public T PickFirst()
        {
            return _list[0].Obj;
        }
    }
}
