using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.ANN.Activation;

namespace AIFH_Vol3_Core.Core.ANN
{
    /// <summary>
    /// A dropout layer.  This layer behaves just like a reugular fully-connected layer once the network is trained.
    /// However, during training some of the neurons will "drop out" of the layer.This greatly reduces overfitting.
    ///
    /// Srivastava, N., Hinton, G., Krizhevsky, A., Sutskever, I., & Salakhutdinov, R. (2014). Dropout: A simple way to
    /// prevent neural networks from overfitting.The Journal of Machine Learning Research, 15(1), 1929-1958.
    /// </summary>
    public class DropoutLayer : BasicLayer
    {
        /// <summary>
        /// The probability of a neuron dropping out.
        /// </summary>
        public double DropoutProbability { get; set; }

        /// <summary>
        /// Which neurons are currently active.
        /// </summary>
        private readonly bool[] _active;
        
        /// <summary>
        /// Construct a dropout layer. 
        /// </summary>
        /// <param name="theActivation">The activation function.</param>
        /// <param name="theHasBias">True, if this layer has bias.  Dropout layers usually will.</param>
        /// <param name="theCount">The count of neurons.</param>
        /// <param name="theDropout">The dropout probability.</param>
        public DropoutLayer(IActivationFunction theActivation, bool theHasBias, int theCount, double theDropout) :
            base(theActivation, theHasBias, theCount)
        {
            DropoutProbability = theDropout;
            _active = new bool[theCount];
            for (int i = 0; i < _active.Length; i++)
            {
                _active[i] = true;
            }
        }

        /// <inheritdoc/>
        public override void TrainingBatch(IGenerateRandom rnd)
        {
            for (int i = 0; i < _active.Length; i++)
            {
                _active[i] = rnd.NextDouble() > DropoutProbability;
            }
        }

        /// <summary>
        /// The neurons in this layer that are currently active.
        /// </summary>
        public bool[] Active
        {
            get
            {
                return _active;
            }
        }

        /// <inheritdoc/>
        public override bool IsActive(int i)
        {
            if (Owner.IsNetworkTraining)
            {
                if (i < _active.Length)
                {
                    return _active[i];
                }
                else
                {
                    return true;// bias always active.
                }
            }
            else
            {
                return true;
            }
        }
    }
}
