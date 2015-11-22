namespace AIFH_Vol3.Examples.Classic.Logic
{
    /// <summary>
    /// A very simple input neuron.
    /// </summary>
    public class InputNeuron: INeuron
    {
        /// <summary>
        /// The value of the input neuron.
        /// </summary>
        public double Value { get; set; }
        
        /// <inheritdoc/>
        public double Compute()
        {
            return Value;
        }
    }
}
