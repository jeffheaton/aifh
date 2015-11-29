package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.ann.activation.ActivationFunction;
import com.heatonresearch.aifh.ann.activation.ActivationLinear;
import com.heatonresearch.aifh.ann.activation.ActivationSigmoid;
import com.heatonresearch.aifh.ann.activation.ActivationTANH;
import com.heatonresearch.aifh.ann.randomize.RangeRandomizeNetwork;
import com.heatonresearch.aifh.ann.randomize.XaiverRandomizeNetwork;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class BasicNetwork implements RegressionAlgorithm {
    /**
     * The default bias activation.
     */
    public static final double DEFAULT_BIAS_ACTIVATION = 1.0;

    /**
     * The value that indicates that there is no bias activation.
     */
    public static final double NO_BIAS_ACTIVATION = 0.0;

    /**
     * The number of input neurons in this network.
     */
    private int inputCount;

    /**
     * The number of neurons in each of the layers.
     */
    private int[] layerCounts;

    /**
     * The number of context neurons in each layer. These context neurons will
     * feed the next layer.
     */
    private int[] layerContextCount;

    /**
     * The number of neurons in each layer that are actually fed by neurons in
     * the previous layer. Bias neurons, as well as context neurons, are not fed
     * from the previous layer.
     */
    private int[] layerFeedCounts;

    /**
     * An index to where each layer begins (based on the number of neurons in
     * each layer).
     */
    private int[] layerIndex;

    /**
     * The outputs from each of the neurons.
     */
    private double[] layerOutput;

    /**
     * The sum of the layer, before the activation function is applied, producing the layerOutput.
     */
    private double[] layerSums;

    /**
     * The number of output neurons in this network.
     */
    private int outputCount;

    /**
     * The index to where the weights that are stored at for a given layer.
     */
    private int[] weightIndex;

    /**
     * The weights for a neural network.
     */
    private double[] weights;

    /**
     * The activation types.
     */
    private ActivationFunction[] activationFunctions;

    /**
     * The context target for each layer. This is how the backwards connections
     * are formed for the recurrent neural network. Each layer either has a
     * zero, which means no context target, or a layer number that indicates the
     * target layer.
     */
    private int[] contextTargetOffset;

    /**
     * The size of each of the context targets. If a layer's contextTargetOffset
     * is zero, its contextTargetSize should also be zero. The contextTargetSize
     * should always match the feed count of the targeted context layer.
     */
    private int[] contextTargetSize;

    /**
     * The bias activation for each layer. This is usually either 1, for a bias,
     * or zero for no bias.
     */
    private double[] biasActivation;

    /**
     * The layer that training should begin on.
     */
    private int beginTraining;

    /**
     * The layer that training should end on.
     */
    private int endTraining;

    /**
     * Does this network have some connections disabled.
     */
    private boolean isLimited;

    /**
     * The limit, under which, all a cconnection is not considered to exist.
     */
    private double connectionLimit;

    /**
     * True if the network has context.
     */
    private boolean hasContext;

    private final List<BasicLayer> layers = new ArrayList<BasicLayer>();

    /**
     * Default constructor.
     */
    public BasicNetwork() {
    }

    /**
     * Clear any connection limits.
     */
    public void clearConnectionLimit() {
        this.connectionLimit = 0.0;
        this.isLimited = false;
    }

    /**
     * Clear any context neurons.
     */
    public void clearContext() {
        int index = 0;

        for (int i = 0; i < this.layerIndex.length; i++) {

            final boolean hasBias = (this.layerContextCount[i] + this.layerFeedCounts[i]) != this.layerCounts[i];

            // fill in regular neurons
            Arrays.fill(this.layerOutput, index, index+this.layerFeedCounts[i], 0);
            index += this.layerFeedCounts[i];

            // fill in the bias
            if (hasBias) {
                this.layerOutput[index++] = this.biasActivation[i];
            }

            // fill in context
            Arrays.fill(this.layerOutput, index, index+this.layerContextCount[i], 0);
            index += this.layerContextCount[i];
        }
    }

    /**
     * Calculate the output for the given input.
     *
     * @param input
     *            The input.
     * @param output
     *            Output will be placed here.
     */
    public void compute(final double[] input, final double[] output) {
        final int sourceIndex = this.layerOutput.length
                - this.layerCounts[this.layerCounts.length - 1];

        System.arraycopy(input, 0, this.layerOutput, sourceIndex,
                this.inputCount);

        for (int i = this.layerIndex.length - 1; i > 0; i--) {
            computeLayer(i);
        }

        // update context values
        final int offset = this.contextTargetOffset[0];

        System.arraycopy(this.layerOutput, 0, layerOutput,
                offset, this.contextTargetSize[0]);

        System.arraycopy(this.layerOutput, 0, output, 0, this.outputCount);
    }


    /**
     * Calculate a layer.
     *
     * @param currentLayer
     *            The layer to calculate.
     */
    protected void computeLayer(final int currentLayer) {

        final int inputIndex = this.layerIndex[currentLayer];
        final int outputIndex = this.layerIndex[currentLayer - 1];
        final int inputSize = this.layerCounts[currentLayer];
        final int outputSize = this.layerFeedCounts[currentLayer - 1];

        int index = this.weightIndex[currentLayer - 1];

        final int limitX = outputIndex + outputSize;
        final int limitY = inputIndex + inputSize;

        // weight values
        for (int x = outputIndex; x < limitX; x++) {
            double sum = 0;
            for (int y = inputIndex; y < limitY; y++) {
                sum += this.weights[index++] * this.layerOutput[y];
            }
            this.layerSums[x] = sum;
            this.layerOutput[x] = sum;
        }

        this.activationFunctions[currentLayer - 1].activationFunction(
                this.layerOutput, outputIndex, outputSize);

        // update context values
        final int offset = this.contextTargetOffset[currentLayer];

        System.arraycopy(this.layerOutput, outputIndex,
                this.layerOutput, offset, this.contextTargetSize[currentLayer]);
    }

    /**
     * @return The activation functions.
     */
    public ActivationFunction[] getActivationFunctions() {
        return this.activationFunctions;
    }

    /**
     * @return the beginTraining
     */
    public int getBeginTraining() {
        return this.beginTraining;
    }

    /**
     * @return The bias activation.
     */
    public double[] getBiasActivation() {
        return this.biasActivation;
    }

    /**
     * @return the connectionLimit
     */
    public double getConnectionLimit() {
        return this.connectionLimit;
    }

    /**
     * @return The offset of the context target for each layer.
     */
    public int[] getContextTargetOffset() {
        return this.contextTargetOffset;
    }

    /**
     * @return The context target size for each layer. Zero if the layer does
     *         not feed a context layer.
     */
    public int[] getContextTargetSize() {
        return this.contextTargetSize;
    }

    /**
     * @return The length of the array the network would encode to.
     */
    public int getEncodeLength() {
        return this.weights.length;
    }

    /**
     * @return the endTraining
     */
    public int getEndTraining() {
        return this.endTraining;
    }

    /**
     * @return True if this network has context.
     */
    public boolean getHasContext() {
        return this.hasContext;
    }

    /**
     * @return The number of input neurons.
     */
    public int getInputCount() {
        return this.inputCount;
    }

    /**
     * @return The layer context count.
     */
    public int[] getLayerContextCount() {
        return this.layerContextCount;
    }

    /**
     * @return The number of neurons in each layer.
     */
    public int[] getLayerCounts() {
        return this.layerCounts;
    }

    /**
     * @return The number of neurons in each layer that are fed by the previous
     *         layer.
     */
    public int[] getLayerFeedCounts() {
        return this.layerFeedCounts;
    }

    /**
     * @return Indexes into the weights for the start of each layer.
     */
    public int[] getLayerIndex() {
        return this.layerIndex;
    }

    /**
     * @return The output for each layer.
     */
    public double[] getLayerOutput() {
        return this.layerOutput;
    }

    /**
     * @return The neuron count.
     */
    public int getNeuronCount() {
        int result = 0;
        for (final int element : this.layerCounts) {
            result += element;
        }
        return result;
    }

    /**
     * @return The number of output neurons.
     */
    public int getOutputCount() {
        return this.outputCount;
    }

    /**
     * @return The index of each layer in the weight and threshold array.
     */
    public int[] getWeightIndex() {
        return this.weightIndex;
    }

    /**
     * @return The index of each layer in the weight and threshold array.
     */
    public double[] getWeights() {
        return this.weights;
    }

    /**
     * Neural networks with only one type of activation function offer certain
     * optimization options. This method determines if only a single activation
     * function is used.
     *
     * @return The number of the single activation function, or -1 if there are
     *         no activation functions or more than one type of activation
     *         function.
     */
    public Class<?> hasSameActivationFunction() {
        final List<Class<?>> map = new ArrayList<Class<?>>();

        for (final ActivationFunction activation : this.activationFunctions) {
            if (!map.contains(activation.getClass())) {
                map.add(activation.getClass());
            }
        }

        if (map.size() != 1) {
            return null;
        } else {
            return map.get(0);
        }
    }

    /**
     * Construct a flat network.
     *
     * @param layers
     *            The layers of the network to create.
     */
    public void init(final BasicLayer[] layers) {

        final int layerCount = layers.length;

        this.inputCount = layers[0].getCount();
        this.outputCount = layers[layerCount - 1].getCount();

        this.layerCounts = new int[layerCount];
        this.layerContextCount = new int[layerCount];
        this.weightIndex = new int[layerCount];
        this.layerIndex = new int[layerCount];

        this.activationFunctions = new ActivationFunction[layerCount];
        this.layerFeedCounts = new int[layerCount];
        this.contextTargetOffset = new int[layerCount];
        this.contextTargetSize = new int[layerCount];
        this.biasActivation = new double[layerCount];

        int index = 0;
        int neuronCount = 0;
        int weightCount = 0;

        for (int i = layers.length - 1; i >= 0; i--) {

            final BasicLayer layer = layers[i];
            BasicLayer nextLayer = null;

            if (i > 0) {
                nextLayer = layers[i - 1];
            }

            this.biasActivation[index] = 1;
            this.layerCounts[index] = layer.getTotalCount();
            this.layerFeedCounts[index] = layer.getCount();
            this.layerContextCount[index] = layer.getContextCount();
            this.activationFunctions[index] = layer.getActivation();

            neuronCount += layer.getTotalCount();

            if (nextLayer != null) {
                weightCount += layer.getCount() * nextLayer.getTotalCount();
            }

            if (index == 0) {
                this.weightIndex[index] = 0;
                this.layerIndex[index] = 0;
            } else {
                this.weightIndex[index] = this.weightIndex[index - 1]
                        + (this.layerCounts[index] * this.layerFeedCounts[index - 1]);
                this.layerIndex[index] = this.layerIndex[index - 1]
                        + this.layerCounts[index - 1];
            }

            int neuronIndex = 0;
            for (int j = layers.length - 1; j >= 0; j--) {
                if (layers[j].getContextFedBy() == layer) {
                    this.hasContext = true;
                    this.contextTargetSize[index] = layers[j].getContextCount();
                    this.contextTargetOffset[index] = neuronIndex
                            + (layers[j].getTotalCount() - layers[j]
                            .getContextCount());
                }
                neuronIndex += layers[j].getTotalCount();
            }

            index++;
        }

        this.beginTraining = 0;
        this.endTraining = this.layerCounts.length - 1;

        this.weights = new double[weightCount];
        this.layerOutput = new double[neuronCount];
        this.layerSums = new double[neuronCount];

        clearContext();
    }

    /**
     * @return the isLimited
     */
    public boolean isLimited() {
        return this.isLimited;
    }

    /**
     * Perform a simple randomization of the weights of the neural network
     * between -1 and 1.
     */
    public void randomize() {
        randomize(1, -1);
    }

    /**
     * Perform a simple randomization of the weights of the neural network
     * between the specified hi and lo.
     *
     * @param hi
     *            The network high.
     * @param lo
     *            The network low.
     */
    public void randomize(final double hi, final double lo) {
        for (int i = 0; i < this.weights.length; i++) {
            this.weights[i] = (Math.random() * (hi - lo)) + lo;
        }
    }

    /**
     * Set the activation functions.
     * @param af The activation functions.
     */
    public void setActivationFunctions(final ActivationFunction[] af) {
        this.activationFunctions = Arrays.copyOf(af, af.length);
    }

    /**
     * @param beginTraining
     *            the beginTraining to set
     */
    public void setBeginTraining(final int beginTraining) {
        this.beginTraining = beginTraining;
    }

    /**
     * Set the bias activation.
     * @param biasActivation The bias activation.
     */
    public void setBiasActivation(final double[] biasActivation) {
        this.biasActivation = biasActivation;
    }

    /**
     * @param endTraining
     *            the endTraining to set
     */
    public void setEndTraining(final int endTraining) {
        this.endTraining = endTraining;
    }

    /**
     * Set the hasContext property.
     * @param hasContext True if the network has context.
     */
    public void setHasContext(final boolean hasContext) {
        this.hasContext = hasContext;
    }

    /**
     * Set the input count.
     * @param inputCount The input count.
     */
    public void setInputCount(final int inputCount) {
        this.inputCount = inputCount;
    }



    /**
     * Set the output count.
     * @param outputCount The output count.
     */
    public void setOutputCount(final int outputCount) {
        this.outputCount = outputCount;
    }


    /**
     * @return the layerSums
     */
    public double[] getLayerSums() {
        return layerSums;
    }

    /**
     * Get the weight between the two layers.
     * @param fromLayer The from layer.
     * @param fromNeuron The from neuron.
     * @param toNeuron The to neuron.
     * @return The weight value.
     */
    public double getWeight(final int fromLayer,
                            final int fromNeuron,
                            final int toNeuron) {
        validateNeuron(fromLayer, fromNeuron);
        validateNeuron(fromLayer + 1, toNeuron);
        final int fromLayerNumber = this.layerContextCount.length - fromLayer - 1;
        final int toLayerNumber = fromLayerNumber - 1;

        if (toLayerNumber < 0) {
            throw new AIFHError(
                    "The specified layer is not connected to another layer: "
                            + fromLayer);
        }

        final int weightBaseIndex
                = this.weightIndex[toLayerNumber];
        final int count
                = this.layerCounts[fromLayerNumber];
        final int weightIndex = weightBaseIndex + fromNeuron
                + (toNeuron * count);

        return this.weights[weightIndex];
    }

    /**
     * Validate the the specified targetLayer and neuron are valid.
     * @param targetLayer The target layer.
     * @param neuron The target neuron.
     */
    public void validateNeuron(final int targetLayer, final int neuron) {
        if ((targetLayer < 0) || (targetLayer >= this.layerCounts.length)) {
            throw new AIFHError("Invalid layer count: " + targetLayer);
        }

        if ((neuron < 0) || (neuron >= getLayerTotalNeuronCount(targetLayer))) {
            throw new AIFHError("Invalid neuron number: " + neuron);
        }
    }

    /**
     * Get the total (including bias and context) neuron cont for a layer.
     * @param l The layer.
     * @return The count.
     */
    public int getLayerTotalNeuronCount(final int l) {
        final int layerNumber = this.layerCounts.length - l - 1;
        return this.layerCounts[layerNumber];
    }

    /**
     * Get the neuron count.
     * @param l The layer.
     * @return The neuron count.
     */
    public int getLayerNeuronCount(final int l) {
        final int layerNumber = this.layerCounts.length - l - 1;
        return this.layerFeedCounts[layerNumber];
    }

    public void addLayer(BasicLayer layer) {
        this.layers.add(layer);
    }

    public void finalizeStructure() {
        final int layerCount = layers.size();

        this.inputCount = layers.get(0).getCount();
        this.outputCount = layers.get(layerCount - 1).getCount();

        this.layerCounts = new int[layerCount];
        this.layerContextCount = new int[layerCount];
        this.weightIndex = new int[layerCount];
        this.layerIndex = new int[layerCount];

        this.activationFunctions = new ActivationFunction[layerCount];
        this.layerFeedCounts = new int[layerCount];
        this.contextTargetOffset = new int[layerCount];
        this.contextTargetSize = new int[layerCount];
        this.biasActivation = new double[layerCount];

        int index = 0;
        int neuronCount = 0;
        int weightCount = 0;

        for (int i = layers.size() - 1; i >= 0; i--) {

            final BasicLayer layer = layers.get(i);
            BasicLayer nextLayer = null;

            if (i > 0) {
                nextLayer = layers.get(i - 1);
            }

            this.biasActivation[index] = 1;
            this.layerCounts[index] = layer.getTotalCount();
            this.layerFeedCounts[index] = layer.getCount();
            this.layerContextCount[index] = layer.getContextCount();
            this.activationFunctions[index] = layer.getActivation();

            neuronCount += layer.getTotalCount();

            if (nextLayer != null) {
                weightCount += layer.getCount() * nextLayer.getTotalCount();
            }

            if (index == 0) {
                this.weightIndex[index] = 0;
                this.layerIndex[index] = 0;
            } else {
                this.weightIndex[index] = this.weightIndex[index - 1]
                        + (this.layerCounts[index] * this.layerFeedCounts[index - 1]);
                this.layerIndex[index] = this.layerIndex[index - 1]
                        + this.layerCounts[index - 1];
            }

            int neuronIndex = 0;
            for (int j = layers.size() - 1; j >= 0; j--) {
                if (layers.get(j).getContextFedBy() == layer) {
                    this.hasContext = true;
                    this.contextTargetSize[index] = layers.get(j).getContextCount();
                    this.contextTargetOffset[index] = neuronIndex
                            + (layers.get(j).getTotalCount() - layers.get(j)
                            .getContextCount());
                }
                neuronIndex += layers.get(j).getTotalCount();
            }

            index++;
        }

        this.beginTraining = 0;
        this.endTraining = this.layerCounts.length - 1;

        this.weights = new double[weightCount];
        this.layerOutput = new double[neuronCount];
        this.layerSums = new double[neuronCount];

        clearContext();
    }

    /**
     * Set the weight between the two specified neurons. The bias neuron is always
     * the last neuron on a layer.
     * @param fromLayer The from layer.
     * @param fromNeuron The from neuron.
     * @param toNeuron The to neuron.
     * @param value The to value.
     */
    public void setWeight(final int fromLayer, final int fromNeuron,
                          final int toNeuron, final double value) {
        final int fromLayerNumber = this.layers.size() - fromLayer - 1;
        final int toLayerNumber = fromLayerNumber - 1;

        if (toLayerNumber < 0) {
            throw new AIFHError(
                    "The specified layer is not connected to another layer: "
                            + fromLayer);
        }

        final int weightBaseIndex
                = getWeightIndex()[toLayerNumber];
        final int count
                = getLayerCounts()[fromLayerNumber];
        final int weightIndex = weightBaseIndex + fromNeuron
                + (toNeuron * count);

        getWeights()[weightIndex] = value;
    }

    public void reset() {
        XaiverRandomizeNetwork random = new XaiverRandomizeNetwork();
        random.randomize(this);
    }

    public List<BasicLayer> getLayers() {
        return this.layers;
    }

    /**
     * Compute the output for the specified input.
     *
     * @param input The input.
     * @return The regression output.
     */
    @Override
    public double[] computeRegression(double[] input) {
        double[] output = new double[getOutputCount()];
        compute(input,output);
        return output;
    }

    /**
     * @return The long term memory for the algorithm.  This is usually weights or other coefficients.
     */
    @Override
    public double[] getLongTermMemory() {
        return this.weights;
    }
}
