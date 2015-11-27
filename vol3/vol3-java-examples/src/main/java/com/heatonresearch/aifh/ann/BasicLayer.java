package com.heatonresearch.aifh.ann;

import com.heatonresearch.aifh.AIFH;
import com.heatonresearch.aifh.ann.activation.ActivationFunction;

public class BasicLayer {
    /**
     * The activation function.
     */
    private ActivationFunction activation;

    /**
     * The neuron count.
     */
    private int count;

    /**
     * The layer that feeds this layer's context.
     */
    private BasicLayer contextFedBy;

    private boolean hasBias;

    /**
     * Do not use this constructor.  This was added to support serialization.
     */
    public BasicLayer() {

    }

    public BasicLayer(final ActivationFunction theActivation, boolean theHasBias, int theCount) {
        this.activation = theActivation;
        this.hasBias = theHasBias;
        this.count = theCount;
    }


    /**
     * @return the activation
     */
    public ActivationFunction getActivation() {
        return this.activation;
    }

    /**
     * @return The number of neurons our context is fed by.
     */
    public int getContextCount() {
        if (this.contextFedBy == null) {
            return 0;
        } else {
            return this.contextFedBy.getCount();
        }
    }

    /**
     * @return The layer that feeds this layer's context.
     */
    public BasicLayer getContextFedBy() {
        return this.contextFedBy;
    }

    /**
     * @return the count
     */
    public int getCount() {
        return this.count;
    }

    /**
     * @return The total number of neurons on this layer, includes context, bias
     *         and regular.
     */
    public int getTotalCount() {
        if (this.contextFedBy == null) {
            return getCount() + (hasBias() ? 1 : 0);
        } else {
            return getCount() + (hasBias() ? 1 : 0)
                    + this.contextFedBy.getCount();
        }
    }

    /**
     * @return the bias
     */
    public boolean hasBias() {
        return this.hasBias;
    }

    /**
     * @param activation
     *            the activation to set
     */
    public void setActivation(final ActivationFunction activation) {
        this.activation = activation;
    }

    /**
     * Set the layer that this layer's context is fed by.
     *
     * @param from
     *            The layer feeding.
     */
    public void setContextFedBy(final BasicLayer from) {
        this.contextFedBy = from;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        final StringBuilder result = new StringBuilder();
        result.append("[");
        result.append(this.getClass().getSimpleName());
        result.append(": count=");
        result.append(this.count);
        result.append(",bias=" + hasBias);

        if (this.contextFedBy != null) {
            result.append(",contextFed=");
            if (this.contextFedBy == this) {
                result.append("itself");
            } else {
                result.append(this.contextFedBy);
            }
        }
        result.append("]");
        return result.toString();
    }

}
