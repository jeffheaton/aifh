/*
 * Artificial Intelligence for Humans
 * Volume 3: Deep Learning and Neural Networks
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014-2015 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */
package com.heatonresearch.aifh.general.data;

import com.heatonresearch.aifh.AIFHError;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * This class is used to store both the input and ideal vectors for a single item of training data.  A label can also
 * be applied.
 */
public class BasicData {
    /**
     * The input vector.
     */
    private final double[] input;

    /**
     * The ideal (or expected output) vector.
     */
    private final double[] ideal;

    /**
     * A label, that can be used to tag this element.
     */
    private String label;

    /**
     * Construct an empty unsupervised element.  An unsupervised element does not have an expected output.
     *
     * @param theInputDimensions The number of dimensions.
     */
    public BasicData(final int theInputDimensions) {
        this(theInputDimensions, 0, null);
    }

    /**
     * Construct an empty supervised element.  A supervised element has both input an ideal.
     *
     * @param theInputDimensions The dimensions for the input vector.
     * @param theIdealDimensions The dimensions for the ideal vector.
     */
    public BasicData(final int theInputDimensions, final int theIdealDimensions) {
        this(theInputDimensions, theIdealDimensions, null);
    }

    public BasicData(final int theInputDimensions, final int theIdealDimensions, final String theLabel) {
        this.label = theLabel;
        this.input = new double[theInputDimensions];
        this.ideal = new double[theIdealDimensions];
    }

    /**
     * Construct a supervised element, with a label.
     *
     * @param theInputData The input data vector.
     * @param theIdealData The ideal data vector.
     * @param theLabel     The label.
     */
    public BasicData(final double[] theInputData, final double[] theIdealData, final String theLabel) {
        this.label = theLabel;
        this.input = theInputData;
        this.ideal = theIdealData;
    }

    /**
     * Construct an unsupervised element, with a label.
     *
     * @param theInputData The input vector.
     * @param theLabel     The label.
     */
    public BasicData(final double[] theInputData, final String theLabel) {
        this(theInputData, new double[0], theLabel);
    }

    /**
     * Construct an unsupervised element, without a label.
     *
     * @param theInputData The input vector.
     */
    public BasicData(final double[] theInputData) {
        this(theInputData, null);
    }

    /**
     * @return The input vector.
     */
    public double[] getInput() {
        return this.input;
    }

    /**
     * @return The ideal vector.
     */
    public double[] getIdeal() {
        return this.ideal;
    }

    /**
     * @return The label vector.
     */
    public String getLabel() {
        return this.label;
    }

    /**
     * Set the label.
     *
     * @param label The label.
     */
    public void setLabel(final String label) {
        this.label = label;
    }

    /**
     * {@inheritDoc}
     */
    public String toString() {
        String result = "[BasicData: input:" +
                Arrays.toString(this.input) +
                ", ideal:" +
                Arrays.toString(this.ideal) +
                ", label:" +
                this.label +
                "]";

        return result;
    }

    /**
     * Convert two 2D arrays into a List of BasicData elements.  One array holds input and the other ideal vectors.
     *
     * @param inputData An array of input vectors.
     * @param idealData An array of ideal vectors.
     * @return A list of BasicData elements.
     */
    public static List<BasicData> convertArrays(final double[][] inputData, final double[][] idealData) {
        // create the list
        final List<BasicData> result = new ArrayList<>();

        // get the lengths
        final int inputCount = inputData[0].length;
        final int idealCount = idealData[0].length;

        // build the list
        for (int row = 0; row < inputData.length; row++) {
            final BasicData dataRow = new BasicData(inputCount, idealCount);
            System.arraycopy(inputData[row], 0, dataRow.getInput(), 0, inputCount);
            System.arraycopy(idealData[row], 0, dataRow.getIdeal(), 0, idealCount);
            result.add(dataRow);
        }

        return result;
    }


    /**
     * Construct supervised training data from an input (X) and ideal (Y).
     * @param theInput The input data (x).
     * @param theIdeal The ideal, or expected (y), data.
     * @return The training set.
     */
    public static List<BasicData> combineXY(double[][] theInput, double[][] theIdeal) {
        if( theInput.length != theIdeal.length ) {
            throw new AIFHError("The element count of the input and ideal element must match: "
                    + theInput.length + " != " + theIdeal.length);
        }

        List<BasicData> result = new ArrayList<>();
        for(int i=0;i<theInput.length;i++) {
            result.add(new BasicData(theInput[i],theIdeal[i],null));
        }
        return result;
    }
}
