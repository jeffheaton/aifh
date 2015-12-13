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
package com.heatonresearch.aifh.som.neighborhood;

import com.heatonresearch.aifh.AIFHError;
import com.heatonresearch.aifh.general.fns.*;

/**
 * Implements a multi-dimensional RBF neighborhood function.  
 *
 */
public class NeighborhoodRBF implements NeighborhoodFunction {

	/**
	 * The radial basis function to use.
	 */
	private FnRBF rbf;

	/**
	 * The size of each dimension.
	 */
	private final int[] size;

	/**
	 * The displacement of each dimension, when mapping the dimensions
	 * to a 1d array.
	 */
	private int[] displacement;

    private double[] params;

    private boolean hexagon;

    public static final double sq75 = Math.sqrt(0.75);

	/**
	 * Construct a 2d neighborhood function based on the sizes for the
	 * x and y dimensions.
	 * @param type The RBF type to use.
	 * @param x The size of the x-dimension.
	 * @param y The size of the y-dimension.
	 */
	public NeighborhoodRBF(final RBFEnum type, final int x, final int y) {
		final int[] size = new int[2];
		size[0] = x;
		size[1] = y;

        this.params = new double[3];

		switch (type) {
		case Gaussian:
			this.rbf = new GaussianFunction(2,this.params,0);
			break;
		case InverseMultiquadric:
			this.rbf = new InverseMultiquadricFunction(2,this.params,0);
			break;
		case Multiquadric:
			this.rbf = new MultiquadricFunction(2,this.params,0);
			break;
		case MexicanHat:
			this.rbf = new MexicanHatFunction(2,this.params,0);
			break;
		}

		this.rbf.setWidth(1);

		this.size = size;

		calculateDisplacement();
	}

	/**
	 * Construct a multi-dimensional neighborhood function.
	 * @param size The sizes of each dimension.
	 * @param type The RBF type to use.
	 */
	public NeighborhoodRBF(final int[] size, final RBFEnum type) {
        this.params = new double[size.length];

		switch (type) {
		case Gaussian:
			this.rbf = new GaussianFunction(size.length,this.params,0);
			break;
		case InverseMultiquadric:
			this.rbf = new InverseMultiquadricFunction(size.length,this.params,0);
			break;
		case Multiquadric:
			this.rbf = new MultiquadricFunction(size.length,this.params,0);
			break;
		case MexicanHat:
			this.rbf = new MexicanHatFunction(size.length,this.params,0);
			break;
		}
		this.size = size;
		calculateDisplacement();
	}

	/**
	 * Calculate all of the displacement values.
	 */
	private void calculateDisplacement() {
		this.displacement = new int[this.size.length];
		for (int i = 0; i < this.size.length; i++) {
			int value;

			if (i == 0) {
				value = 0;
			} else if (i == 1) {
				value = this.size[0];
			} else {
				value = this.displacement[i - 1] * this.size[i - 1];
			}

			this.displacement[i] = value;
		}
	}

	/**
	 * Calculate the value for the multi RBF function.
	 * @param currentNeuron The current neuron.
	 * @param bestNeuron The best neuron.
	 * @return A percent that determines the amount of training the current
	 * neuron should get.  Usually 100% when it is the bestNeuron.
	 */
	public double function(final int currentNeuron, final int bestNeuron) {
		final double[] vector = new double[this.displacement.length];
		final int[] vectorCurrent = translateCoordinates(currentNeuron);
		final int[] vectorBest = translateCoordinates(bestNeuron);
		for (int i = 0; i < vectorCurrent.length; i++) {
			vector[i] = vectorCurrent[i] - vectorBest[i];
		}

        if( this.hexagon ) {
            double row = vector[1];
            double col = vector[0];
            double evenIndent = 1;
            double oddIndent = 2.5;
            double indent = ((row%2==1)?oddIndent:evenIndent);

            vector[1] = (int)(sq75+(row * sq75 ));
            vector[0] = (int)(indent+(3*col));

        }


		return this.rbf.evaluate(vector);

	}

	/**
	 * @return The radius.
	 */
	public double getRadius() {
		return this.rbf.getWidth();
	}

	/**
	 * @return The RBF to use.
	 */
	public FnRBF getRBF() {
		return this.rbf;
	}

	/**
	 * Set the radius.
	 * @param radius The radius.
	 */
	public void setRadius(final double radius) {
		this.rbf.setWidth(radius);
	}

	/**
	 * Translate the specified index into a set of multi-dimensional
	 * coordinates that represent the same index.  This is how the
	 * multi-dimensional coordinates are translated into a one dimensional
	 * index for the input neurons.
	 * @param index The index to translate.
	 * @return The multi-dimensional coordinates.
	 */
	private int[] translateCoordinates(final int index) {
		final int[] result = new int[this.displacement.length];
		int countingIndex = index;

		for (int i = this.displacement.length - 1; i >= 0; i--) {
			int value;
			if (this.displacement[i] > 0) {
				value = countingIndex / this.displacement[i];
			} else {
				value = countingIndex;
			}

			countingIndex -= this.displacement[i] * value;
			result[i] = value;

		}

		return result;
	}

    public boolean isHexagon() {
        return this.hexagon;
    }

    public void setHexagon(final boolean theHexagon) {
        if( theHexagon && this.size.length!=2) {
            throw new AIFHError("Hexagon lattice can only be used in two dimensions.");
        }
        this.hexagon = theHexagon;
    }
}
