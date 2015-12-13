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
package com.heatonresearch.aifh.energetic;

import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

/**
 * Implements a Boltzmann machine.
 *
 */
public class BoltzmannMachine extends EnergeticNetwork {

	/**
	 * The current temperature of the neural network. The higher the
	 * temperature, the more random the network will behave.
	 */
	private double temperature;

	/**
	 * The thresholds.
	 */
	private double[] threshold;

	/**
	 * Count used to internally determine if a neuron is "on".
	 */
	private transient int[] on;

	/**
	 * Count used to internally determine if a neuron is "off".
	 */
	private transient int[] off;

	/**
	 * The number of cycles to anneal for.
	 */
	private int annealCycles = 100;

	/**
	 * The number of cycles to run the network through before annealing.
	 */
	private int runCycles = 1000;

    private GenerateRandom random = new MersenneTwisterGenerateRandom();

	/**
	 * Default constructors.
	 */
	public BoltzmannMachine() {
		super();
	}

	/**
	 * Construct a Boltzmann machine with the specified number of neurons.
	 * @param neuronCount The number of neurons.
	 */
	public BoltzmannMachine(final int neuronCount) {
		super(neuronCount);

		this.threshold = new double[neuronCount];
	}

	/**
	 * Note: for Boltzmann networks, you will usually want to call the "run"
	 * method to compute the output.
	 * 
	 * This method can be used to copy the input data to the current state. A
	 * single iteration is then run, and the new current state is returned.
	 * 
	 * @param input
	 *            The input pattern.
	 * @return The new current state.
	 */
	public double[] compute(final double[] input) {
		final double[] result = new double[input.length];
		System.arraycopy(input, 0, getCurrentState(), 0, input.length);
		run();
        System.arraycopy(getCurrentState(), 0, result, 0, result.length);
		return result;
	}

	/**
	 * Decrease the temperature by the specified amount.
	 * 
	 * @param d
	 *            The amount to decrease by, for example .8 to change to
     * 80% of current.
	 */
	public void decreaseTemperature(final double d) {
		this.temperature *= d;
	}

	public void establishEquilibrium() {
		final int count = getNeuronCount();

		if (this.on == null) {
			this.on = new int[count];
			this.off = new int[count];
		}

		for (int i = 0; i < count; i++) {
			this.on[i] = 0;
			this.off[i] = 0;
		}

		for (int n = 0; n < this.runCycles * count; n++) {
			run((int) this.random.nextDouble(0, count - 1));
		}
		for (int n = 0; n < this.annealCycles * count; n++) {
			final int i = (int) this.random.nextDouble(0, count - 1);
			run(i);
			if (getCurrentState()[i]>0) {
				this.on[i]++;
			} else {
				this.off[i]++;
			}
		}

		for (int i = 0; i < count; i++) {
			getCurrentState()[i] = this.on[i] > this.off[i]?1:0;
		}
	}

	/**
	 * @return the annealCycles
	 */
	public int getAnnealCycles() {
		return this.annealCycles;
	}

	/**
	 * {@inheritDoc}
	 */
	public int getInputCount() {
		return getNeuronCount();
	}

	/**
	 * {@inheritDoc}
	 */
	public int getOutputCount() {
		return getNeuronCount();
	}

	/**
	 * @return the runCycles
	 */
	public int getRunCycles() {
		return this.runCycles;
	}

	/**
	 * @return The temperature the network is currently operating at.
	 */
	public double getTemperature() {
		return this.temperature;
	}

	/**
	 * @return the threshold
	 */
	public double[] getThreshold() {
		return this.threshold;
	}

	/**
	 * Run the network for all neurons present.
	 */
	public void run() {
		final int count = getNeuronCount();
		for (int i = 0; i < count; i++) {
			run(i);
		}
	}

	/**
	 * Run the network for the specified neuron.
	 * 
	 * @param i
	 *            The neuron to run for.
	 */
	public void run(final int i) {
		int j;
		double sum, probability;

		final int count = getNeuronCount();

		sum = 0;
		for (j = 0; j < count; j++) {
			sum += getWeight(i, j) * ((getCurrentState()[j]>0) ? 1 : 0);
		}
		sum -= this.threshold[i];
		probability = 1 / (1 + Math.exp(-sum / this.temperature));
		if (this.random.nextDouble() <= probability) {
			getCurrentState()[i] = 1.0;
		} else {
			getCurrentState()[i] = 0.0;
		}
	}

	/**
	 * @param annealCycles
	 *            the annealCycles to set
	 */
	public void setAnnealCycles(final int annealCycles) {
		this.annealCycles = annealCycles;
	}

	/**
	 * @param runCycles
	 *            the runCycles to set
	 */
	public void setRunCycles(final int runCycles) {
		this.runCycles = runCycles;
	}

	/**
	 * Set the network temperature.
	 * 
	 * @param temperature
	 *            The temperature to operate the network at.
	 */
	public void setTemperature(final double temperature) {
		this.temperature = temperature;
	}

	/**
	 * Set the thresholds.
	 * @param t The thresholds.
	 */
	public void setThreshold(final double[] t) {
		this.threshold = t;

	}

	/**
	 * {@inheritDoc}
	 */
	public void updateProperties() {
		// nothing needed here
	}

    /**
     * @return The random number generator.
     */
    public GenerateRandom getRandom() {
        return this.random;
    }

    /**
     * Set the random number generator.
     * @param random The random number generator.
     */
    public void setRandom(final GenerateRandom random) {
        this.random = random;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public double[] getLongTermMemory() {
        return new double[0];  //To change body of implemented methods use File | Settings | File Templates.
    }
}
