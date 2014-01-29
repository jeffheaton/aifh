/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2014 by Jeff Heaton
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
package com.heatonresearch.aifh.vol2.examples.mergelife.universe;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import com.heatonresearch.aifh.vol2.examples.mergelife.physics.Physics;

public class UniverseRunner {

	/**
	 * The event used to sync waiting for tasks to stop.
	 */
	private final Lock accessLock = new ReentrantLock();
	private boolean autoKill;
	private double diff;
	private int iteration;
	private final Physics physics;
	private final Universe tempUniverse;

	private final Universe universe;

	public UniverseRunner(final Universe theUniverse, final Physics thePhysics) {
		this.universe = theUniverse;
		this.tempUniverse = (Universe) theUniverse.clone();
		this.physics = thePhysics;
	}

	public void advance() {
		final int height = this.universe.getHeight();
		final int width = this.universe.getWidth();

		try {
			this.accessLock.lock();
			this.tempUniverse.copy(this.universe);

			for (int col = 0; col < width; col++) {
				for (int row = 0; row < height; row++) {
					this.physics.processPixel(this.tempUniverse, row, col);
				}
			}

			this.diff = this.tempUniverse.compare(this.universe);

			this.iteration++;

			this.universe.copy(this.tempUniverse);

			if (this.diff < 0.0001 && this.iteration > 5) {
				if (this.autoKill) {
					reset();
				}
			}
		} finally {
			this.accessLock.unlock();
		}
	}

	public void crossover(final UniverseRunner crossoverParent1,
			final UniverseRunner crossoverParent2) {
		final double[] parent1 = crossoverParent1.getPhysics().getData();
		final double[] parent2 = crossoverParent2.getPhysics().getData();
		final double[] child = getPhysics().getData();
		final int len = parent1.length;
		final int p1 = (int) (Math.random() * (double)len);
		final int p2 = (int) (Math.random() * (double)len);

		for (int i = 0; i < getPhysics().getData().length; i++) {
			if (i < p1) {
				child[i] = parent1[i];
			} else if (i >= p1 && i <= p2) {
				child[i] = parent2[i];
			} else if (i > p2) {
				child[i] = parent1[i];
			}
		}
	}

	public double getDiff() {
		return this.diff;
	}

	public int getIterations() {
		return this.iteration;
	}

	public Physics getPhysics() {
		return this.physics;
	}

	/**
	 * @return the autoKill
	 */
	public boolean isAutoKill() {
		return this.autoKill;
	}

	public void mutate(final Physics sourcePhysics, final double probChange,
			final double perterb) {
		getPhysics().copyData(sourcePhysics.getData());

		for (int i = 0; i < sourcePhysics.getData().length; i++) {
			if (Math.random() < probChange) {
				getPhysics().getData()[i] += perterb * (Math.random() * 2 - 1);
			}
		}

	}

	public void randomize() {
		this.accessLock.lock();
		this.universe.randomize();
		this.iteration = 0;
		this.accessLock.unlock();
	}

	public void reset() {
		this.accessLock.lock();
		this.physics.randomize();
		this.universe.randomize();
		this.iteration = 0;
		this.accessLock.unlock();
	}

	public int runToConverge(final int maxIterations) {
		this.iteration = 0;
		for (;;) {
			advance();

			if (this.iteration > 5 && this.diff < 0.01) {
				break;
			}

			if (this.iteration > maxIterations) {
				break;
			}
		}
		return this.iteration;
	}

	/**
	 * @param autoKill
	 *            the autoKill to set
	 */
	public void setAutoKill(final boolean autoKill) {
		this.autoKill = autoKill;
	}

	@Override
	public String toString() {
		return "Iteration: " + this.iteration + ", Diff=" + this.diff;
	}

}
