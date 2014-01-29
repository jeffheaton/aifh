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

public class Universe implements Cloneable {
	private final int cellSize;
	private final UniverseCell[][] data;

	public Universe(final int height, final int width, final int theSize) {
		this.cellSize = theSize;
		this.data = new UniverseCell[height][width];
		for (int row = 0; row < height; row++) {
			for (int col = 0; col < width; col++) {
				this.data[row][col] = new UniverseCell(theSize);
			}
		}
	}

	public void add(final int row, final int col, final int i, final double d) {
		this.data[row][col].add(i, d);

	}

	@Override
	public Object clone() {
		final Universe result = new Universe(getHeight(), getWidth(),
				this.cellSize);
		result.copy(this);
		return result;
	}

	public double compare(final Universe otherUniverse) {
		int result = 0;
		int total = 0;
		for (int row = 0; row < otherUniverse.getHeight(); row++) {
			for (int col = 0; col < otherUniverse.getWidth(); col++) {
				final int d1 = Math.abs((int) (255 * get(row, col).getAvg()));
				final int d2 = Math.abs((int) (255 * otherUniverse
						.get(row, col).getAvg()));
				if (Math.abs(d1 - d2) > 10) {
					result++;
				}
				total++;
			}
		}

		return (double) result / (double) total;
	}

	public void copy(final Universe source) {
		for (int row = 0; row < getHeight(); row++) {
			for (int col = 0; col < getWidth(); col++) {
				for (int i = 0; i < this.cellSize; i++) {
					this.data[row][col].set(i, source.get(row, col).get(i));
				}
			}
		}
	}

	public UniverseCell get(final int row, final int col) {
		return this.data[row][col];
	}

	public double get(final int row, final int col, final int i) {
		return this.data[row][col].get(i);
	}

	public int getCellSize() {
		return this.cellSize;
	}

	public UniverseCell[][] getData() {
		return this.data;
	}

	public int getHeight() {
		return this.data.length;
	}

	public int getWidth() {
		return this.data[0].length;
	}

	public boolean isValid(final int row, final int col) {
        return !(row < 0 || col < 0 || row >= getHeight() || col >= getWidth());
    }

	public UniverseCell newCell() {
		return new UniverseCell(this.cellSize);
	}

	public void randomize() {
		for (int row = 0; row < getHeight(); row++) {
			for (int col = 0; col < getWidth(); col++) {
				for (int i = 0; i < 3; i++) {
					this.data[row][col].randomize();
				}
			}
		}
	}

}
