/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
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
package com.heatonresearch.aifh.examples.ga.tsp;

/**
 * City: Holds the location of a city for the traveling salesman problem.
 *
 * @author Jeff Heaton
 */
public class City {
    /**
     * The city's x position.
     */
    int xpos;

    /**
     * The city's y position.
     */
    int ypos;

    /**
     * Constructor.
     *
     * @param x The city's x position
     * @param y The city's y position.
     */
    public City(final int x, final int y) {
        this.xpos = x;
        this.ypos = y;
    }

    /**
     * Return's the city's x position.
     *
     * @return The city's x position.
     */
    int getx() {
        return this.xpos;
    }

    /**
     * Returns the city's y position.
     *
     * @return The city's y position.
     */
    int gety() {
        return this.ypos;
    }

    /**
     * Returns how close the city is to another city.
     *
     * @param cother The other city.
     * @return A distance.
     */
    public int proximity(final City cother) {
        return proximity(cother.getx(), cother.gety());
    }

    /**
     * Returns how far this city is from a a specific point. This method uses
     * the pythagorean theorem to calculate the distance.
     *
     * @param x The x coordinate
     * @param y The y coordinate
     * @return The distance.
     */
    int proximity(final int x, final int y) {
        final int xdiff = this.xpos - x;
        final int ydiff = this.ypos - y;
        return (int) Math.sqrt(xdiff * xdiff + ydiff * ydiff);
    }
}
