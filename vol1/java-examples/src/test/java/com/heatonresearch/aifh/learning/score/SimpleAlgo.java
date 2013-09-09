/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
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

package com.heatonresearch.aifh.learning.score;

import com.heatonresearch.aifh.learning.ClassificationAlgorithm;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;

/**
 * Simple learning algorithm to test.
 */
public class SimpleAlgo implements RegressionAlgorithm, ClassificationAlgorithm {
    private final double[] cannedResults;
    private int currentIndex = 0;

    public SimpleAlgo(final double[] theCannedResults) {
        this.cannedResults = theCannedResults;
    }

    @Override
    public int computeClassification(final double[] input) {
        return (int) this.cannedResults[this.currentIndex++];
    }

    @Override
    public double[] computeRegression(final double[] input) {
        final double[] result = new double[1];
        result[0] = this.cannedResults[currentIndex++];
        return result;
    }

    @Override
    public double[] getLongTermMemory() {
        return new double[0];  //To change body of implemented methods use File | Settings | File Templates.
    }
}
