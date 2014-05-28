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
package com.heatonresearch.aifh.examples.capstone.model.milestone2;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.randomize.GenerateRandom;

import java.util.ArrayList;
import java.util.List;

/**
 * Used to perform a k-leave out cross validation.
 * This works by breaking the data set into k (often 5) random subsets.  From this we can create 5 training & validation
 * sets. Each validation set becomes one of the k random subsets.  For each validation set a corresponding training set
 * is created by using all data, but leaving out the validation set.
 * <p/>
 * http://en.wikipedia.org/wiki/Cross-validation_(statistics)
 */
public class CrossValidate {

    /**
     * The folds of the cross validation.
     */
    private final List<CrossValidateFold> folds = new ArrayList<CrossValidateFold>();

    /**
     * The constructor.
     *
     * @param k        The number of folds.
     * @param training The training set.
     * @param rnd      A random number generator.
     */
    public CrossValidate(int k, List<BasicData> training, GenerateRandom rnd) {
        List<BasicData> temp = new ArrayList<BasicData>();
        temp.addAll(training);

        // Setup k validation sets.
        for (int i = 0; i < k; i++) {
            folds.add(new CrossValidateFold());
        }

        // Divide over the k sets.
        int leaveOutSet = 0;

        while (temp.size() > 0) {
            int idx = rnd.nextInt(temp.size());
            BasicData item = temp.get(idx);
            temp.remove(idx);

            this.folds.get(leaveOutSet).getValidationSet().add(item);
            for (int includeSet = 0; includeSet < this.folds.size(); includeSet++) {
                if (includeSet != leaveOutSet) {
                    this.folds.get(includeSet).getTrainingSet().add(item);
                }
            }

            leaveOutSet++;
            if (leaveOutSet >= k) {
                leaveOutSet = 0;
            }
        }
    }

    /**
     * @return The folds.
     */
    public List<CrossValidateFold> getFolds() {
        return this.folds;
    }

    /**
     * @return The average score over all folds.
     */
    public double getScore() {
        double sum = 0;
        for (CrossValidateFold fold : this.folds) {
            sum += fold.getScore();
        }
        return sum / this.folds.size();
    }

    /**
     * @return The number of folds.
     */
    public int size() {
        return this.folds.size();
    }
}
