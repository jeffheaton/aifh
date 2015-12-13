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
package com.heatonresearch.aifh.examples.neat.hyperneat;

import org.encog.mathutil.IntPair;
import org.encog.ml.CalculateScore;
import org.encog.ml.MLMethod;

import java.util.Random;

/**
 * The objective function for the boxes experiment. This score is described as
 * follows in Kenneth O. Stanley's
 * "A Hypercube-Based Indirect Encoding for Evolving Large-Scale Neural Networks"
 * .
 *
 * The field coordinates range between [-1, 1] in the x and y dimensions.
 * However, the resolution within this range, i.e. the node density, can be
 * varied. During evolution, the resolution of each field is fixed at 11 x 11.
 * Thus the connective CPPN must learn to correctly connect a visual field of
 * 121 inputs to a target field of 121 outputs, a total of 14,641 potential
 * connection strengths.
 *
 * During evolution, each individual in the population is evaluated for its
 * ability to find the center of the bigger object. If the connectivity is not
 * highly accurate, it is likely the substrate will often incorrectly choose the
 * small object over the large one. Each individual evaluation thus includes 75
 * trials, where each trial places the two objects at different locations. The
 * trials are organized as follows. The small object appears at 25 uniformly
 * distributed locations such that it is always completely within the visual
 * field. For each of these 25 locations, the larger object is placed five units
 * to the right, down, and diagonally, once per trial. The large object wraps
 * around to the other side of the field when it hits the border. If the larger
 * object is not completely within the visual field, it is moved the smallest
 * distance possible that places it fully in view. Because of wrapping, this
 * method of evaluation tests cases where the small object is on all possible
 * sides of the large object. Thus many relative positions (though not all) are
 * tested for a total number of 75 trials on the 11 by 11 substrate for each
 * evaluation during evolution.
 *
 * Within each trial, the substrate is activated over the entire visual field.
 * The unit with the highest activation in the target field is interpreted as
 * the substrate's selection. Fitness is calculated from the sum of the squared
 * distances between the target and the point of highest activation over all 75
 * trials. This fitness function rewards generalization and provides a smooth
 * gradient for solutions that are close but not perfect.
 *
 */
public class BoxesScore implements CalculateScore {
    public static final double EDGE_LEN = 2.0;
    public static final double SQR_LEN = 0.5772;
    public static final double DIMENSIONS = 3;
    private final int resolution;
    private final double pixelSize;

    public BoxesScore(int theResolution) {
        this.resolution = theResolution;
        this.pixelSize = EDGE_LEN / theResolution;
    }

    @Override
    public double calculateScore(MLMethod phenotype) {
        BoxTrialCase test = new BoxTrialCase(new Random());
        TrialEvaluation eval = new TrialEvaluation(phenotype, test);

        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 25; j++) {
                IntPair targetPos = eval.getTest().initTestCase(i);
                IntPair actualPos = eval.query(this.resolution);

                eval.accumulate(
                        calcRealDistanceSquared(targetPos, actualPos),
                        Math.max(
                                0.0,
                                eval.getMaxActivation()
                                        - eval.getMinActivation()));
            }
        }

        return eval.calculateFitness();
    }

    private double calcRealDistanceSquared(IntPair a, IntPair b) {
        double xdelta = (a.getX() - b.getX()) * this.pixelSize;
        double ydelta = (a.getY() - b.getY()) * this.pixelSize;
        return xdelta * xdelta + ydelta * ydelta;
    }

    @Override
    public boolean shouldMinimize() {
        return false;
    }

    @Override
    public boolean requireSingleThreaded() {
        return false;
    }

}
