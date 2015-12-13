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
package com.heatonresearch.aifh.ann.randomize;

import com.heatonresearch.aifh.ann.BasicNetwork;

/**
 * Created by jeff on 11/28/15.
 */
public class XaiverRandomizeNetwork extends AbstractRandomizeNetwork {

    private void randomizeLayer(BasicNetwork network, int fromLayer) {
        final int fromCount = network.getLayerTotalNeuronCount(fromLayer);
        final int toCount = network.getLayers().get(fromLayer + 1).getCount();

        for (int fromNeuron = 0; fromNeuron < fromCount; fromNeuron++) {
            for (int toNeuron = 0; toNeuron < toCount; toNeuron++) {
                double sigma = Math.sqrt(2.0/(fromCount+toCount));
                double w = this.getRnd().nextGaussian() * sigma;
                network.setWeight(fromLayer, fromNeuron, toNeuron, w);
            }
        }
    }

    @Override
    public void randomize(BasicNetwork network) {
        for (int i = 0; i < network.getLayers().size() - 1; i++) {
            randomizeLayer(network, i);
        }
    }
}
