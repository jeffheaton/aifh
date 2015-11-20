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
package com.heatonresearch.aifh.examples.classic.hopfield;

import com.heatonresearch.aifh.energetic.HopfieldNetwork;
import com.heatonresearch.aifh.energetic.TrainHopfieldStorkey;

/**
 * Simple class to recognize some patterns with a Hopfield Neural Network.
 * This is very loosely based on a an example by Karsten Kutza, 
 * written in C on 1996-01-30.
 * http://www.neural-networks-at-your-fingertips.com/hopfield.html
 *
 * I translated it to Java and adapted it to use Encog for neural
 * network processing.  I mainly kept the patterns from the 
 * original example.
 *
 */
public class HopfieldAssociateStorkey {


    public void run()
    {
        HopfieldNetwork hopfieldLogic = new HopfieldNetwork(HopfieldAssociateHebbian.WIDTH
                *HopfieldAssociateHebbian.HEIGHT);
        TrainHopfieldStorkey train = new TrainHopfieldStorkey(hopfieldLogic);

        for(int i=0;i<HopfieldAssociateHebbian.PATTERN.length;i++)
        {
            train.addPattern(HopfieldAssociateHebbian.convertPattern(HopfieldAssociateHebbian.PATTERN, i));
        }

        HopfieldAssociateHebbian.evaluate(hopfieldLogic,HopfieldAssociateHebbian.PATTERN);
        HopfieldAssociateHebbian.evaluate(hopfieldLogic,HopfieldAssociateHebbian.PATTERN2);
    }

    public static void main(String[] args)
    {
        HopfieldAssociateStorkey program = new HopfieldAssociateStorkey();
        program.run();
    }
}
