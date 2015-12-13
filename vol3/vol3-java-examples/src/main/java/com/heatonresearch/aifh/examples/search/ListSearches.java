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
package com.heatonresearch.aifh.examples.search;

import com.heatonresearch.aifh.selection.GridModelSelection;
import com.heatonresearch.aifh.selection.RandomModelSelection;

import java.util.Arrays;

public class ListSearches {
    public static void main(String[] args) {

        System.out.println("Grid Search");
        GridModelSelection grid = new GridModelSelection();
        grid.addCategoryAxis(new String[] {"sigmoid", "ReLU", "tanh"});
        grid.addNumericAxis(1,10,1);
        Object[] list;

        while( (list=grid.next()) != null )
        {
            System.out.println(Arrays.toString(list));
        }

        System.out.println("Random Search");

        RandomModelSelection rnd = new RandomModelSelection();
        rnd.addCategoryAxis(new String[] {"sigmoid", "ReLU", "tanh"});
        rnd.addNumericAxis(1,10,1);

        for(int i=0;i<10;i++) {
            System.out.println(Arrays.toString(rnd.next()));
        }

    }
}
