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
package com.heatonresearch.aifh.selection;

import com.heatonresearch.aifh.randomize.GenerateRandom;

import java.util.ArrayList;
import java.util.List;

public class CategorySearchAxis implements SearchAxis {
    private final List<String> categories = new ArrayList<String>();
    private int currentIndex = 0;

    public CategorySearchAxis(String[] catList) {
        for(String str:catList) {
            this.categories.add(str);
        }
    }

    public List<String> getCategories() {
        return this.categories;
    }

    @Override
    public void reset() {
        this.currentIndex = 0;
    }

    @Override
    public boolean advance() {
        this.currentIndex++;
        if( this.currentIndex>=this.categories.size() ) {
            this.currentIndex = 0;
            return true;
        }
        return false;
    }

    @Override
    public Object currentState() {
        return this.categories.get(this.currentIndex);
    }

    @Override
    public Object sample(GenerateRandom rnd) {
        return this.categories.get(rnd.nextInt(this.categories.size()));
    }
}
