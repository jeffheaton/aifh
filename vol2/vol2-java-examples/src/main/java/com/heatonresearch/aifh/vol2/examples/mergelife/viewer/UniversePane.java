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
package com.heatonresearch.aifh.vol2.examples.mergelife.viewer;

import com.heatonresearch.aifh.vol2.examples.mergelife.physics.MergePhysics;
import com.heatonresearch.aifh.vol2.examples.mergelife.physics.Physics;
import com.heatonresearch.aifh.vol2.examples.mergelife.universe.Universe;
import com.heatonresearch.aifh.vol2.examples.mergelife.universe.UniverseRunner;
import com.heatonresearch.aifh.vol2.examples.mergelife.universe.UniverseVisualizer;

import java.awt.*;

public class UniversePane {
    private Image image;
    private final UniverseRunner universeRunner;
    private final UniverseVisualizer visualizer;

    public UniversePane() {
        final int width = MultiverseViewer.getConfig().getPaneWidth()
                / MultiverseViewer.getConfig().getZoom();
        final int height = MultiverseViewer.getConfig().getPaneHeight()
                / MultiverseViewer.getConfig().getZoom();

        final Universe universe = new Universe(height, width, 3);
        final Physics physics = new MergePhysics(universe);

        universe.randomize();
        physics.randomize();

        this.universeRunner = new UniverseRunner(universe, physics);
        this.visualizer = new UniverseVisualizer(universe,
                MultiverseViewer.getConfig().getZoom());
    }

    public void advance() {
        this.universeRunner.advance();
    }

    public Image getImage() {
        return this.image;
    }

    public UniverseRunner getUniverseRunner() {
        return this.universeRunner;
    }

    public void visualize() {
        this.image = this.visualizer.visualize();
    }
}
