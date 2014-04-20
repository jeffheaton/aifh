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
package com.heatonresearch.aifh.examples.alife.mergelife.viewer;

import com.heatonresearch.aifh.examples.alife.mergelife.physics.MergePhysics;
import com.heatonresearch.aifh.examples.alife.mergelife.physics.Physics;
import com.heatonresearch.aifh.examples.alife.mergelife.universe.Universe;
import com.heatonresearch.aifh.examples.alife.mergelife.universe.UniverseRunner;
import com.heatonresearch.aifh.examples.alife.mergelife.universe.UniverseVisualizer;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

public class SingularUniverseViewer extends JFrame implements
        ComponentListener, Runnable, WindowListener {
    private UniverseRunner runner;
    private boolean running = false;
    private final double[] sourceData;
    private boolean stopRunning;
    private UniverseVisualizer visual;
    private final int zoom;

    public SingularUniverseViewer(final Physics thePhysics, final int theZoom) {
        this.sourceData = thePhysics.getData().clone();
        this.zoom = theZoom;
        setTitle("Multiverse");
        setSize(1024, 768);
        addComponentListener(this);
        addWindowListener(this);
    }

    @Override
    public void componentHidden(final ComponentEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void componentMoved(final ComponentEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void componentResized(final ComponentEvent e) {
        final int width = getWidth() / this.zoom;
        final int height = getHeight() / this.zoom;

        final Universe universe = new Universe(height, width, 3);
        final Physics physics = new MergePhysics(universe);
        physics.copyData(this.sourceData);
        universe.randomize();
        this.runner = new UniverseRunner(universe, physics);
        this.visual = new UniverseVisualizer(universe, this.zoom);

        if (!this.running) {
            this.running = true;
            final Thread t = new Thread(this);
            t.start();
        }
    }

    @Override
    public void componentShown(final ComponentEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void run() {
        final Graphics g = getGraphics();

        this.stopRunning = false;
        while (!this.stopRunning) {
            this.runner.advance();
            final Image image = this.visual.visualize();
            g.drawImage(image, 0, 0, null);
        }
    }

    @Override
    public void windowActivated(final WindowEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void windowClosed(final WindowEvent e) {
        this.stopRunning = true;

    }

    @Override
    public void windowClosing(final WindowEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void windowDeactivated(final WindowEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void windowDeiconified(final WindowEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void windowIconified(final WindowEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void windowOpened(final WindowEvent e) {
        // TODO Auto-generated method stub

    }
}
