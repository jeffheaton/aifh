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
package com.heatonresearch.aifh.examples.ca.mergelife.viewer;

import com.heatonresearch.aifh.examples.ca.mergelife.physics.Physics;
import com.heatonresearch.aifh.examples.ca.mergelife.universe.AdvanceTask;
import com.heatonresearch.aifh.examples.ca.mergelife.universe.UniverseRunner;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Display panel for a universe.
 */
public class DisplayPanel extends JPanel implements MouseListener {

    /**
     * The source for a universe copy.
     */
    private UniverseRunner copySource;

    /**
     * The first parent for a crossover.
     */
    private UniverseRunner crossoverParent1;

    /**
     * The second parent for a crossover.
     */
    private UniverseRunner crossoverParent2;

    /**
     * The multiverse grid.
     */
    private final UniversePane[][] grid;

    /**
     * A graphics device for off-screen rendering.
     */
    private final Graphics offscreenGraphics;

    /**
     * The offscreen image, this reduces flicker.
     */
    private final BufferedImage offscreenImage;

    /**
     * The parent frame.
     */
    private final MultiverseViewer owner;

    /**
     * The number of rows.
     */
    private final int rows;

    /**
     * The number of columns.
     */
    private final int cols;

    /**
     * The thread pool.
     */
    private final ExecutorService threadPool;

    /**
     * Random number generator.
     */
    private GenerateRandom rnd = new MersenneTwisterGenerateRandom();

    /**
     * The constructor.
     *
     * @param theOwner The parent frame.
     * @param theRows  The number of rows.
     * @param theCols  The number of columns.
     */
    public DisplayPanel(final MultiverseViewer theOwner, final int theRows,
                        final int theCols) {
        this.rows = theRows;
        this.cols = theCols;
        this.owner = theOwner;
        this.grid = new UniversePane[this.rows][this.cols];
        this.threadPool = Executors.newFixedThreadPool(8);

        for (int row = 0; row < this.rows; row++) {
            for (int col = 0; col < this.cols; col++) {
                this.grid[row][col] = new UniversePane();
            }
        }

        // create offscreen drawing buffer
        this.offscreenImage = new BufferedImage(this.cols
                * MultiverseViewer.getConfig().getPaneWidth(), this.rows
                * MultiverseViewer.getConfig().getPaneHeight(),
                BufferedImage.TYPE_INT_ARGB);

        this.offscreenGraphics = this.offscreenImage.createGraphics();

        addMouseListener(this);
    }

    /**
     * Begin a copy.
     *
     * @param row The multiverse row.
     * @param col The multiverse column.
     */
    public void copyPane(final int row, final int col) {
        this.copySource = this.grid[row][col].getUniverseRunner();
        this.crossoverParent1 = null;
        this.crossoverParent2 = null;
        this.owner.enableDeselect();
    }

    /**
     * Identify the first parent for a crossover.
     *
     * @param row The multiverse row.
     * @param col The multiverse column.
     */
    public void crossover(final int row, final int col) {
        this.crossoverParent1 = this.grid[row][col].getUniverseRunner();
        this.crossoverParent2 = null;
        this.copySource = null;
        this.owner.enableDeselect();

    }

    /**
     * Deselect any selected universes.
     */
    public void deselect() {
        this.copySource = null;
        this.crossoverParent1 = this.crossoverParent2 = null;
    }

    /**
     * Show the popup menu.
     *
     * @param e The mouse event.
     */
    private void doPop(final MouseEvent e) {
        final int x = e.getX();
        final int y = e.getY();
        final int row = y / MultiverseViewer.getConfig().getPaneHeight();
        final int col = x / MultiverseViewer.getConfig().getPaneWidth();
        final PanePopup menu = new PanePopup(this, row, col);
        menu.show(e.getComponent(), e.getX(), e.getY());
    }

    /**
     * Draw the status for a universe.
     *
     * @param g   Graphics device.
     * @param row The multiverse row.
     * @param col The multiverse column.
     * @param fm  The font metrics.
     * @param s The text.
     */
    private void drawStatus(final Graphics g, final int row, final int col,
                            final FontMetrics fm, final String s) {
        final int x = col * MultiverseViewer.getConfig().getPaneWidth();
        final int y = row * MultiverseViewer.getConfig().getPaneHeight();
        final int textY = y + MultiverseViewer.getConfig().getPaneHeight();
        g.setColor(Color.LIGHT_GRAY);
        g.fillRect(x, textY - fm.getHeight(), MultiverseViewer.getConfig()
                .getPaneWidth(), fm.getHeight());
        g.setColor(Color.BLACK);
        g.drawString(s, x, textY);
    }

    /**
     * Load a universe.
     *
     * @param row The multiverse row.
     * @param col The multiverse column.
     */
    public synchronized void load(final int row, final int col) {
        try {
            final JFileChooser fc = new JFileChooser();
            fc.setCurrentDirectory(MultiverseViewer.getConfig()
                    .getSaveDirectory());
            final int returnVal = fc.showOpenDialog(this);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                MultiverseViewer.getConfig().setSaveDirectory(
                        fc.getSelectedFile().getParentFile());
                this.grid[row][col].getUniverseRunner().getPhysics()
                        .load(fc.getSelectedFile().toString());
                this.grid[row][col].getUniverseRunner().randomize(this.rnd);
            }
        } catch (final IOException ex) {
            ex.printStackTrace();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseClicked(final MouseEvent e) {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseEntered(final MouseEvent e) {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseExited(final MouseEvent e) {

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public synchronized void mousePressed(final MouseEvent e) {
        if (e.isPopupTrigger()) {
            doPop(e);
        } else {
            final int x = e.getX();
            final int y = e.getY();
            final int row = y / MultiverseViewer.getConfig().getPaneHeight();
            final int col = x / MultiverseViewer.getConfig().getPaneWidth();

            if (this.copySource != null) {
                final UniverseRunner target = this.grid[row][col]
                        .getUniverseRunner();
                target.getPhysics().copyData(
                        this.copySource.getPhysics().getData());
                target.randomize(this.rnd);
            } else if (this.crossoverParent1 != null
                    && this.crossoverParent2 == null) {
                this.crossoverParent2 = this.grid[row][col].getUniverseRunner();
            } else  {
                final UniverseRunner target = this.grid[row][col]
                        .getUniverseRunner();
                target.crossover(this.rnd, this.crossoverParent1, this.crossoverParent2);
                target.randomize(this.rnd);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void mouseReleased(final MouseEvent e) {
        if (e.isPopupTrigger()) {
            doPop(e);
        }
    }

    /**
     * Mutate the selected universe across all free multiverse cells.
     *
     * @param row The row of the universe to generate mutations from.
     * @param col The column of the universe to generate mutations from.
     */
    public synchronized void mutateAcross(final int row, final int col) {
        final Physics sourcePhysics = this.grid[row][col].getUniverseRunner()
                .getPhysics();
        for (int currentRow = 0; currentRow < this.rows; currentRow++) {
            for (int currentCol = 0; currentCol < this.cols; currentCol++) {
                if (currentRow != row || currentCol != col) {
                    this.grid[currentRow][currentCol].getUniverseRunner()
                            .mutate(this.rnd, sourcePhysics, 0.5, 0.2);
                    this.grid[currentRow][currentCol].getUniverseRunner()
                            .randomize(this.rnd);
                }
            }
        }
    }

    /**
     * Mutate a single universe.
     *
     * @param row The multiverse row.
     * @param col The multiverse column.
     */
    public synchronized void mutateSingle(final int row, final int col) {
        final UniverseRunner target = this.grid[row][col].getUniverseRunner();
        target.mutate(this.rnd, target.getPhysics(), 0.5, 0.2);
        target.randomize(this.rnd);
    }

    /**
     * Randomize a universe (both physics and state).
     *
     * @param row The multiverse row.
     * @param col The multiverse column.
     */
    public synchronized void randomize(final int row, final int col) {
        this.grid[row][col].getUniverseRunner().randomize(this.rnd);

    }

    /**
     * Randomize a universe (not physics, only state).
     *
     * @param row The multiverse row.
     * @param col The multiverse column.
     */
    public synchronized void reset(final int row, final int col) {
        this.grid[row][col].getUniverseRunner().reset(this.rnd);
    }

    /**
     * Reset all universes (state and physics).
     */
    public void resetAll() {
        for (int row = 0; row < this.rows; row++) {
            for (int col = 0; col < this.cols; col++) {
                reset(row, col);
            }
        }
    }

    /**
     * Run a single universe full screen.
     *
     * @param row The row.
     * @param col The column.
     */
    public void runSingular(final int row, final int col) {
        this.owner.performStop();
        final Physics sourcePhysics = this.grid[row][col].getUniverseRunner()
                .getPhysics();
        final SingularUniverseViewer v = new SingularUniverseViewer(
                sourcePhysics, 2);
        v.setVisible(true);
    }

    /**
     * Save a universe.
     *
     * @param row The row.
     * @param col The column.
     */
    public void save(final int row, final int col) {
        try {
            final JFileChooser fc = new JFileChooser();
            fc.setCurrentDirectory(MultiverseViewer.getConfig()
                    .getSaveDirectory());
            final int returnVal = fc.showSaveDialog(this);
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                MultiverseViewer.getConfig().setSaveDirectory(
                        fc.getSelectedFile().getParentFile());
                this.grid[row][col].getUniverseRunner().getPhysics()
                        .save(fc.getSelectedFile().toString());
            }
        } catch (final IOException ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Set, if we should auto-kill universes that "stabilize" to no movement.
     *
     * @param autoKill True, if autokill is enabled.
     */
    public void setAutoKill(final boolean autoKill) {
        for (final UniversePane[] element : this.grid) {
            for (final UniversePane anElement : element) {
                anElement.getUniverseRunner().setAutoKill(autoKill);
            }
        }
    }

    private void updateGUI() {
        final Graphics g = getGraphics();
        final FontMetrics fm = this.offscreenGraphics.getFontMetrics();

        // Loop over all universes.
        for (int row = 0; row < this.rows; row++) {
            for (int col = 0; col < this.cols; col++) {
                final int x = col
                        * MultiverseViewer.getConfig().getPaneWidth();
                final int y = row
                        * MultiverseViewer.getConfig().getPaneHeight();

                final UniversePane cell = this.grid[row][col];
                final Image img = cell.getImage();
                this.offscreenGraphics.drawImage(img, x, y, null);

                final UniverseRunner selected = this.grid[row][col]
                        .getUniverseRunner();

                // Display any selection information.
                if (this.copySource != null) {
                    if (this.copySource == selected) {
                        drawStatus(this.offscreenGraphics, row, col, fm,
                                "Source");
                    }
                } else if (this.crossoverParent1 != null
                        || this.crossoverParent2 != null) {
                    if (selected == this.crossoverParent1) {
                        drawStatus(this.offscreenGraphics, row, col, fm,
                                "Father");
                    } else if (selected == this.crossoverParent2) {
                        drawStatus(this.offscreenGraphics, row, col, fm,
                                "Mother");
                    }
                } else {
                    final String s = "diff: "
                            + cell.getUniverseRunner().getDiff() + ",age: "
                            + cell.getUniverseRunner().getIterations();
                    drawStatus(this.offscreenGraphics, row, col, fm, s);
                }
            }
        }

        g.drawImage(this.offscreenImage, 0, 0, this);
    }

    /**
     * Update the multiverse on screen.
     */
    public void update() {
        final Collection<AdvanceTask> tasks = new ArrayList<AdvanceTask>();

        for (final UniversePane[] element : this.grid) {
            for (final UniversePane anElement : element) {
                tasks.add(new AdvanceTask(anElement));
            }
        }

        try {
            this.threadPool.invokeAll(tasks);

            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    updateGUI();
                }
            });

            Thread.sleep(100);

        } catch (final InterruptedException ex) {
            ex.printStackTrace();
        }
    }

}
