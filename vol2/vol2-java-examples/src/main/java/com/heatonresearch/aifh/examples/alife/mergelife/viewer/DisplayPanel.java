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

import com.heatonresearch.aifh.examples.alife.mergelife.physics.Physics;
import com.heatonresearch.aifh.examples.alife.mergelife.universe.AdvanceTask;
import com.heatonresearch.aifh.examples.alife.mergelife.universe.UniverseRunner;

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

public class DisplayPanel extends JPanel implements MouseListener {

    private UniverseRunner copySource;
    private UniverseRunner crossoverParent1;
    private UniverseRunner crossoverParent2;
    private final UniversePane[][] grid;
    private final Graphics offscreenGraphics;
    private final BufferedImage offscreenImage;
    private final MultiverseViewer owner;
    private final int rows, cols;
    private final ExecutorService threadPool;

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

    public void copyPane(final int row, final int col) {
        this.copySource = this.grid[row][col].getUniverseRunner();
        this.crossoverParent1 = null;
        this.crossoverParent2 = null;
        this.owner.enableDeselect();
    }

    public void crossover(final int row, final int col) {
        this.crossoverParent1 = this.grid[row][col].getUniverseRunner();
        this.crossoverParent2 = null;
        this.copySource = null;
        this.owner.enableDeselect();

    }

    public void deselect() {
        this.copySource = null;
        this.crossoverParent1 = this.crossoverParent2 = null;
    }

    private void doPop(final MouseEvent e) {
        final int x = e.getX();
        final int y = e.getY();
        final int row = y / MultiverseViewer.getConfig().getPaneHeight();
        final int col = x / MultiverseViewer.getConfig().getPaneWidth();
        final PanePopup menu = new PanePopup(this, row, col);
        menu.show(e.getComponent(), e.getX(), e.getY());
    }

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

    public void load(final int row, final int col) {
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
                this.grid[row][col].getUniverseRunner().randomize();
            }
        } catch (final IOException ex) {
            ex.printStackTrace();
        }
    }

    @Override
    public void mouseClicked(final MouseEvent e) {

    }

    @Override
    public void mouseEntered(final MouseEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void mouseExited(final MouseEvent e) {
        // TODO Auto-generated method stub

    }

    @Override
    public void mousePressed(final MouseEvent e) {
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
                target.randomize();
            } else if (this.crossoverParent1 != null
                    && this.crossoverParent2 == null) {
                this.crossoverParent2 = this.grid[row][col].getUniverseRunner();
            } else if (this.crossoverParent1 != null
                    && this.crossoverParent2 != null) {
                final UniverseRunner target = this.grid[row][col]
                        .getUniverseRunner();
                target.crossover(this.crossoverParent1, this.crossoverParent2);
                target.randomize();
            }
        }
    }

    @Override
    public void mouseReleased(final MouseEvent e) {
        if (e.isPopupTrigger()) {
            doPop(e);
        }
    }

    public void mutateAcross(final int row, final int col) {
        final Physics sourcePhysics = this.grid[row][col].getUniverseRunner()
                .getPhysics();
        for (int currentRow = 0; currentRow < this.rows; currentRow++) {
            for (int currentCol = 0; currentCol < this.cols; currentCol++) {
                if (currentRow != row || currentCol != col) {
                    this.grid[currentRow][currentCol].getUniverseRunner()
                            .mutate(sourcePhysics, 0.5, 0.2);
                    this.grid[currentRow][currentCol].getUniverseRunner()
                            .randomize();
                }
            }
        }
    }

    public void mutateSingle(final int row, final int col) {
        final UniverseRunner target = this.grid[row][col].getUniverseRunner();
        target.mutate(target.getPhysics(), 0.5, 0.2);
        target.randomize();
    }

    public void randomize(final int row, final int col) {
        this.grid[row][col].getUniverseRunner().randomize();

    }

    public void reset(final int row, final int col) {
        this.grid[row][col].getUniverseRunner().reset();
    }

    public void resetAll() {
        for (int row = 0; row < this.rows; row++) {
            for (int col = 0; col < this.cols; col++) {
                reset(row, col);
            }
        }
    }

    public void runSingular(final int row, final int col) {
        this.owner.performStop();
        final Physics sourcePhysics = this.grid[row][col].getUniverseRunner()
                .getPhysics();
        final SingularUniverseViewer v = new SingularUniverseViewer(
                sourcePhysics, 2);
        v.setVisible(true);
    }

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

    public void setAutoKill(final boolean autoKill) {
        for (final UniversePane[] element : this.grid) {
            for (final UniversePane anElement : element) {
                anElement.getUniverseRunner().setAutoKill(autoKill);
            }
        }
    }

    public void update() {
        final Collection<AdvanceTask> tasks = new ArrayList<AdvanceTask>();

        for (final UniversePane[] element : this.grid) {
            for (final UniversePane anElement : element) {
                tasks.add(new AdvanceTask(anElement));
            }
        }

        try {
            this.threadPool.invokeAll(tasks);

            final Graphics g = getGraphics();
            final FontMetrics fm = this.offscreenGraphics.getFontMetrics();

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
        } catch (final InterruptedException ex) {
            ex.printStackTrace();
        }
    }

}
