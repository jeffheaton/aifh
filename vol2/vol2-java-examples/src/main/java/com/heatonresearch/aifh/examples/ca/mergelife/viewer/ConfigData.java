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

import java.io.File;
import java.io.Serializable;

/**
 * Configuration data for the dialog.
 */
public class ConfigData implements Serializable {
    /**
     * The version.
     */
    private static final long serialVersionUID = 2L;

    /**
     * The universe height.
     */
    private int paneHeight = 200;

    /**
     * The universe width.
     */
    private int paneWidth = 200;

    /**
     * The save directory.
     */
    private File saveDirectory = new File(".");

    /**
     * How many universes across.
     */
    private int universePaneColumns = 7;

    /**
     * How many universes down.
     */
    private int universePaneRows = 4;

    /**
     * The zoom factor.
     */
    private int zoom = 1;

    /**
     * @return the paneHeight
     */
    public int getPaneHeight() {
        return this.paneHeight;
    }

    /**
     * @return the paneWidth
     */
    public int getPaneWidth() {
        return this.paneWidth;
    }

    /**
     * @return the saveDirectory
     */
    public File getSaveDirectory() {
        return this.saveDirectory;
    }

    /**
     * @return the universePaneColumns
     */
    public int getUniversePaneColumns() {
        return this.universePaneColumns;
    }

    /**
     * @return the universePaneRows
     */
    public int getUniversePaneRows() {
        return this.universePaneRows;
    }

    /**
     * @return the zoom
     */
    public int getZoom() {
        if (this.zoom < 1) {
            this.zoom = 1;
        }
        return this.zoom;
    }

    /**
     * @param paneHeight the paneHeight to set
     */
    public void setPaneHeight(final int paneHeight) {
        this.paneHeight = paneHeight;
    }

    /**
     * @param paneWidth the paneWidth to set
     */
    public void setPaneWidth(final int paneWidth) {
        this.paneWidth = paneWidth;
    }

    /**
     * @param saveDirectory the saveDirectory to set
     */
    public void setSaveDirectory(final File saveDirectory) {
        this.saveDirectory = saveDirectory;
    }

    /**
     * @param universePaneColumns the universePaneColumns to set
     */
    public void setUniversePaneColumns(final int universePaneColumns) {
        this.universePaneColumns = universePaneColumns;
    }

    /**
     * @param universePaneRows the universePaneRows to set
     */
    public void setUniversePaneRows(final int universePaneRows) {
        this.universePaneRows = universePaneRows;
    }

    /**
     * @param zoom the zoom to set
     */
    public void setZoom(final int zoom) {
        this.zoom = zoom;
    }
}
