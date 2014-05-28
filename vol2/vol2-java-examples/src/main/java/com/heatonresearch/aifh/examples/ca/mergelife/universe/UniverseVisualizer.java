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
package com.heatonresearch.aifh.examples.ca.mergelife.universe;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;

/**
 * Visualize the universe.
 */
public class UniverseVisualizer {
    /**
     * The image.
     */
    private final BufferedImage image;
    /**
     * The raster.
     */
    private final WritableRaster raster;
    /**
     * The universe.
     */
    private final Universe universe;
    /**
     * The zoom.
     */
    private final int zoom;

    /**
     * The constructor.
     *
     * @param theUniverse The universe.
     * @param theZoom     The zoom factor.
     */
    public UniverseVisualizer(final Universe theUniverse, final int theZoom) {
        this.universe = theUniverse;
        final int width = this.universe.getWidth();
        final int height = this.universe.getHeight();

        this.image = new BufferedImage(width * theZoom, height * theZoom,
                BufferedImage.TYPE_INT_RGB);
        this.zoom = theZoom;
        this.raster = this.image.getRaster();
    }

    /**
     * Create the image.
     *
     * @param pixels The pixels.
     * @param width  The width.
     * @param height The height.
     * @return The image.
     */
    private Image createImage(final int[] pixels, final int width,
                              final int height) {
        this.raster.setPixels(0, 0, this.image.getWidth(),
                this.image.getHeight(), pixels);
        return this.image;
    }

    /**
     * @return The universe rendered to an image.
     */
    public Image visualize() {
        final int width = this.universe.getWidth();
        final int height = this.universe.getHeight();
        final int imageSize = width * height;

        final int[] pixels = new int[imageSize * this.zoom * this.zoom * 3];
        final int rowSize = width * 3 * this.zoom;

        for (int row = 0; row < height; row++) {
            for (int col = 0; col < width; col++) {
                for (int i = 0; i < 3; i++) {
                    final double d = (this.universe.get(row, col, i) + 1.0) / 2.0;
                    for (int y = 0; y < this.zoom; y++) {
                        for (int x = 0; x < this.zoom; x++) {
                            int idx = (row * this.zoom + y) * rowSize
                                    + (col * this.zoom + x) * 3;
                            pixels[idx + i] = (int) (d * 255.0);
                        }
                    }
                }
            }
        }

        return createImage(pixels, width * this.zoom, height * this.zoom);

    }
}
