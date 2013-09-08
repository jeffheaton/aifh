/*
 * Artificial Intelligence for Humans
 * Volume 1: Fundamental Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh

 * Copyright 2013 by Jeff Heaton
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

package com.heatonresearch.aifh.examples.distance;

import javax.swing.*;
import java.awt.*;

/**
 * Sample: GUI element that displays sampled data.
 */
public class Sample extends JPanel {

    /**
     * Serial id for this class.
     */
    private static final long serialVersionUID = 2250441617163548592L;
    /**
     * The image data.
     */
    SampleData data;

    /**
     * The constructor.
     *
     * @param width  The width of the downsampled image
     * @param height The height of the downsampled image
     */
    Sample(final int width, final int height) {
        this.data = new SampleData(' ', width, height);
    }

    /**
     * The image data object.
     *
     * @return The image data object.
     */
    SampleData getData() {
        return this.data;
    }

    /**
     * @param g Display the downsampled image.
     */
    @Override
    public void paint(final Graphics g) {
        if (this.data == null) {
            return;
        }

        int x, y;
        final int vcell = getHeight() / this.data.getHeight();
        final int hcell = getWidth() / this.data.getWidth();

        g.setColor(Color.white);
        g.fillRect(0, 0, getWidth(), getHeight());

        g.setColor(Color.black);
        for (y = 0; y < this.data.getHeight(); y++) {
            g.drawLine(0, y * vcell, getWidth(), y * vcell);
        }
        for (x = 0; x < this.data.getWidth(); x++) {
            g.drawLine(x * hcell, 0, x * hcell, getHeight());
        }

        for (y = 0; y < this.data.getHeight(); y++) {
            for (x = 0; x < this.data.getWidth(); x++) {
                if (this.data.getData(x, y)) {
                    g.fillRect(x * hcell, y * vcell, hcell, vcell);
                }
            }
        }

        g.setColor(Color.black);
        g.drawRect(0, 0, getWidth() - 1, getHeight() - 1);

    }

    /**
     * Assign a new image data object.
     *
     * @param data The image data object.
     */

    void setData(final SampleData data) {
        this.data = data;
    }

}
