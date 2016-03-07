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
package com.heatonresearch.aifh.examples.classic.som.colors;

import Jama.Matrix;
import com.heatonresearch.aifh.som.SelfOrganizingMap;

import javax.swing.*;
import java.awt.*;

public class MapPanel extends JPanel {
    private final SelfOrganizingMap network;
    private final int width;
    private final int height;
    private final int cellSize;
    private boolean displayNumbers;

    public MapPanel(SelfOrganizingMap theNetwork, int theCellSize, int theWidth, int theHeight)
    {
        this.network = theNetwork;
        this.width = theWidth;
        this.height = theHeight;
        this.cellSize = theCellSize;
    }

    private int convertColor(double d)
    {
        double result = 128*d;
        result+=128;
        result = Math.min(result, 255);
        result = Math.max(result, 0);
        return (int)result;
    }

    public boolean isDisplayNumbers() {
        return this.displayNumbers;
    }

    public void setDisplayNumbers(final boolean displayNumbers) {
        this.displayNumbers = displayNumbers;
    }

    @Override
    public void paint(Graphics g)
    {
        FontMetrics fm = g.getFontMetrics();

        int idx = 0;
        Matrix weights = this.network.getWeights();
        for(int y = 0; y< this.height; y++)
        {
            for(int x = 0; x< this.width; x++)
            {
                int index = (y*this.width)+x;
                int red = convertColor(weights.get(index,0));
                int green = convertColor(weights.get(index,1));
                int blue = convertColor(weights.get(index,2));
                g.setColor(new Color(red,green,blue));
                g.fillRect(x*this.cellSize, y*this.cellSize, this.cellSize, this.cellSize);

                if( isDisplayNumbers() ) {
                    g.setColor(Color.BLACK);
                    g.drawString(""+idx,x*this.cellSize, (y*this.cellSize)+fm.getHeight());
                    idx++;
                }
            }
        }
    }
}
