/*
 * Encog(tm) Java Examples v3.3
 * http://www.heatonresearch.com/encog/
 * https://github.com/encog/encog-java-examples
 *
 * Copyright 2008-2014 Heaton Research, Inc.
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

import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;

public class MapPanel extends JPanel {

    /**
     *
     */
    private static final long serialVersionUID = 7528474872067939033L;
    public static final int CELL_SIZE = 8;
    public static final int WIDTH = 50;
    public static final int HEIGHT = 50;

    private Matrix weights;

    public MapPanel(SomColors som)
    {
        this.weights = som.getNetwork().getWeights();
    }

    private int convertColor(double d)
    {
        //System.out.println(d);
        double result = 128*d;
        result+=128;
        result = Math.min(result, 255);
        result = Math.max(result, 0);
        return (int)result;
    }

    @Override
    public void paint(Graphics g)
    {
        for(int y = 0; y< HEIGHT; y++)
        {
            for(int x = 0; x< WIDTH; x++)
            {
                int index = (y*WIDTH)+x;
                int red = convertColor(weights.get(index,0));
                int green = convertColor(weights.get(index,1));
                int blue = convertColor(weights.get(index,2));
                g.setColor(new Color(red,green,blue));
                g.fillRect(x*CELL_SIZE, y*CELL_SIZE, CELL_SIZE, CELL_SIZE);
            }
        }
    }
}
