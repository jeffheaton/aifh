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
package com.heatonresearch.aifh.examples.classic.hopfield;

import com.heatonresearch.aifh.energetic.HopfieldNetwork;
import com.heatonresearch.aifh.energetic.TrainHopfieldHebbian;

/**
 * Simple class to recognize some patterns with a Hopfield Neural Network.
 * This version makes use of the storkey training.
 *
 *
 * This is very loosely based on a an example by Karsten Kutza, 
 * written in C on 1996-01-30. (link below is no longer active)
 * http://www.neural-networks-at-your-fingertips.com/hopfield.html
 *
 * I translated it to Java and adapted it to use Encog for neural
 * network processing.  I mainly kept the patterns from the 
 * original example.
 *
 */
public class HopfieldAssociateHebbian {

    public static final int HEIGHT = 10;
    public static final int WIDTH = 10;

    /**
     * The neural network will learn these patterns.
     */
    public static final String[][] PATTERN  = { {
            "O O O O O ",
            " O O O O O",
            "O O O O O ",
            " O O O O O",
            "O O O O O ",
            " O O O O O",
            "O O O O O ",
            " O O O O O",
            "O O O O O ",
            " O O O O O"  },

            { "OO  OO  OO",
                    "OO  OO  OO",
                    "  OO  OO  ",
                    "  OO  OO  ",
                    "OO  OO  OO",
                    "OO  OO  OO",
                    "  OO  OO  ",
                    "  OO  OO  ",
                    "OO  OO  OO",
                    "OO  OO  OO"  },

            { "OOOOO     ",
                    "OOOOO     ",
                    "OOOOO     ",
                    "OOOOO     ",
                    "OOOOO     ",
                    "     OOOOO",
                    "     OOOOO",
                    "     OOOOO",
                    "     OOOOO",
                    "     OOOOO"  },

            { "O  O  O  O",
                    " O  O  O  ",
                    "  O  O  O ",
                    "O  O  O  O",
                    " O  O  O  ",
                    "  O  O  O ",
                    "O  O  O  O",
                    " O  O  O  ",
                    "  O  O  O ",
                    "O  O  O  O"  },

            { "OOOOOOOOOO",
                    "O        O",
                    "O OOOOOO O",
                    "O O    O O",
                    "O O OO O O",
                    "O O OO O O",
                    "O O    O O",
                    "O OOOOOO O",
                    "O        O",
                    "OOOOOOOOOO"  } };

    /**
     * The neural network will be tested on these patterns, to see
     * which of the last set they are the closest to.
     */
    public static final String[][] PATTERN2 = { {
            "          ",
            "          ",
            "          ",
            "          ",
            "          ",
            " O O O O O",
            "O O O O O ",
            " O O O O O",
            "O O O O O ",
            " O O O O O"  },

            { "OOO O    O",
                    " O  OOO OO",
                    "  O O OO O",
                    " OOO   O  ",
                    "OO  O  OOO",
                    " O OOO   O",
                    "O OO  O  O",
                    "   O OOO  ",
                    "OO OOO  O ",
                    " O  O  OOO"  },

            { "OOOOO     ",
                    "O   O OOO ",
                    "O   O OOO ",
                    "O   O OOO ",
                    "OOOOO     ",
                    "     OOOOO",
                    " OOO O   O",
                    " OOO O   O",
                    " OOO O   O",
                    "     OOOOO"  },

            { "O  OOOO  O",
                    "OO  OOOO  ",
                    "OOO  OOOO ",
                    "OOOO  OOOO",
                    " OOOO  OOO",
                    "  OOOO  OO",
                    "O  OOOO  O",
                    "OO  OOOO  ",
                    "OOO  OOOO ",
                    "OOOO  OOOO"  },

            { "OOOOOOOOOO",
                    "O        O",
                    "O        O",
                    "O        O",
                    "O   OO   O",
                    "O   OO   O",
                    "O        O",
                    "O        O",
                    "O        O",
                    "OOOOOOOOOO"  } };

    public static double[] convertPattern(String[][] data, int index)
    {
        int resultIndex = 0;
        double[] result = new double[WIDTH*HEIGHT];
        for(int row=0;row<HEIGHT;row++)
        {
            for(int col=0;col<WIDTH;col++)
            {
                char ch = data[index][row].charAt(col);
                result[resultIndex++] = (ch!=' ')?1:-1;
            }
        }
        return result;
    }

    public static void display(double[] pattern1,double[] pattern2)
    {
        int index1 = 0;
        int index2 = 0;

        for(int row = 0;row<HEIGHT;row++)
        {
            StringBuilder line = new StringBuilder();

            for(int col = 0;col<WIDTH;col++)
            {
                if(pattern1[index1++]>0)
                    line.append('O');
                else
                    line.append(' ');
            }

            line.append("   ->   ");

            for(int col = 0;col<WIDTH;col++)
            {
                if(pattern2[index2++]>0)
                    line.append('O');
                else
                    line.append(' ');
            }

            System.out.println(line.toString());
        }
    }


    public static void evaluate(HopfieldNetwork hopfieldLogic, String[][] pattern)
    {
        for(int i=0;i<pattern.length;i++)
        {
            double[] pattern1 = convertPattern(pattern,i);
            hopfieldLogic.setCurrentState(pattern1);
            int cycles = hopfieldLogic.runUntilStable(100);
            double[] pattern2 = hopfieldLogic.getCurrentState();
            System.out.println("Cycles until stable(max 100): " + cycles + ", result=");
            display( pattern1, pattern2);
            System.out.println("----------------------");
        }
    }

    public void run()
    {
        HopfieldNetwork hopfieldLogic = new HopfieldNetwork(WIDTH*HEIGHT);
        TrainHopfieldHebbian train = new TrainHopfieldHebbian(hopfieldLogic);

        for(int i=0;i<PATTERN.length;i++)
        {
            train.addPattern(convertPattern(PATTERN, i));
        }
        train.learn();

        evaluate(hopfieldLogic,PATTERN);
        evaluate(hopfieldLogic,PATTERN2);
    }

    public static void main(String[] args)
    {
        HopfieldAssociateHebbian program = new HopfieldAssociateHebbian();
        program.run();
    }

}
