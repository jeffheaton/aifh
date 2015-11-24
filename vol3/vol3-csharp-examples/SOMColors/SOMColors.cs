//
// Encog(tm) Examples v3.0 - .Net Version
// http://www.heatonresearch.com/encog/
//
// Copyright 2008-2011 Heaton Research, Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//  http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//   
// For more information on Heaton Research copyrights, licenses 
// and trademarks visit:
// http://www.heatonresearch.com/copyright
//
using AIFH_Vol3.Core;
using AIFH_Vol3.Core.General.Fns;
using AIFH_Vol3.Core.Randomize;
using AIFH_Vol3_Core.Core.SOM;
using AIFH_Vol3_Core.Core.SOM.Neighborhood;
using AIFH_Vol3_Core.Core.SOM.Train;
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

namespace SOMColors
{
    public partial class SOMColors : Form
    {
        public const int CELL_SIZE = 8;
        public const int WIDTH = 50;
        public const int HEIGHT = 50;

        private readonly INeighborhoodFunction gaussian;
        private readonly SelfOrganizingMap network;
        private readonly double[][] samples;
        private readonly BasicTrainSOM train;
        private int iteration;
        private IGenerateRandom rnd = new MersenneTwisterGenerateRandom();

        public SOMColors()
        {
            InitializeComponent();

            network = CreateNetwork();
            gaussian = new NeighborhoodRBF(RBFEnum.Gaussian, WIDTH, HEIGHT);
            train = new BasicTrainSOM(network, 0.01, null, gaussian);

            train.ForceWinner = false;
            samples = AIFH.Alloc2D<double>(15, 3);
            
            for (int i = 0; i < 15; i++)
            {
                samples[i][0] = rnd.NextDouble(-1, 1);
                samples[i][1] = rnd.NextDouble(-1, 1);
                samples[i][2] = rnd.NextDouble(-1, 1);
            }

            train.SetAutoDecay(100, 0.8, 0.003, 30, 5);
        }

        private SelfOrganizingMap CreateNetwork()
        {
            var result = new SelfOrganizingMap(3, WIDTH*HEIGHT);
            result.Reset();
            return result;
        }

        private void updateTimer_Tick(object sender, EventArgs e)
        {
            iteration++;
            if (iteration > 100)
                updateTimer.Enabled = false;


            var idx = (int)rnd.NextInt(samples.Length);
            var c = samples[idx];

            train.TrainPattern(c);
            train.AutoDecay();
            DrawMap();
            //System.out.println("Iteration " + i + ","+ this.train.toString());
        }

        private int ConvertColor(double d)
        {
            double result = 128*d;
            result += 128;
            result = Math.Min(result, 255);
            result = Math.Max(result, 0);
            return (int) result;
        }

        private void DrawMap()
        {
            Graphics g = CreateGraphics();
            for (int y = 0; y < HEIGHT; y++)
            {
                for (int x = 0; x < WIDTH; x++)
                {
                    int index = (y*WIDTH) + x;
                    int red = ConvertColor(network.Weights[index, 0]);
                    int green = ConvertColor(network.Weights[index, 1]);
                    int blue = ConvertColor(network.Weights[index, 2]);
                    Color c = Color.FromArgb(red, green, blue);
                    Brush brush = new SolidBrush(c);
                    g.FillRectangle(brush, x*CELL_SIZE, y*CELL_SIZE, CELL_SIZE, CELL_SIZE);
                }
            }
            g.Dispose();
        }
    }
}