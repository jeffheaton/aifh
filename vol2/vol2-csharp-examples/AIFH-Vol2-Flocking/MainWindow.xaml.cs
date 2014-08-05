// Artificial Intelligence for Humans
// Volume 2: Nature-Inspired Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2014 by Jeff Heaton
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
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
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Threading;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;
using AIFH_Vol2.Core.Distance;
using AIFH_Vol2.Core.Randomize;

namespace AIFH_Vol2_Flocking
{
    /// <summary>
    ///     Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow
    {
        /// <summary>
        /// The number of particles.
        /// </summary>
        private const int ParticleCount = 100;

        /// <summary>
        /// The size of each particle.
        /// </summary>
        private const double ParticleSize = 10;

        /// <summary>
        /// Distance calculation.
        /// </summary>
        private readonly ICalculateDistance _distanceCalc = new EuclideanDistance();

        /// <summary>
        /// The particles.
        /// </summary>
        private readonly IList<Particle> _particles = new List<Particle>();
        private volatile bool _shouldStop;


        /// <summary>
        /// The constant for alignment.
        /// </summary>
        private const double ConstAlignment = 0.5;

        /// <summary>
        /// The constant for cohesion.
        /// </summary>
        private const double ConstCohesion = 0.01;

        /// <summary>
        /// The constant for separation.
        /// </summary>
        private const double ConstSeparation = 0.25;


        public MainWindow()
        {
            InitializeComponent();
        }

        /// <summary>
        /// Find the index that has the max value in a vector.
        /// </summary>
        /// <param name="data">The vector.</param>
        /// <returns>The index.</returns>
        public static int MaxIndex(double[] data)
        {
            int result = -1;
            for (int i = 0; i < data.Length; i++)
            {
                if (result == -1 || data[i] > data[result])
                {
                    result = i;
                }
            }
            return result;
        }

        /// <summary>
        /// Get the mean particle location for the specified dimension.
        /// </summary>
        /// <param name="particles">The particles.</param>
        /// <param name="dimension">The dimension.</param>
        /// <returns>The mean.</returns>
        public static double ParticleLocationMean(IList<Particle> particles, int dimension)
        {
            double sum = 0;
            int count = 0;
            foreach (Particle p in particles)
            {
                sum += p.Location[dimension];
                count++;
            }
            return sum/count;
        }

        
        /// <summary>
        /// Get the particle velocity mean for the specified dimension.
        /// </summary>
        /// <param name="particles">The particles.</param>
        /// <param name="dimension">The dimension.</param>
        /// <returns>The velocity mean.</returns>
        public static double ParticleVelocityMean(IList<Particle> particles, int dimension)
        {
            double sum = 0;
            int count = 0;
            foreach (Particle p in particles)
            {
                sum += p.Velocity[dimension];
                count++;
            }
            return sum/count;
        }

        /// <summary>
        /// Find the nearest neighbor particle.
        /// </summary>
        /// <param name="target">The particle to look for neighbors to.</param>
        /// <param name="particles">All particles.</param>
        /// <param name="k">The number of particles to find.</param>
        /// <param name="maxDist">The max distance to check.</param>
        /// <returns>The nearest neighbors.</returns>
        private IList<Particle> FindNearest(Particle target, IEnumerable<Particle> particles, int k, double maxDist)
        {
            IList<Particle> result = new List<Particle>();
            var tempDist = new double[k];
            int worstIndex = -1;

            foreach (Particle particle in particles)
            {
                if (particle == target)
                {
                    continue;
                }
                double d = _distanceCalc.Calculate(particle.Location, target.Location);

                if (d > maxDist)
                {
                    continue;
                }

                if (result.Count < k)
                {
                    tempDist[result.Count] = d;
                    result.Add(particle);
                    worstIndex = MaxIndex(tempDist);
                }
                else if (d < tempDist[worstIndex])
                {
                    tempDist[worstIndex] = d;
                    result[worstIndex] = particle;
                    worstIndex = MaxIndex(tempDist);
                }
            }

            return result;
        }

        public void RandomizeParticles()
        {
            IGenerateRandom random = new MersenneTwisterGenerateRandom();

            for (int i = 0; i < ParticleCount; i++)
            {
                var p = new Particle(2);
                p.Location[0] = random.NextDouble(OutputCanvas.ActualWidth);
                p.Location[1] = random.NextDouble(OutputCanvas.ActualHeight);
                p.Velocity[0] = 3;
                p.Velocity[1] = random.NextDouble(2.0*Math.PI);
                _particles.Add(p);
            }
        }

        public void MoveParticles()
        {
            foreach (Particle p in _particles)
            {
                double r = p.Velocity[1] + (Math.PI*5.0)/12.0;

                // move the particle
                double dx = Math.Cos(r);
                double dy = Math.Sin(r);
                p.Location[0] += (dx*p.Velocity[0]);
                p.Location[1] += (dy*p.Velocity[0]);

                // handle wraps
                if (p.Location[0] < 0)
                {
                    p.Location[0] = OutputCanvas.ActualWidth;
                }
                if (p.Location[1] < 0)
                {
                    p.Location[1] = OutputCanvas.ActualHeight;
                }
                if (p.Location[0] > OutputCanvas.ActualWidth)
                {
                    p.Location[0] = 0;
                }
                if (p.Location[1] > OutputCanvas.ActualHeight)
                {
                    p.Location[1] = 0;
                }
            }
        }

        public void RenderParticles()
        {
            OutputCanvas.Children.Clear();
            var rect = new Rectangle
            {
                Fill = Brushes.Black,
                Width = OutputCanvas.ActualWidth,
                Height = OutputCanvas.ActualHeight,
                Stroke = Brushes.Black
            };
            rect.SetValue(Canvas.LeftProperty, 0.0);
            rect.SetValue(Canvas.TopProperty, 0.0);

            OutputCanvas.Children.Add(rect);

            foreach (Particle p in _particles)
            {
                // render the particles
                var x = new double[3];
                var y = new double[3];

                x[0] = (int) p.Location[0];
                y[0] = (int) p.Location[1];

                double r = p.Velocity[1] + (Math.PI*5.0)/12.0;
                x[1] = x[0] - (Math.Cos(r)*ParticleSize);
                y[1] = y[0] - (Math.Sin(r)*ParticleSize);

                double r2 = p.Velocity[1] + (Math.PI*7.0)/12.0;
                x[2] = x[0] - (Math.Cos(r2)*ParticleSize);
                y[2] = y[0] - (Math.Sin(r2)*ParticleSize);

                var poly = new Polygon();
                poly.Points.Add(new Point(x[0], y[0]));
                poly.Points.Add(new Point(x[1], y[1]));
                poly.Points.Add(new Point(x[2], y[2]));

                poly.Stroke = Brushes.White;
                poly.Fill = Brushes.White;
                OutputCanvas.Children.Add(poly);
            }
        }

        public void SteerParticles()
        {
            foreach (Particle p in _particles)
            {
                ///////////////////////////////////////////////////////////////
                // Begin implementation of three very basic laws of flocking.
                ///////////////////////////////////////////////////////////////
                IList<Particle> neighbors = FindNearest(p, _particles, 5, double.PositiveInfinity);
                IList<Particle> nearest = FindNearest(p, _particles, 5, 10);

                // 1. Separation - avoid crowding neighbors (short range repulsion)
                double separation = 0;
                if (nearest.Count > 0)
                {
                    double meanX = ParticleLocationMean(nearest, 0);
                    double meanY = ParticleLocationMean(nearest, 1);
                    double dx = meanX - p.Location[0];
                    double dy = meanY - p.Location[1];
                    separation = Math.Atan2(dx, dy) - p.Velocity[1];
                    separation += Math.PI;
                }

                // 2. Alignment - steer towards average heading of neighbors
                double alignment = 0;

                if (neighbors.Count > 0)
                {
                    alignment = ParticleVelocityMean(neighbors, 1) - p.Velocity[1];
                }

                // 3. Cohesion - steer towards average position of neighbors (long range attraction)
                double cohesion = 0;

                if (neighbors.Count > 0)
                {
                    double meanX = ParticleLocationMean(_particles, 0);
                    double meanY = ParticleLocationMean(_particles, 1);
                    double dx = meanX - p.Location[0];
                    double dy = meanY - p.Location[1];
                    cohesion = Math.Atan2(dx, dy) - p.Velocity[1];
                }

                // perform the turn
                // The degree to which each of the three laws is applied is configurable.
                // The three default ratios that I provide work well.
                double turnAmount = (cohesion*ConstCohesion) + (alignment*ConstAlignment) + (separation*ConstSeparation);

                p.Velocity[1] += turnAmount;

                ///////////////////////////////////////////////////////////////
                // End implementation of three very basic laws of flocking.
                ///////////////////////////////////////////////////////////////
            }
        }

        // This method will be called when the thread is started. 
        public void DoWork()
        {
            while (!_shouldStop)
            {
                Thread.Sleep(100);
                MoveParticles();
                SteerParticles();
                if (!Dispatcher.CheckAccess())
                {
                    Dispatcher.Invoke(RenderParticles);
                }
                else
                {
                    RenderParticles();
                }
            }
        }

        private void Window_Loaded_1(object sender, RoutedEventArgs e)
        {
            RandomizeParticles();
            var t = new Thread(DoWork);
            t.Start();
        }

        private void Window_Closing_1(object sender, CancelEventArgs e)
        {
            _shouldStop = true;
        }
    }
}
