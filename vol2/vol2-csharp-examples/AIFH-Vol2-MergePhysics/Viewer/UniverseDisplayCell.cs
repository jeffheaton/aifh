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
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using AIFH_Vol2.Core.Randomize;
using AIFH_Vol2_MergePhysics.Physics;
using AIFH_Vol2_MergePhysics.Properties;
using AIFH_Vol2_MergePhysics.Universe;

namespace AIFH_Vol2_MergePhysics.Viewer
{
    /// <summary>
    ///     A cell, holding one universe.
    /// </summary>
    public class UniverseDisplayCell
    {
        private readonly WriteableBitmap _bitmap;
        private readonly Label _label;

        /// <summary>
        ///     A random number generator.
        /// </summary>
        private readonly IGenerateRandom _rnd = new MersenneTwisterGenerateRandom();

        /// <summary>
        ///     The universe runner.
        /// </summary>
        private readonly UniverseRunner _universeRunner;

        /// <summary>
        ///     The universe visualizer.
        /// </summary>
        private readonly UniverseVisualizer _visualizer;

        /// <summary>
        ///     The constructor.
        /// </summary>
        public UniverseDisplayCell(int paneWidth, int paneHeight, Label label)
        {
            int width = paneWidth
                        /Settings.Default.Zoom;
            int height = paneHeight
                         /Settings.Default.Zoom;

            _label = label;

            var universe = new UniverseHolder(height, width, 3);
            var physics = new MergePhysics(universe);

            universe.Randomize(_rnd);
            physics.Randomize();

            _universeRunner = new UniverseRunner(universe, physics);
            _visualizer = new UniverseVisualizer(universe,
                Settings.Default.Zoom);

            _bitmap = new WriteableBitmap(
                paneWidth,
                paneHeight,
                96,
                96,
                PixelFormats.Bgr32,
                null);
        }

        /// <summary>
        ///     The universe runner.
        /// </summary>
        public UniverseRunner UniverseRunner
        {
            get { return _universeRunner; }
        }

        /// <summary>
        /// The image rendered to.
        /// </summary>
        public WriteableBitmap Image
        {
            get { return _bitmap; }
        }

        /// <summary>
        /// The text caption.
        /// </summary>
        public Label Caption
        {
            get { return _label; }
        }

        /// <summary>
        ///     Advance a frame.
        /// </summary>
        public void Advance()
        {
            _universeRunner.Advance(_rnd);
        }

        /// <summary>
        ///     Visuzlizr the universe.
        /// </summary>
        public void Visualize()
        {
            _visualizer.Visualize(_bitmap);
        }
    }
}
