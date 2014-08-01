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