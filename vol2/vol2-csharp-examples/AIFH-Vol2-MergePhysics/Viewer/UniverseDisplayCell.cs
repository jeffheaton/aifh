using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
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
        /// <summary>
        ///     A random number generator.
        /// </summary>
        private readonly IGenerateRandom _rnd = new MersenneTwisterGenerateRandom();

        /// <summary>
        ///     The universe runner.
        /// </summary>
        private readonly UniverseRunner _universeRunner;

        /// <summary>
        ///     The universe rendered.
        /// </summary>
        private Rectangle[][] _grid;

        /// <summary>
        ///     The universe visualizer.
        /// </summary>
        private UniverseVisualizer _visualizer;

        /// <summary>
        ///     The constructor.
        /// </summary>
        public UniverseDisplayCell(Canvas canvas, int xPosition, int yPosition)
        {
            int width = Settings.Default.PaneWidth
                        /Settings.Default.Zoom;
            int height = Settings.Default.PaneHeight
                         /Settings.Default.Zoom;

            var universe = new UniverseHolder(height, width, 3);
            var physics = new MergePhysics(universe);

            universe.Randomize(_rnd);
            physics.Randomize();

            _universeRunner = new UniverseRunner(universe, physics);
            _visualizer = new UniverseVisualizer(universe,
                Settings.Default.Zoom);

            for (int row = 0; row < Settings.Default.PaneHeight; row++)
            {
                for (int col = 0; col < Settings.Default.PaneWidth; col++)
                {
                    int x = xPosition + row;
                    int y = yPosition + col;

                    var rect = new Rectangle
                    {
                        Fill = Brushes.Black,
                        Width = width,
                        Height = height,
                    };
                    rect.SetValue(Canvas.LeftProperty, x);
                    rect.SetValue(Canvas.TopProperty, y);
                    canvas.Children.Add(rect);
                }   
            }
        }

        /// <summary>
        ///     The universe runner.
        /// </summary>
        public UniverseRunner UniverseRunner
        {
            get { return _universeRunner; }
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
            _visualizer.Visualize(_grid);
        }
    }
}