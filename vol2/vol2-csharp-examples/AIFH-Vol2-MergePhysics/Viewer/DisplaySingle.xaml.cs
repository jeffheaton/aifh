using System;
using System.ComponentModel;
using System.Threading;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Threading;
using AIFH_Vol2_MergePhysics.Universe;

namespace AIFH_Vol2_MergePhysics.Viewer
{
    /// <summary>
    ///     Interaction logic for DisplaySingle.xaml
    /// </summary>
    public partial class DisplaySingle
    {
        private readonly double[] _physics;
        private Thread _background;
        private UniverseDisplayCell _display;

        /// <summary>
        ///     Are the universes running?
        /// </summary>
        private volatile bool _running;

        /// <summary>
        ///     Has a stop been requested?
        /// </summary>
        private bool _stopRequest;

        /// <summary>
        /// Display a single CA.
        /// </summary>
        /// <param name="universeRunner">The runner to use physics from.</param>
        public DisplaySingle(UniverseRunner universeRunner)
        {
            InitializeComponent();
            int len = universeRunner.PhysicsRules.Data.Length;
            _physics = new double[len];
            Array.Copy(universeRunner.PhysicsRules.Data, _physics, len);
        }


        /// <summary>
        /// Stop the animation.
        /// </summary>
        private void StopAnimation()
        {
            if (_running)
            {
                for (;;)
                {
                    _stopRequest = true;
                    if (_background.Join(100)) // set appropriate timeout
                    {
                        break;
                    }

                    // resolve deadlock
                    Application.Current.Dispatcher.Invoke(DispatcherPriority.Background,
                        new Action(delegate { }));
                }
            }
        }

        /// <summary>
        /// The background thread.
        /// </summary>
        private void DoWork()
        {
            _running = true;

            while (!_stopRequest)
            {
                _display.Advance();

                lock (this)
                {
                    if (!Dispatcher.CheckAccess())
                    {
                        Dispatcher.Invoke(UpdateUi);
                    }
                    else
                    {
                        UpdateUi();
                    }
                }
            }
            _stopRequest = false;
            _running = false;
        }

        /// <summary>
        /// Update the user interface.
        /// </summary>
        private void UpdateUi()
        {
            _display.Visualize();
        }

        /// <summary>
        /// The window is closing.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void Window_Closing_1(object sender, CancelEventArgs e)
        {
            StopAnimation();
        }

        /// <summary>
        /// The window changed size.
        /// </summary>
        /// <param name="sender">The sending object.</param>
        /// <param name="e">The event.</param>
        private void Window_SizeChanged_1(object sender, SizeChangedEventArgs e)
        {
            // Clear out any previous animation
            StopAnimation();
            CanvasOutput.Children.Clear();

            // Create new animation at correct size.
            _display = new UniverseDisplayCell((int) e.NewSize.Width, (int) e.NewSize.Height, null);
            _display.UniverseRunner.PhysicsRules.CopyData(_physics);
            _display.Visualize();
            _display.UniverseRunner.AutoKill = false;
            var img = new Image {Source = _display.Image};
            img.SetValue(Canvas.LeftProperty, 0.0);
            img.SetValue(Canvas.TopProperty, 0.0);
            CanvasOutput.Children.Add(img);

            // Start the thread.
            _stopRequest = false;
            _background = new Thread(DoWork);
            _background.Start();
        }
    }
}