using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;
using AIFH_Vol1.Core.Randomize;

namespace AIFH_Vol1_Random
{
    /// <summary>
    ///     Inter logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        /// <summary>
        ///     Delegate to update the status from another thread.
        /// </summary>
        /// <param name="tries">Tries attempted so far.</param>
        /// <param name="error">Current error rate.</param>
        public delegate void UpdateStatusDelegate(int tries, double error);

        public const int BarWidth = 10;

        private readonly Label _line1 = new Label();
        private readonly Label _line2 = new Label();
        private readonly Label _line3 = new Label();
        private readonly Label _line4 = new Label();
        private readonly IList<Rectangle> _barRects = new List<Rectangle>();
        private readonly int[] boxes;

        private long _evalCount;
        private int _evalRate;
        private long _evalTime;
        private long _generated;
        private double _high;
        private long _lastUpdate;
        private double _low;
        private bool _requestStop;
        private IGenerateRandom _rnd;
        private int _runtime;
        private long _started;
        private bool _uniformMode;

        public MainWindow()
        {
            InitializeComponent();
            boxes = new int[2000];
            Reset();
        }


        public void Reset()
        {
            _evalCount = 0;
            _evalTime = 0;
            _evalRate = 0;
            _lastUpdate = -1;
            _generated = 0;
            _started = -1;
            _low = double.PositiveInfinity;
            _high = double.NegativeInfinity;
            _runtime = 0;
            for (int i = 0; i < boxes.Length; i++)
            {
                boxes[i] = 0;
            }
        }

        public void ReportNumber(double d)
        {
            if (_lastUpdate == -1)
            {
                _lastUpdate = Environment.TickCount;
                _started = _lastUpdate;
            }

            long currentTime = Environment.TickCount;
            _generated++;
            _low = Math.Min(_low, d);
            _high = Math.Max(_high, d);

            var boxNumber = (int) ((d*300.0) + 1000.0);
            if (boxNumber >= 0 && boxNumber < boxes.Length)
            {
                boxes[boxNumber]++;
            }


            _evalTime = (currentTime - _started) - 5000;

            if (_evalTime > 0)
            {
                _evalCount++;
            }


            if ((currentTime - _lastUpdate) > 1000)
            {
                _runtime = (int) ((currentTime - _started)/1000);
                _lastUpdate = currentTime;
                if (_evalCount > 0)
                {
                    _evalRate = (int) (_evalCount/_evalTime);
                }
                else
                {
                    _evalRate = 0;
                }

                // Update the GUI.  This must be done on the GUI thread.
                Action a = () =>
                {
                    long gen = _generated/1000;
                    _line1.Content = "Running for: " + _runtime + " seconds";
                    _line2.Content = "Generated: " + gen.ToString("n0") + " thousand numbers";
                    _line3.Content = "Range: " + _low.ToString("0.00")
                                     + " to " + _high.ToString("0.00");
                    _line4.Content = "Rate: " + _evalRate.ToString("n0") + " nums/ms";

                    int barCount = _barRects.Count;
                    int mode = boxes.Concat(new[] {0}).Max();

                    int bar2Box;
                    int boxesIndex = 0;

                    if (_uniformMode)
                    {
                        bar2Box = 1;
                        boxesIndex = (boxes.Length/2) + bar2Box;
                    }
                    else
                    {
                        bar2Box = boxes.Length/barCount;
                    }

                    for (int i = 0; i < barCount; i++)
                    {
                        int barAmount = 0;
                        for (int j = 0; j < bar2Box; j++)
                        {
                            barAmount += boxes[boxesIndex++];
                        }
                        barAmount /= bar2Box;


                        double barRatio = (double) barAmount/mode;
                        double barHeight = barRatio*CanvasChart.ActualHeight;

                        Rectangle r = _barRects[i];
                        r.Height = barHeight;
                        Canvas.SetTop(r, CanvasChart.ActualHeight - r.Height);
                    }
                };

                lock (this)
                {
                    Dispatcher.BeginInvoke(a);
                }
            }
        }

        private void Window_Loaded_1(object sender, RoutedEventArgs e)
        {
            double y = 0.0;
            _line1.Content = "";
            _line1.SetValue(Canvas.TopProperty, 0.0);
            _line1.SetValue(Canvas.LeftProperty, y);
            CanvasChart.Children.Add(_line1);
            _line1.Measure(new Size(double.PositiveInfinity, double.PositiveInfinity));
            double textHeight = _line1.DesiredSize.Height;

            y += textHeight;
            _line2.Content = "";
            _line2.SetValue(Canvas.TopProperty, y);
            _line2.SetValue(Canvas.LeftProperty, 0.0);
            CanvasChart.Children.Add(_line2);

            y += textHeight;
            _line3.Content = "";
            _line3.SetValue(Canvas.TopProperty, y);
            _line3.SetValue(Canvas.LeftProperty, 0.0);
            CanvasChart.Children.Add(_line3);

            y += textHeight;
            _line4.Content = "";
            _line4.SetValue(Canvas.TopProperty, y);
            _line4.SetValue(Canvas.LeftProperty, 0.0);
            CanvasChart.Children.Add(_line4);
        }

        private void Canvas_SizeChanged_1(object sender, SizeChangedEventArgs e)
        {
            // remove previous set of rectangle bars, if any
            lock (this)
            {
                foreach (Rectangle r in _barRects)
                {
                    CanvasChart.Children.Remove(r);
                }
                _barRects.Clear();

                // now create a new set
                int barCount = (int) (CanvasChart.ActualWidth/BarWidth) + 1;
                for (int i = 0; i < barCount; i++)
                {
                    var r = new Rectangle
                    {
                        Stroke = Brushes.DeepSkyBlue,
                        Fill = Brushes.Aqua,
                        Width = BarWidth,
                        Height = 0
                    };
                    Canvas.SetTop(r, CanvasChart.ActualHeight - r.Height);
                    Canvas.SetLeft(r, i*BarWidth);
                    _barRects.Add(r);
                    CanvasChart.Children.Add(r);
                }
            }
        }

        private void ButtonStop_Click(object sender, RoutedEventArgs e)
        {
            _requestStop = true;
        }

        private void ButtonStart_Click(object sender, RoutedEventArgs e)
        {
            _requestStop = false;
            ButtonStart.IsEnabled = false;
            ButtonStop.IsEnabled = true;

            ComboDistribution.IsEnabled = false;
            ComboGenerator.IsEnabled = false;

            switch (ComboGenerator.SelectedIndex)
            {
                case 0:
                    _rnd = new BasicGenerateRandom();
                    break;
                case 1:
                    _rnd = new LinearCongruentialRandom();
                    break;
                case 2:
                    _rnd = new SecureGenerateRandom();
                    break;
                case 3:
                    _rnd = new MultiplyWithCarryGenerateRandom();
                    break;
                case 4:
                    _rnd = new MersenneTwisterGenerateRandom();
                    break;
                default:
                    _rnd = new BasicGenerateRandom();
                    break;
            }

            _uniformMode = ComboDistribution.SelectedIndex == 0;
            Reset();

            var t = new Thread(BackgroundThread);
            t.Start();
        }

        public void BackgroundThread()
        {
            if (_uniformMode)
            {
                while (!_requestStop)
                {
                    ReportNumber(_rnd.NextDouble());
                }
            }
            else
            {
                while (!_requestStop)
                {
                    ReportNumber(_rnd.NextGaussian());
                }
            }

            Action a = () =>
            {
                ButtonStart.IsEnabled = true;
                ButtonStop.IsEnabled = false;
                ComboDistribution.IsEnabled = true;
                ComboGenerator.IsEnabled = true;
            };

            Dispatcher.BeginInvoke(a);
        }

        private void Window_Closed_1(object sender, EventArgs e)
        {
            _requestStop = true;
            Application.Current.Shutdown();
        }
    }
}