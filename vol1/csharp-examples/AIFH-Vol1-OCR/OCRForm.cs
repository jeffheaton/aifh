
using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using AIFH_Vol1.Core.Distance;
using Microsoft.VisualBasic;

namespace AIFH_Vol1_OCR
{
    public partial class OCRForm : Form
    {
        /// <summary>
        ///     The downsample width for the application.
        /// </summary>
        private const int DownsampleWidth = 5;

        /// <summary>
        ///     The down sample height for the application.
        /// </summary>
        private const int DownsampleHeight = 7;

        private readonly ICalculateDistance _distCalc = new EuclideanDistance();
        private readonly Pen _blackPen;

        private readonly Graphics _entryGraphics;
        private readonly Bitmap _entryImage;
        private readonly Dictionary<char, double[]> _letterData = new Dictionary<char, double[]>();
        private double[] _downsampled;
        private int _entryLastX;
        private int _entryLastY;

        public OCRForm()
        {
            InitializeComponent();
            _blackPen = new Pen(Color.Black);
            _entryImage = new Bitmap(entry.Width, entry.Height);
            _entryGraphics = Graphics.FromImage(_entryImage);
            _downsampled = new double[DownsampleHeight*DownsampleWidth];
            ClearEntry();
        }

        private void entry_Paint(object sender, PaintEventArgs e)
        {
            Graphics g = e.Graphics;
            g.DrawImage(_entryImage, 0, 0);
            var blackPen = new Pen(Color.Black);
            g.DrawRectangle(blackPen, 0, 0, entry.Width - 1, entry.Height - 1);
        }

        private void btnDelete_Click(object sender, EventArgs e)
        {
            var str = (string) letters.Items[letters.SelectedIndex];
            char ch = str[0];
            _letterData.Remove(ch);
            letters.Items.Remove(str);
            ClearEntry();
        }

        private void btnLoad_Click(object sender, EventArgs e)
        {
            try
            {
                TextReader f = new StreamReader("Sample.dat");

                String line;

                _letterData.Clear();
                letters.Items.Clear();

                while ((line = f.ReadLine()) != null)
                {
                    const int sampleSize = DownsampleHeight*DownsampleWidth;
                    char ch = char.ToUpper(line[0]);
                    var localSample = new double[sampleSize];

                    int idx = 2;
                    for (int i = 0; i < sampleSize; i++)
                    {
                        if (line[idx++] == '1')
                            localSample[i] = 1;
                        else
                            localSample[i] = 0;
                    }

                    _letterData.Add(ch, localSample);
                    letters.Items.Add("" + ch);
                }

                f.Close();

                MessageBox.Show(this, @"Loaded from 'sample.dat'.");
            }
            catch (Exception ex)
            {
                MessageBox.Show(@"Error: " + ex.Message);
            }
        }

        private void btnSave_Click(object sender, EventArgs e)
        {
            try
            {
                TextWriter f = new StreamWriter("..\\..\\..\\Data\\Sample.dat");
                const int size = DownsampleHeight*DownsampleWidth;

                foreach (object t in letters.Items)
                {
                    char ch = ((string) t)[0];
                    double[] data = _letterData[ch];

                    f.Write(ch + ":");
                    for (int j = 0; j < size; j++)
                    {
                        f.Write(data[j]>0.1 ? "1" : "0");
                    }
                    f.WriteLine("");
                }
                f.Close();
                MessageBox.Show(@"Saved to 'sample.dat'.");
            }
            catch (Exception e2)
            {
                MessageBox.Show(@"Error: " + e2.Message, @"Training");
            }
        }

        private void btnAdd_Click(object sender, EventArgs e)
        {
            var ds = new DownSample(_entryImage);
            _downsampled = ds.PerformDownSample(DownsampleWidth, DownsampleHeight);
            sample.Invalidate();
            const string prompt = "What letter did you just draw.";
            const string title = "Input Required";
            const string Default = "";
            Int32 xPos = ((SystemInformation.WorkingArea.Width/2) - 200);
            Int32 yPos = ((SystemInformation.WorkingArea.Height/2) - 100);

            bool valid = false;
            foreach (double t in _downsampled)
            {
                if (t>0.1)
                {
                    valid = true;
                }
            }

            if (!valid)
            {
                MessageBox.Show(@"Please draw a letter before adding it.");
                return;
            }

            String result = Interaction.InputBox(prompt, title, Default, xPos, yPos);
            if (result != null)
            {
                result = result.ToUpper();
                if (result.Length == 0)
                {
                    MessageBox.Show(@"Please enter a character.");
                }
                else if (result.Length > 1)
                {
                    MessageBox.Show(@"Please enter only a single character.");
                }
                else if (_letterData.ContainsKey(result[0]))
                {
                    MessageBox.Show(@"That letter is already defined, please delete first.");
                }
                else
                {
                    letters.Items.Add(result);
                    _letterData.Add(result[0], _downsampled);
                    ClearEntry();
                }
            }
        }

        private void btnRecognize_Click(object sender, EventArgs e)
        {
            var ds = new DownSample(_entryImage);
            _downsampled = ds.PerformDownSample(DownsampleWidth, DownsampleHeight);
            sample.Invalidate();


            int sampleSize = DownsampleHeight*DownsampleWidth;
            char bestChar = '?';
            double bestDistance = double.MaxValue;

            foreach (char ch in _letterData.Keys)
            {
                double[] data = _letterData[ch];
                double dist = _distCalc.Calculate(data, _downsampled);

                if (dist < bestDistance)
                {
                    bestDistance = dist;
                    bestChar = ch;
                }
            }


            MessageBox.Show(@"That might be " + bestChar, @"Recognize");
            ClearEntry();
        }

        private void btnClear_Click(object sender, EventArgs e)
        {
            ClearEntry();
        }

        private void btnSample_Click(object sender, EventArgs e)
        {
            var ds = new DownSample(_entryImage);
            _downsampled = ds.PerformDownSample(DownsampleWidth, DownsampleHeight);
            sample.Invalidate();
        }

        public void ClearEntry()
        {
            Brush whiteBrush = new SolidBrush(Color.White);
            _entryGraphics.FillRectangle(whiteBrush, 0, 0, entry.Width, entry.Height);
            entry.Invalidate();
            var ds = new DownSample(_entryImage);
            _downsampled = ds.PerformDownSample(DownsampleWidth, DownsampleHeight);
            sample.Invalidate();
        }

        private void entry_MouseDown(object sender, MouseEventArgs e)
        {
            entry.Capture = true;
            _entryLastX = e.X;
            _entryLastY = e.Y;
        }

        private void entry_MouseUp(object sender, MouseEventArgs e)
        {
            _entryGraphics.DrawLine(_blackPen, _entryLastX, _entryLastY, e.X, e.Y);
            entry.Invalidate();
            entry.Capture = false;
        }

        private void entry_MouseMove(object sender, MouseEventArgs e)
        {
            if (entry.Capture)
            {
                _entryGraphics.DrawLine(_blackPen, _entryLastX, _entryLastY, e.X, e.Y);
                entry.Invalidate();
                _entryLastX = e.X;
                _entryLastY = e.Y;
            }
        }

        private void sample_Paint(object sender, PaintEventArgs e)
        {
            Graphics g = e.Graphics;

            int x, y;
            int vcell = sample.Height/DownsampleHeight;
            int hcell = sample.Width/DownsampleWidth;
            Brush whiteBrush = new SolidBrush(Color.White);
            Brush blackBrush = new SolidBrush(Color.Black);
            var blackPen = new Pen(Color.Black);

            g.FillRectangle(whiteBrush, 0, 0, sample.Width, sample.Height);


            for (y = 0; y < DownsampleHeight; y++)
            {
                g.DrawLine(blackPen, 0, y*vcell, sample.Width, y*vcell);
            }
            for (x = 0; x < DownsampleWidth; x++)
            {
                g.DrawLine(blackPen, x*hcell, 0, x*hcell, sample.Height);
            }

            int index = 0;
            for (y = 0; y < DownsampleHeight; y++)
            {
                for (x = 0; x < DownsampleWidth; x++)
                {
                    if (_downsampled[index++]>0.1)
                    {
                        g.FillRectangle(blackBrush, x*hcell, y*vcell, hcell, vcell);
                    }
                }
            }

            g.DrawRectangle(blackPen, 0, 0, sample.Width - 1, sample.Height - 1);
        }

        private void letters_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (letters.SelectedIndex >= 0)
            {
                var str = (string) letters.Items[letters.SelectedIndex];
                char ch = str[0];
                _downsampled = _letterData[ch];
                sample.Invalidate();
            }
        }
    }
}