using AIFH_Vol3.Core;
using AIFH_Vol3.Core.General.Data;
using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AIFH_Vol3_Core.Core.Util
{
    /// <summary>
    ///  * This reads the MNIST dataset of handwritten digits into a data set.
    /// The MNIST dataset is found at http://yann.lecun.com/exdb/mnist/.
    ///
    /// Very loosely adapted from a class by Gabe Johnson<johnsogg@cmu.edu>.
    /// https://code.google.com
    /// /p/pen-ui/source/browse/trunk/skrui/src/org/six11/skrui
    /// /charrec/MNISTReader.java? r = 185
    /// </summary>
    public class MNISTReader
    {
        private int _numLabels;
        private int _numImages;
        private int _numRows;
        private int _numCols;
        private IList<BasicData> _data;

        private int ReadInt32(BinaryReader reader)
        {
            byte[] b = reader.ReadBytes(4);
            return b[3] + (b[2] << 8) + (b[1] << 16) + (b[0] << 24);
        }

        public MNISTReader(String labelFilename, String imageFilename, int depth)
        {
            if (depth != 1 && depth != 3)
            {
                throw new AIFHError("MNIST depth must be 1 or 3.");
            }

            BinaryReader labels;
            BinaryReader images;

            // Read label file, decompress (as read in) if needed.
            if (labelFilename.ToLower().EndsWith(".gz"))
            {
                labels = new BinaryReader(new GZipStream(File.Open(labelFilename, FileMode.Open),
                    CompressionMode.Decompress));
            }
            else
            {
                labels = new BinaryReader(File.Open(labelFilename, FileMode.Open));
            }

            // Read images file, decompress (as read in) if needed.
            if (imageFilename.ToLower().EndsWith(".gz"))
            {
                images = new BinaryReader(new GZipStream(File.Open(imageFilename, FileMode.Open),
                    CompressionMode.Decompress));
            }
            else
            {
                images = new BinaryReader(File.Open(imageFilename, FileMode.Open));
            }
            
            int magicNumber = ReadInt32(labels);
            if (magicNumber != 2049)
            {
                throw new AIFHError("Label file has wrong magic number: "
                        + magicNumber + " (should be 2049)");
            }
            magicNumber = ReadInt32(images);
            if (magicNumber != 2051)
            {
                throw new AIFHError("Image file has wrong magic number: "
                        + magicNumber + " (should be 2051)");
            }
            _numLabels = ReadInt32(labels);
            _numImages = ReadInt32(images);
            _numRows = ReadInt32(images);
            _numCols = ReadInt32(images);
            if (_numLabels != _numImages)
            {
                StringBuilder str = new StringBuilder();
                str.Append("Image file and label file do not contain the same number of entries.\n");
                str.Append("  Label file contains: " + _numLabels + "\n");
                str.Append("  Image file contains: " + _numImages + "\n");
                throw new AIFHError(str.ToString());
            }

            byte[] labelsData = labels.ReadBytes(_numLabels);
            int imageVectorSize = _numCols * _numRows;
            byte[] imagesData = images.ReadBytes(_numLabels * imageVectorSize);

            _data = new List<BasicData>();
            int imageIndex = 0;
            for (int i = 0; i < _numLabels; i++)
            {
                Console.WriteLine(i + "/" + _numLabels);
                int label = labelsData[i];
                double[] inputData = new double[imageVectorSize * depth];
                int outputIndex = 0;
                int t = imageIndex;
                for (int k = 0; k < depth; k++)
                {
                    imageIndex = t;
                    for (int j = 0; j < imageVectorSize; j++)
                    {
                        inputData[outputIndex++] = ((double)(imagesData[imageIndex++] & 0xff)) / 255.0;
                    }
                }
                double[] idealData = new double[10];
                idealData[label] = 1.0;
                _data.Add(new BasicData(inputData, idealData, null));
            }

            images.Close();
            labels.Close();
        }

        /// <summary>
        /// The number of labels.
        /// </summary>
        public int NumLabels
        {
            get
            {
                return _numLabels;
            }
        }

        /// <summary>
        /// The number of images.
        /// </summary>
        public int NumImages
        {
            get
            {
                return _numImages;
            }
        }

        /// <summary>
        /// The rows.
        /// </summary>
        public int NumRows
        {
            get
            {
                return _numRows;
            }
        }

        /// <summary>
        /// The columns.
        /// </summary>
        public int NumCols
        {
            get
            {
                return _numCols;
            }
        }

        /// <summary>
        /// The data.
        /// </summary>
        public IList<BasicData> Data
        {
            get
            {
                return _data;
            }
        }
    }
}
