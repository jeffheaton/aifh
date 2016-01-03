// Artificial Intelligence for Humans
// Volume 3: Deep Learning and Neural Networks
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2015 by Jeff Heaton
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
using System.IO;
using System.IO.Compression;
using System.Text;
using AIFH_Vol3.Core;
using AIFH_Vol3.Core.General.Data;

namespace AIFH_Vol3_Core.Core.Util
{
    /// <summary>
    ///     * This reads the MNIST dataset of handwritten digits into a data set.
    ///     The MNIST dataset is found at http://yann.lecun.com/exdb/mnist/.
    ///     Very loosely adapted from a class by Gabe Johnson
    ///     <johnsogg@ cmu.edu>
    ///         .
    ///         https://code.google.com
    ///         /p/pen-ui/source/browse/trunk/skrui/src/org/six11/skrui
    ///         /charrec/MNISTReader.java? r = 185
    /// </summary>
    public class MNISTReader
    {
        public MNISTReader(string labelFilename, string imageFilename, int depth)
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

            var magicNumber = ReadInt32(labels);
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
            NumLabels = ReadInt32(labels);
            NumImages = ReadInt32(images);
            NumRows = ReadInt32(images);
            NumCols = ReadInt32(images);
            if (NumLabels != NumImages)
            {
                var str = new StringBuilder();
                str.Append("Image file and label file do not contain the same number of entries.\n");
                str.Append("  Label file contains: " + NumLabels + "\n");
                str.Append("  Image file contains: " + NumImages + "\n");
                throw new AIFHError(str.ToString());
            }

            var labelsData = labels.ReadBytes(NumLabels);
            var imageVectorSize = NumCols*NumRows;
            var imagesData = images.ReadBytes(NumLabels*imageVectorSize);

            Data = new List<BasicData>();
            var imageIndex = 0;
            for (var i = 0; i < NumLabels; i++)
            {
                Console.WriteLine(i + "/" + NumLabels);
                int label = labelsData[i];
                var inputData = new double[imageVectorSize*depth];
                var outputIndex = 0;
                var t = imageIndex;
                for (var k = 0; k < depth; k++)
                {
                    imageIndex = t;
                    for (var j = 0; j < imageVectorSize; j++)
                    {
                        inputData[outputIndex++] = (imagesData[imageIndex++] & 0xff)/255.0;
                    }
                }
                var idealData = new double[10];
                idealData[label] = 1.0;
                Data.Add(new BasicData(inputData, idealData, null));
            }

            images.Close();
            labels.Close();
        }

        /// <summary>
        ///     The number of labels.
        /// </summary>
        public int NumLabels { get; private set; }

        /// <summary>
        ///     The number of images.
        /// </summary>
        public int NumImages { get; private set; }

        /// <summary>
        ///     The rows.
        /// </summary>
        public int NumRows { get; private set; }

        /// <summary>
        ///     The columns.
        /// </summary>
        public int NumCols { get; private set; }

        /// <summary>
        ///     The data.
        /// </summary>
        public IList<BasicData> Data { get; private set; }

        /// <summary>
        /// Read a big-endian integer.
        /// </summary>
        /// <param name="reader">Where to read from.</param>
        /// <returns>The integer.</returns>
        private int ReadInt32(BinaryReader reader)
        {
            var b = reader.ReadBytes(4);
            return b[3] + (b[2] << 8) + (b[1] << 16) + (b[0] << 24);
        }
    }
}