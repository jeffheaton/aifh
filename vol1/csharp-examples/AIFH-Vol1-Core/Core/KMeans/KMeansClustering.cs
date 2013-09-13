// Artificial Intelligence for Humans
// Volume 1: Fundamental Algorithms
// C# Version
// http://www.aifh.org
// http://www.jeffheaton.com
//
// Code repository:
// https://github.com/jeffheaton/aifh
//
// Copyright 2013 by Jeff Heaton
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
using AIFH_Vol1.Core.Distance;
using AIFH_Vol1.Core.General.Data;
using AIFH_Vol1.Core.Randomize;

namespace AIFH_Vol1.Core.KMeans
{
    /// <summary>
    /// KMeans Clustering.  First, observations are each placed into random clusters.  There are two methods to do this:
    /// random and Forgy. Then we iterate through assignment and update steps.  Assignment places clusters in new clusters
    /// that they might be closer to.  Update updates the center of each cluster, called the centroid.  The center of each
    /// cluster is the mean of all observations in that cluster.
    ///
    /// This class uses a number of supporting objects:
    ///
    /// randomGeneration: The random number generator used for clustering.
    /// distanceMetric: The distance metric used to determine distance to centroids.
    ///
    /// http://en.wikipedia.org/wiki/Kmeans
    /// </summary>
    public class KMeansClustering
    {
        /// <summary>
        /// The number of clusters.
        /// </summary>
        private readonly int _k;

        /// <summary>
        /// The clusters.
        /// </summary>
        private readonly List<Cluster> _clusters = new List<Cluster>();

        /// <summary>
        /// The random number generator to use.
        /// </summary>
        private IGenerateRandom _randomGeneration = new BasicGenerateRandom();

        /// <summary>
        /// The distance metric to tuse.
        /// </summary>
        private ICalculateDistance _distanceMetric = new EuclideanDistance();

        /// <summary>
        /// Construct the object with K clusters. 
        /// </summary>
        /// <param name="theK">The number of clusters (K).</param>
        public KMeansClustering(int theK)
        {
            _k = theK;
        }

        /// <summary>
        /// Validate and find the number of dimensions from the first observation. 
        /// </summary>
        /// <param name="theObservations">The observations.</param>
        /// <returns>The number of dimensions.</returns>
        private int FindDimensions(IList<BasicData> theObservations)
        {
            if (theObservations.Count == 0)
            {
                throw new AIFHError("No observations provided to cluster, array zero length.");
            }

            if (theObservations.Count < _k)
            {
                throw new AIFHError("There are fewer observations ("
                        + theObservations.Count + ") than k (" + _k + ").");
            }

            int dimensions = theObservations[0].Input.Length;

            if (dimensions == 0)
            {
                throw new AIFHError("Observations have no dimensions.");
            }

            return dimensions;
        }

        /// <summary>
        /// Init the observations to random clusters.  Use the "Init Random" algorithm. The Random Partition method first
        /// randomly assigns a cluster to each observation and then proceeds to the update step, thus computing the initial mean to be the centroid of the cluster's randomly assigned points.
        /// 
        /// </summary>
        /// <param name="theObservations">The observations to cluster.</param>
        public void InitRandom(IList<BasicData> theObservations)
        {
            int dimensions = FindDimensions(theObservations);

            // create the clusters
            for (int i = 0; i < _k; i++)
            {
                _clusters.Add(new Cluster(dimensions));
            }

            // assign each observation to a random cluster
            foreach (BasicData observation in theObservations)
            {
                int clusterIndex = _randomGeneration.NextInt(_k);
                Cluster cluster = _clusters[clusterIndex];
                cluster.Observations.Add(observation);
            }

            // handle any empty clusters
            foreach (Cluster cluster in _clusters)
            {
                if (cluster.Observations.Count == 0)
                {
                    bool done = false;
                    while (!done)
                    {
                        int sourceIndex = _randomGeneration.NextInt(_k);
                        Cluster source = _clusters[sourceIndex];
                        if (source != cluster && source.Observations.Count > 1)
                        {
                            int sourceObservationIndex = _randomGeneration.NextInt(source.Observations.Count);
                            BasicData sourceObservation = source.Observations[sourceObservationIndex];
                            source.Observations.RemoveAt(sourceObservationIndex);
                            cluster.Observations.Add(sourceObservation);
                            done = true;
                        }
                    }
                }
            }

            // calculate initial centers
            UpdateStep();

        }

        /// <summary>
        /// Init the observations to random clusters.  The Forgy method randomly chooses k observations from the
        /// data set and uses these as the initial means. 
        /// </summary>
        /// <param name="theObservations">The observations to cluster.</param>
        public void InitForgy(IList<BasicData> theObservations)
        {
            int dimensions = FindDimensions(theObservations);

            _clusters.Clear();

            var usedObservations = new HashSet<int>();

            for (int i = 0; i < _k; i++)
            {
                var cluster = new Cluster(dimensions);
                _clusters.Add(cluster);

                int observationIndex = -1;

                while (observationIndex == -1)
                {
                    observationIndex = _randomGeneration.NextInt(theObservations.Count);
                    if (usedObservations.Contains(observationIndex))
                    {
                        observationIndex = -1;
                    }
                }

                double[] observation = theObservations[observationIndex].Input;
                Array.Copy(observation, 0, cluster.Center, 0, dimensions);
                usedObservations.Add(observationIndex);
            }

            // assign all observations to a cluster
            foreach (BasicData observation in theObservations)
            {
                Cluster cluster = FindNearestCluster(observation.Input);
                cluster.Observations.Add(observation);
            }

            // calculate initial centers
            UpdateStep();
        }

        /**
         * The update step updates the centroids.
         */
        private void UpdateStep()
        {
            foreach (Cluster cluster in _clusters)
            {
                cluster.CalculateCenter();
            }
        }

        /// <summary>
        /// The assignment step assigns observations to the nearest clusters.
        /// </summary>
        /// <returns>True, if we are done.  We are done if no observations moved clusters.</returns>
        private bool AssignmentStep()
        {
            bool done = true;

            foreach (Cluster cluster in _clusters)
            {
                int observationIndex = 0;
                int observationCount = cluster.Observations.Count;

                if (observationCount > 1)
                {
                    while (observationIndex < observationCount)
                    {
                        BasicData observation = cluster.Observations[observationIndex++];

                        Cluster targetCluster = FindNearestCluster(observation.Input);
                        if (targetCluster != cluster)
                        {
                            cluster.Observations.Remove(observation);
                            targetCluster.Observations.Add(observation);
                            observationCount--;
                            done = false;
                        }
                    }
                }
            }

            return done;
        }

        /// <summary>
        /// Find the nearest cluster for an observation. 
        /// </summary>
        /// <param name="observation">The observation.</param>
        /// <returns>The nearest cluster.</returns>
        public Cluster FindNearestCluster(double[] observation)
        {
            Cluster result = null;
            double resultDist = double.PositiveInfinity;

            foreach (Cluster cluster in _clusters)
            {
                double dist = _distanceMetric.Calculate(observation, cluster.Center);
                if (dist < resultDist)
                {
                    resultDist = dist;
                    result = cluster;
                }
            }

            return result;
        }

        /// <summary>
        /// Perform one iteration of assignment and update steps. 
        /// </summary>
        /// <returns>True, if we are done, no new assignments.</returns>
        public bool Iteration()
        {
            if (_clusters.Count == 0)
            {
                throw new AIFHError("Must call one of the init methods first.");
            }

            bool done = AssignmentStep();

            if (!done)
            {
                UpdateStep();
            }

            return done;
        }

        /// <summary>
        /// Perform the specified number of iterations. Stop early if we are done. 
        /// </summary>
        /// <param name="maxIterations">The max number of iterations.</param>
        /// <returns>True, if we are done.</returns>
        public int Iteration(int maxIterations)
        {
            int iterationCount = 1;

            while (iterationCount <= maxIterations && !Iteration())
            {
                iterationCount++;
            }

            return iterationCount;
        }

        /// <summary>
        /// The random number generator used.
        /// </summary>
        public IGenerateRandom RandomGeneration
        {
            get
            {
                return _randomGeneration;
            }
            set
            {
                _randomGeneration = value;
            }
        }

        /// <summary>
        /// The distance metric used.
        /// </summary>
        public ICalculateDistance DistanceMetric
        {
            get
            {
                return _distanceMetric;
            }
            set
            {
                _distanceMetric = value;
            }
        }

        /// <summary>
        /// The number of clusters.
        /// </summary>
        public int K
        {
            get
            {
                return _k;
            }
        }

        /// <summary>
        /// The clusters.
        /// </summary>
        public IList<Cluster> Clusters
        {
            get
            {
                return _clusters;
            }
        }

    }
}
