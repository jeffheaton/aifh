namespace AIFH_Vol2.Examples.Capstone.Model
{
    public class TitanicConfig
    {
        /// <summary>
        /// The name of the training data. (that we are to train on)
        /// </summary>
        public const string TrainingFilename = "train.csv";

        /// <summary>
        /// The name of the test data. (that Kaggle evaluates us on)
        /// </summary>
        public const string TestFilename = "test.csv";

        /// <summary>
        /// Dump the normalized data to this file.  This file is not actually used, but rather can be viewed to see
        /// the normalization.
        /// </summary>
        public const string NormDumpFilename = "normalized_dump.csv";

        /// <summary>
        /// The number of input features used.
        /// </summary>
        public const int InputFeatureCount = 13;

        /// <summary>
        /// The low range of the normalization.
        /// </summary>
        public const double InputNormalizeLow = -1;

        /// <summary>
        /// The high range of the normalization.
        /// </summary>
        public const double InputNormalizeHigh = 1;

        /// <summary>
        /// The value used for a prediction of survival.
        /// </summary>
        public const double PredictSurvive = 1;

        /// <summary>
        /// The value used for a prediction of perish.
        /// </summary>
        public const double PredictPerish = 0;

        /// <summary>
        /// The number of folds to use.
        /// </summary>
        public const int FoldCount = 5;

        /// <summary>
        /// The number of particles to use.
        /// </summary>
        public const int ParticleCount = 30;

        /// <summary>
        /// The number of RBF functions to use in each network.
        /// </summary>
        public const int RbfCount = 5;

        /// <summary>
        /// The number of iterations to allow with no improvement.
        /// </summary>
        public const int AllowNoImprovement = 100;
    }
}
