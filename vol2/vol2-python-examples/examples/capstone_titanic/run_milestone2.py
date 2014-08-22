import os
import sys
import csv

from titanic_milestone1 import *
from titanic_milestone2 import *

# The training data currently in use.
current_input = []
current_ideal = []

def calculate_score(alg):
    """
    Score the Titanic model. The score is percentage cases predicted correctly.
    """
    incorrect_count = 0
    total_count = 0

    for idx in len(current_input):
        total_count = total_count + 1
        predict_survive = alg.compute_regression(current_input[idx])[0] > 0.5
        ideal_survive = current_ideal[idx][0] > 0.5

        if predict_survive == ideal_survive:
            incorrect_count=incorrect_count+1

        return float(incorrect_count) / float(total_count)



def train_fold(k,fold):
    """
    Train a fold.
    @param k    The fold number.
    @param fold The fold.
    """

    no_improve = 0
    local_best = 0

    # Get the training and cross validation sets.
    training_input = fold.training_input
    training_ideal = fold.training_ideal
    validation_input = fold.validation_input
    validation_ideal = fold.validation_ideal

    # Create random particles for the RBF.
        RBFNetwork[] particles = new RBFNetwork[TitanicConfig.ParticleCount];
        for (int i = 0; i < particles.length; i++) {
            particles[i] = new RBFNetwork(TitanicConfig.InputFeatureCount, TitanicConfig.RBF_COUNT, 1);
            particles[i].reset(rnd);
        }

        /**
         * Construct a network to hold the best network.
         */
        if (bestNetwork == null) {
            bestNetwork = new RBFNetwork(TitanicConfig.InputFeatureCount, TitanicConfig.RBF_COUNT, 1);
        }

        /**
         * Setup the scoring function.
         */
        ScoreFunction score = new ScoreTitanic(training);
        ScoreFunction scoreValidate = new ScoreTitanic(validation);

        /**
         * Setup particle swarm.
         */
        boolean done = false;
        TrainPSO train = new TrainPSO(particles, score);
        int iterationNumber = 0;
        StringBuilder line = new StringBuilder();

        do {
            iterationNumber++;

            train.iteration();

            RBFNetwork best = (RBFNetwork) train.getBestParticle();

            double trainingScore = train.getLastError();
            double validationScore = scoreValidate.calculateScore(best);

            if (validationScore > bestScore) {
                System.arraycopy(best.getLongTermMemory(), 0, this.bestNetwork.getLongTermMemory(), 0, best.getLongTermMemory().length);
                this.bestScore = validationScore;
            }

            if (validationScore > localBest) {
                noImprove = 0;
                localBest = validationScore;
            } else {
                noImprove++;
            }

            line.setLength(0);
            line.append("Fold #");
            line.append(k + 1);
            line.append(", Iteration #");
            line.append(iterationNumber);
            line.append(": training correct: ");
            line.append(trainingScore);
            line.append(", validation correct: ");
            line.append(validationScore);
            line.append(", no improvement: ");
            line.append(noImprove);

            if (noImprove > TitanicConfig.AllowNoImprovement) {
                done = true;
            }

            System.out.println(line.toString());
        } while (!done);

        fold.setScore(localBest);
    }


# Find the data dir
if len(sys.argv) != 2:
    print("Please call this program with a single parameter that specifies your data directory.")
    sys.exit(1)
else:
    filename = sys.argv[1]

data_path = filename
training_path = os.path.join(data_path, TitanicConfig.TrainingFilename)
test_path = os.path.join(data_path, TitanicConfig.TestFilename)
normalize_Path = os.path.join(data_path, TitanicConfig.NormDumpFilename)

norm = NormalizeTitanic()
stats = TitanicStats()

norm.analyze(stats, training_path)
norm.analyze(stats, test_path)
stats.dump()

ids = []
norm.normalize(stats, training_path, ids,
                    TitanicConfig.InputNormalizeLow,
                    TitanicConfig.InputNormalizeHigh,
                    TitanicConfig.PredictSurvive,
                    TitanicConfig.PredictPerish)