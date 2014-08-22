#!/usr/bin/env python
"""
    Artificial Intelligence for Humans
    Volume 2: Nature-Inspired Algorithms
    Python Version
    http://www.aifh.org
    http://www.jeffheaton.com

    Code repository:
    https://github.com/jeffheaton/aifh

    Copyright 2014 by Jeff Heaton

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

    For more information on Heaton Research copyrights, licenses
    and trademarks visit:
    http://www.heatonresearch.com/copyright
"""
from titanic_milestone1 import *
from random import *
import sys
import os

# Find the AIFH core files
aifh_dir = os.path.dirname(os.path.abspath(__file__))
aifh_dir = os.path.abspath(aifh_dir + os.sep + ".." + os.sep + ".." + os.sep + "lib" + os.sep + "aifh")
sys.path.append(aifh_dir)

from rbf_network import *
from pso import *

class CrossValidateFold:
    """
    A cross validation fold.  This contains a training and validation set.  A score is also
    held for the validation sets.
    """
    def __init__(self):
        self.training_input = []
        self.training_ideal = []
        self.validation_input = []
        self.validation_ideal = []
        self.score = float("inf")

class CrossValidate:
    """
    Used to perform a k-leave out cross validation.
    This works by breaking the data set into k (often 5) random subsets.  From this we can create 5 training & validation
    sets. Each validation set becomes one of the k random subsets.  For each validation set a corresponding training set
    is created by using all data, but leaving out the validation set.
    http://en.wikipedia.org/wiki/Cross-validation_(statistics)
    """
    def __init__(self,k,training_input,training_ideal):
        """
        The constructor.
        @param k The number of folds.
        @param training The training set.
        """

        # The folds of the cross validation.
        self.folds = []
        self.best_network = None
        self.best_score = float("inf")

        # Copy
        temp_input = list(training_input)
        temp_ideal = list(training_ideal)

        # Create the folds
        for i in range(k):
            self.folds.append(CrossValidateFold())

        # Divide over the k sets.
        leave_out_set = 0

        while len(temp_ideal)>0:
            idx = randint(0,len(temp_input)-1)

            item_input = temp_input[idx]
            item_ideal = temp_ideal[idx]

            del temp_input[idx]
            del temp_ideal[idx]

            self.folds[leave_out_set].validation_input.append(item_input)
            self.folds[leave_out_set].validation_ideal.append(item_ideal)

            for include_set in range(len(self.folds)):
                if include_set != leave_out_set:
                    self.folds[include_set].training_input.append(item_input)
                    self.folds[include_set].training_ideal.append(item_ideal)

            leave_out_set = leave_out_set + 1
            if leave_out_set >= k:
                leave_out_set = 0

    def score(self):
        """
        The average score over all folds.
        """
        sum = 0
        for fold in self.folds:
            sum = sum + fold.score

        return sum / len(self.folds)

# The training data currently in use.
current_input = []
current_ideal = []
network = None

def calculate_score(alg):
    """
    Score the Titanic model. The score is percentage cases predicted correctly.
    """
    global network
    incorrect_count = 0
    total_count = 0

    network.copy_memory(alg)

    for idx in range(len(current_input)):
        total_count = total_count + 1
        predict_survive = network.compute_regression(current_input[idx])[0] > 0.5
        ideal_survive = float(current_ideal[idx][0]) > 0.5

        if predict_survive == ideal_survive:
            incorrect_count=incorrect_count+1

    return float(incorrect_count) / float(total_count)



def train_fold(cross,k,fold):
    """
    Train a fold.
    @param k    The fold number.
    @param fold The fold.
    """

    global current_input, current_ideal, network
    no_improve = 0
    local_best = 0

    # Get the training and cross validation sets.
    training_input = fold.training_input
    training_ideal = fold.training_ideal
    validation_input = fold.validation_input
    validation_ideal = fold.validation_ideal

    # Create an RBF network.
    network = RbfNetwork(TitanicConfig.InputFeatureCount, TitanicConfig.RBF_COUNT, 1)
    network.reset()

    # Construct a network to hold the best network.
    if cross.best_network == None:
        cross.best_network = RbfNetwork(TitanicConfig.InputFeatureCount, TitanicConfig.RBF_COUNT, 1)


    # Perform the PSO training
    train = TrainPSO(TitanicConfig.ParticleCount,len(network.long_term_memory),calculate_score)
    train.goal_minimize = False
    train.display_iteration = True

    done = False
    iteration_number = 0

    while not done:
        iteration_number = iteration_number + 1

        # point the score function at training data
        current_input = training_input
        current_ideal = training_ideal

        # perform the iteration
        train.iteration()

        # Get the best particle
        train.copy_best(network.long_term_memory)

        train_score = train.get_best_score()

        # point the score function at training data
        current_input = validation_input
        current_ideal = validation_ideal

        # Evaluate validation set
        validation_score = calculate_score(network.long_term_memory)

        if validation_score > cross.best_score:
            train.copy_best(cross.best_network.long_term_memory)
            cross.best_score = validation_score

        if validation_score > local_best:
            no_improve = 0
            local_best = validation_score
        else:
            no_improve = no_improve + 1

        print("Fold #" + str(k+1) + ", Iteration #" + str(iteration_number)
              + ": training correct: " +str(train_score) + ": validation correct: "
              + str(validation_score) + ", No Improvement: " + str(no_improve))

        if no_improve > TitanicConfig.AllowNoImprovement:
            done = True

        fold.score = local_best

def fit_titanic(training_path, test_path):
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

    # Fold the data for cross validation.
    cross = CrossValidate(TitanicConfig.FoldCount, norm.result_input, norm.result_ideal)

    for k in range(0,len(cross.folds)):
        print("Cross validation fold #" + str(k + 1) + "/" + str(len(cross.folds)))
        train_fold(cross, k, cross.folds[k])


    # Show the cross validation summary.
    print("Crossvalidation summary:")
    k = 1
    for fold in cross.folds:
        print("Fold #" + str(k) + ": " + str(fold.score))
        k = k + 1

    print("Final, crossvalidated score:" + str(cross.score) )

    return cross