from titanic_milestrone1 import *
import random

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

        # Copy
        temp_input = list(training_input)
        temp_ideal = list(training_ideal)

        # Create the folds
        for i in range(k):
            self.folds.append(CrossValidateFold())

        # Divide over the k sets.
        leave_out_set = 0

        while len(temp_ideal)>0:
            idx = random.randint(0,len(temp_input))

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
