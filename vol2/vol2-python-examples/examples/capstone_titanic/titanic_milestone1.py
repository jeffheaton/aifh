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
import csv


class TitanicConfig:
    """
    Configuration data for the Titanic project.
    """
    # The name of the training data. (that we are to train on)
    TrainingFilename = "train.csv"

    # The name of the test data. (that Kaggle evaluates us on)
    TestFilename = "test.csv"

    # Dump the normalized data to this file.  This file is not actually used, but rather can be viewed to see
    # the normalization.
    NormDumpFilename = "normalized_dump.csv"

    # The number of input features used.
    InputFeatureCount = 13

    # The low range of the normalization.
    InputNormalizeLow = -1

    # The high range of the normalization.
    InputNormalizeHigh = 1

    # The value used for a prediction of survival.
    PredictSurvive = 1

    # The value used for a prediction of perish.
    PredictPerish = 0

    # The number of folds to use.
    FoldCount = 5

    # The number of particles to use.
    ParticleCount = 30

    # The number of RBF functions to use in each network.
    RBF_COUNT = 5

    # The number of iterations to allow with no improvement.
    AllowNoImprovement = 100


class CalcHistogram:
    def __init__(self):
        self.histogram = {}

    def update(self, key):
        # See if we already have an entry
        if key in self.histogram:
            count = self.histogram[key]
            self.histogram[key] = count + 1
        else:
            self.histogram[key] = 1

    def get_max(self):
        max_count = 0
        result = None

        for key in self.histogram.keys():
            count = self.histogram[key]
            if result == None or max_count < count or (max_count == count and result < key):
                result = key
                max_count = count

        return result

    def get_min(self):
        min_count = 0
        result = None

        for key in self.histogram.keys():
            count = self.histogram[key]
            if result == None or min_count > count or (min_count == count and result < key):
                result = key
                min_count = count

        return result


class CalcMean:
    def __init__(self):
        # How many values have we encountered so far.
        self.count = 0

        # What is the sum of values.
        self.sum = 0


    def update(self, d):
        """
        Update mean for a new value.
        @param d The next value.
        """
        self.sum = self.sum + d
        self.count = self.count + 1


    def calculate(self):
        """
        @return The calculated mean.
        """
        return self.sum / self.count


class CalcSurvival:
    def __init__(self):
        # The count of males.
        self.count_male = 0

        # The count of females.
        self.count_female = 0

        # The count of male survivors.
        self.male_survive = 0

        # The count of female survivors.
        self.female_survive = 0


    def update(self, male, survived):
        """
        Update for a passenger.
        @param male     True, if passenger was male.
        @param survived True, if passenger survived.
        """
        if male:
            self.count_male = self.count_male + 1
        else:
            self.count_female = self.count_female + 1

        if survived:
            if male:
                self.male_survive = self.male_survive + 1
            else:
                self.female_survive = self.female_survive + 1

    def __str__(self):
        count = self.count_male + self.count_female
        result = "(Count: "
        result = result + str(count)

        if count > 0:
            pct = (self.female_survive + self.male_survive) / float(count)
            result = result + ", survived: "
            result = result + str(pct)

        if self.count_male > 0:
            pct = self.male_survive / float(self.count_male)
            result = result + ", male.survived: "
            result = result + str(pct)

        if self.count_female > 0:
            pct = self.female_survive / float(self.count_female)
            result = result + ", female.survived: "
            result = result + str(pct)

        result = result + ")"
        return result


class TitanicStats:
    def __init__(self):
        # Passengers with the title "master", mean age.
        self.mean_master = CalcMean()

        # Passengers with the title "mr", mean age.
        self.mean_mr = CalcMean()

        # Passengers with the title "miss", mean age.
        self.mean_miss = CalcMean()

        # Passengers with the title "mrs", mean age.
        self.mean_mrs = CalcMean()

        # Passengers with a military title, mean age.
        self.mean_military = CalcMean()

        # Passengers with a nobility title, mean age.
        self.mean_nobility = CalcMean()

        # Passengers with the title "dr".
        self.mean_dr = CalcMean()

        # Passengers with the title "rev".
        self.mean_clergy = CalcMean()

        # Total passengers.
        self.mean_total = CalcMean()

        # Total male passengers.
        self.mean_male = CalcMean()

        # Total female passengers.
        self.mean_female = CalcMean()

        # Passengers in 1st class, average fare.
        self.mean_fare1 = CalcMean()

        # Passengers in 2st class, average fare.
        self.mean_fare2 = CalcMean()

        # Passengers in 3rd class, average fare.
        self.mean_fare3 = CalcMean()

        # Survival stats for passengers with a title of "master".
        self.survival_master = CalcSurvival()

        # Survival stats for passengers with a title of "mr".
        self.survival_mr = CalcSurvival()

        # Survival stats for passengers with a title of "miss".
        self.survival_miss = CalcSurvival()

        # Survival stats for passengers with a title of "mrs".
        self.survival_mrs = CalcSurvival()

        # Survival stats for passengers with a military title.
        self.survival_military = CalcSurvival()

        # Survival stats for passengers with a nobility title.
        self.survival_nobility = CalcSurvival()

        # Survival stats for passengers with a title of "dr".
        self.survival_dr = CalcSurvival()

        # Survival stats for passengers with a title of "rev".
        self.survival_clergy = CalcSurvival()

        # Survival stats for all passengers.
        self.survival_total = CalcSurvival()

        # Survival stats for passengers that embarked from Southampton, England.
        self.embarked_s = CalcSurvival()

        # Survival stats for passengers that embarked from Cherbourg, France.
        self.embarked_c = CalcSurvival()

        # Survival stats for passengers that embarked from Queenstown, England.
        self.embarked_q = CalcSurvival()

        # Histogram of embark locations.
        self.embarked_histo = CalcHistogram()

    def dump(self):
        """
        Dump all stats to stdout.
        """
        print("Mean Master: Mean Age: " + str(self.mean_master.calculate()) + " " + str(self.survival_master))
        print("Mr.: Mean Age: " + str(self.mean_mr.calculate()) + " " + str(self.survival_mr))
        print("Miss.: Mean Age: " + str(self.mean_miss.calculate()) + " " + str(self.survival_miss))
        print("Mrs.: Mean Age: " + str(self.mean_mrs.calculate()) + " " + str(self.survival_mrs))
        print("Military: Mean Age: " + str(self.mean_mrs.calculate()) + " " + str(self.survival_military))
        print("Clergy: Mean Age: " + str(self.mean_clergy.calculate()) + " " + str(self.survival_clergy))
        print("Nobility: Mean Age: " + str(self.mean_nobility.calculate()) + " " + str(self.survival_nobility))
        print("Dr: Mean Age: " + str(self.mean_dr.calculate()) + " " + str(self.survival_dr))
        print("Total known survival: Mean Age: " + str(self.mean_total.calculate()) + " " + str(self.survival_total))
        print("")
        print("Embarked Queenstown: Mean Age: " + str(self.embarked_q))
        print("Embarked Southampton: Mean Age: " + str(self.embarked_s))
        print("Embarked Cherbourg: Mean Age: " + str(self.embarked_c))
        print("Most common embarked: " + str(self.embarked_histo.get_max()))
        print("")
        print("Mean Age Male: " + str(self.mean_male.calculate()))
        print("Mean Age Female: " + str(self.mean_female.calculate()))
        print("")
        print("Mean Fair 1st Class: " + str(self.mean_fare1.calculate()))
        print("Mean Fair 2st Class: " + str(self.mean_fare2.calculate()))
        print("Mean Fair 3st Class: " + str(self.mean_fare3.calculate()))


class NormalizeTitanic:
    def analyze(self, stats, filename):
        """
        Analyze and generate stats for titanic data.
        @param stats    The stats for titanic.
        @param filename The file to analyze.
        @return The passenger count.
        @throws IOException Errors reading file.
        """
        count = 0
        headerMap = {}

        with open(filename, 'rb') as f:
            reader = csv.reader(f)

            header_map = {}
            header = reader.next()

            for i in range(0, len(header)):
                header_map[header[i].lower()] = i

            age_index = header_map["age"]
            name_index = header_map["name"]
            sex_index = header_map["sex"]
            index_embarked = header_map["embarked"]
            index_fare = header_map["fare"]
            index_pclass = header_map["pclass"]

            survived_index = -1

            # test data does not have survived
            if "survived" in header_map:
                survived_index = header_map["survived"]

            for next_line in reader:
                count = count + 1
                name = next_line[name_index]
                age_str = next_line[age_index]
                sex_str = next_line[sex_index]
                embarked_str = next_line[index_embarked]

                # test data does not have survived, do not use survived boolean if using test data!
                survived = False
                if survived_index != -1:
                    survived_str = next_line[survived_index]
                    survived = (survived_str == "1")

                if index_embarked != -1:
                    embarked_str = next_line[index_embarked]

                # calculate average fare per class
                str_fare = next_line[index_fare]
                if len(str_fare) > 0:
                    fare = float(str_fare)

                    pclass = next_line[index_pclass]
                    if pclass == "1":
                        stats.mean_fare1.update(fare)
                    elif pclass == "2":
                        stats.mean_fare2.update(fare)
                    elif pclass == "3":
                        stats.mean_fare3.update(fare)

                is_male = (sex_str == "male")

                # Only compute survival stats on training data
                if survived_index != -1:
                    if embarked_str == "Q":
                        stats.embarked_q.update(is_male, survived)
                    elif embarked_str == "S":
                        stats.embarked_s.update(is_male, survived)
                    elif embarked_str == "C":
                        stats.embarked_c.update(is_male, survived)

                stats.embarked_histo.update(embarked_str)

                # Only compute survival stats on training data.
                if survived_index != -1:
                    stats.survival_total.update(is_male, survived)

                if survived_index != -1:
                    if "Master." in name:
                        stats.survival_master.update(is_male, survived)
                    elif "Mr." in name:
                        stats.survival_mr.update(is_male, survived)
                    elif "Miss." in name or "Mlle." in name:
                        stats.survival_miss.update(is_male, survived)
                    elif "Mrs." in name or "Mme." in name:
                        stats.survival_mrs.update(is_male, survived)
                    elif "Col." in name or "Capt." in name or "Major." in name:
                        stats.survival_military.update(is_male, survived)
                    elif "Countess." in name or "Lady." in name or "Sir." in name or "Don." in name or "Dona." in name or "Jonkheer." in name:
                        stats.survival_nobility.update(is_male, survived)
                    elif "Dr." in name:
                        stats.survival_dr.update(is_male, survived)
                    elif "Rev." in name:
                        stats.survival_clergy.update(is_male, survived)

                if len(age_str) > 0:
                    age = float(age_str)

                    # Update general mean age for male/female
                    if is_male:
                        stats.mean_male.update(age)
                    else:
                        stats.mean_female.update(age)

                    # Update the total average age
                    stats.mean_total.update(age)

                if "Master." in name:
                    stats.mean_master.update(age)
                    # Only compute survival stats on training data.
                    if survived_index != -1:
                        stats.survival_master.update(is_male, survived)
                elif "Mr." in name:
                    stats.mean_mr.update(age)
                    # Only compute survival stats on training data.
                    if survived_index != -1:
                        stats.survival_mr.update(is_male, survived)
                elif "Miss." in name or "Mlle." in name:
                    stats.mean_miss.update(age)
                    # Only compute survival stats on training data.
                    if survived_index != -1:
                        stats.survival_miss.update(is_male, survived)
                elif "Mrs." in name or "Mme." in name:
                    stats.mean_mrs.update(age)
                    # Only compute survival stats on training data.
                    if survived_index != -1:
                        stats.survival_mrs.update(is_male, survived)
                elif "Col." in name or "Capt." in name or "Major." in name:
                    stats.mean_military.update(age)
                    # Only compute survival stats on training data.
                    if survived_index != -1:
                        stats.survival_military.update(is_male, survived)
                elif "Countess." in name or "Lady." in name or "Sir." in name or "Don." in name or "Dona." in name or "Jonkheer." in name:
                    stats.mean_nobility.update(age)
                    # Only compute survival stats on training data.
                    if survived_index != -1:
                        stats.survival_nobility.update(is_male, survived)
                elif "Dr." in name:
                    stats.mean_dr.update(age)
                    # Only compute survival stats on training data.
                    if survived_index != -1:
                        stats.survival_dr.update(is_male, survived)
                elif "Rev." in name:
                    stats.mean_clergy.update(age)
                    # Only compute survival stats on training data.
                    if survived_index != -1:
                        stats.survival_clergy.update(is_male, survived)

        return count

    def range_normalize(self, x, data_low, data_high, normalized_low, normalized_high):
        """
        Normalize to a range.
        @param x              The value to normalize.
        @param dataLow        The low end of the range of the data.
        @param dataHigh       The high end of the range of the data.
        @param normalizedLow  The normalized low end of the range of data.
        @param normalizedHigh The normalized high end of the range of data.
        @return The normalized value.
        """
        return ((x - data_low)
                / (data_high - data_low)) \
               * (normalized_high - normalized_low) + normalized_low


    def normalize(self, stats, filename, ids, input_low, input_high, predict_survive, predict_perish):
        self.result_input = []
        self.result_ideal = []

        headerMap = {}

        with open(filename, 'rb') as f:
            reader = csv.reader(f)

            header_map = {}
            header = reader.next()

            for i in range(0, len(header)):
                header_map[header[i].lower()] = i

            age_index = header_map["age"]
            name_index = header_map["name"]
            sex_index = header_map["sex"]
            index_embarked = header_map["embarked"]
            index_pclass = header_map["pclass"]
            index_sibsp = header_map["sibsp"]
            index_parch = header_map["parch"]
            index_fare = header_map["fare"]
            index_id = header_map["passengerid"]
            survived_index = -1

            # test data does not have survived
            if "survived" in header_map:
                survived_index = header_map["survived"]

            for next_line in reader:
                name = next_line[name_index]
                sex = next_line[sex_index]
                embarked = next_line[index_embarked]
                id = next_line[index_id]

                # Add record the passenger id, if requested
                if ids != None:
                    ids.append(id)

                is_male = (sex.lower() == "male")

                # do we have an age for this person?
                if len(next_line[age_index]) == 0:
                    # age is missing, interpolate using name
                    if "Master." in name:
                        age = stats.mean_master.calculate()
                    elif "Mr." in name:
                        age = stats.mean_mr.calculate()
                    elif "Miss." in name or "Mlle." in name:
                        age = stats.mean_miss.calculate()
                    elif "Mrs." in name or "Mme." in name:
                        age = stats.mean_mrs.calculate()
                    elif "Col." in name or "Capt." in name or "Major." in name:
                        age = stats.mean_military.calculate()
                    elif "Countess." in name or "Lady." in name or "Sir." in name or "Don." in name or "Dona." in name or "Jonkheer." in name:
                        age = stats.mean_nobility.calculate()
                    elif "Dr." in name:
                        age = stats.mean_dr.calculate()
                    elif "Rev." in name:
                        age = stats.mean_clergy.calculate()
                    else:
                        if is_male:
                            age = stats.mean_male.calculate()
                        else:
                            age = stats.mean_female.calculate()
                else:
                    age = float(next_line[age_index])

                input = [0] * TitanicConfig.InputFeatureCount
                input[0] = self.range_normalize(age, 0, 100, input_low, input_high)

                # sex-male
                input[1] = input_high if is_male else input_low

                # pclass
                pclass = float(next_line[index_pclass])
                input[2] = self.range_normalize(pclass, 1, 3, input_low, input_high)

                # sibsp
                sibsp = float(next_line[index_sibsp])
                input[3] = self.range_normalize(sibsp, 0, 10, input_low, input_high)

                # parch
                parch = float(next_line[index_parch])
                input[4] = self.range_normalize(parch, 0, 10, input_low, input_high)

                # fare
                str_fare = next_line[index_fare]

                if len(str_fare) == 0:
                    if int(pclass) == 1:
                        fare = stats.mean_fare1.calculate()
                    elif int(pclass) == 2:
                        fare = stats.getMeanFare2().calculate()
                    elif int(pclass) == 3:
                        fare = stats.getMeanFare3().calculate();
                    else:
                        # should not happen, we would have a class other than 1,2,3.
                        # however, if that DID happen, use the median class (2).
                        fare = stats.mean_Fare2.calculate()
                else:
                    fare = float(next_line[index_fare])

                input[5] = self.range_normalize(fare, 0, 500, input_low, input_high)

                # embarked-c
                input[6] = input_high if embarked.strip() == "c" else input_low

                # embarked-q
                input[7] = input_high if embarked.strip() == "q" else input_low

                # embarked-s
                input[8] = input_high if embarked.strip() == "s" else input_low

                # name-mil
                input[9] = input_high if ("Col." in name or "Capt." in name or "Major." in name) else input_low

                # name-nobility
                input[10] = input_high if (
                "Countess." in name or "Lady." in name or "Sir." in name or "Don." in name or "Dona." in name or "Jonkheer.") else input_low

                # name-dr
                input[11] = input_high if ("Dr." in name) else input_low


                # name-clergy
                input[12] = input_high if ("Rev." in name) else input_low

                # add the new row
                self.result_input.append(input)

                # add survived, if it exists
                if survived_index != -1:
                    survived = int(next_line[survived_index])
                    ideal = [predict_survive if survived == 1 else predict_perish]
                    self.result_ideal.append(ideal)



