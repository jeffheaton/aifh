/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */
package com.heatonresearch.aifh.examples.capstone.model.milestone1;

/**
 * Stats that were collected about the titanic passengers to help with normalization, interpolation
 * and modeling.
 */
public class TitanicStats {
    /**
     * Passengers with the title "master", mean age.
     */
    private final CalcMean meanMaster = new CalcMean();

    /**
     * Passengers with the title "mr", mean age.
     */
    private final CalcMean meanMr = new CalcMean();

    /**
     * Passengers with the title "miss", mean age.
     */
    private final CalcMean meanMiss = new CalcMean();

    /**
     * Passengers with the title "mrs", mean age.
     */
    private final CalcMean meanMrs = new CalcMean();

    /**
     * Passengers with a military title, mean age.
     */
    private final CalcMean meanMilitary = new CalcMean();

    /**
     * Passengers with a nobility title, mean age.
     */
    private final CalcMean meanNobility = new CalcMean();

    /**
     * Passengers with the title "dr".
     */
    private final CalcMean meanDr = new CalcMean();

    /**
     * Passengers with the title "rev".
     */
    private final CalcMean meanClergy = new CalcMean();

    /**
     * Total passengers.
     */
    private final CalcMean meanTotal = new CalcMean();

    /**
     * Total male passengers.
     */
    private final CalcMean meanMale = new CalcMean();

    /**
     * Total female passengers.
     */
    private final CalcMean meanFemale = new CalcMean();

    /**
     * Passengers in 1st class, average fare.
     */
    private final CalcMean meanFare1 = new CalcMean();

    /**
     * Passengers in 2st class, average fare.
     */
    private final CalcMean meanFare2 = new CalcMean();

    /**
     * Passengers in 3rd class, average fare.
     */
    private final CalcMean meanFare3 = new CalcMean();

    /**
     * Survival stats for passengers with a title of "master".
     */
    private final CalcSurvival survivalMaster = new CalcSurvival();

    /**
     * Survival stats for passengers with a title of "mr".
     */
    private final CalcSurvival survivalMr = new CalcSurvival();

    /**
     * Survival stats for passengers with a title of "miss".
     */
    private final CalcSurvival survivalMiss = new CalcSurvival();

    /**
     * Survival stats for passengers with a title of "mrs".
     */
    private final CalcSurvival survivalMrs = new CalcSurvival();

    /**
     * Survival stats for passengers with a military title.
     */
    private final CalcSurvival survivalMilitary = new CalcSurvival();

    /**
     * Survival stats for passengers with a nobility title.
     */
    private final CalcSurvival survivalNobility = new CalcSurvival();

    /**
     * Survival stats for passengers with a title of "dr".
     */
    private final CalcSurvival survivalDr = new CalcSurvival();

    /**
     * Survival stats for passengers with a title of "rev".
     */
    private final CalcSurvival survivalClergy = new CalcSurvival();

    /**
     * Survival stats for all passengers.
     */
    private final CalcSurvival survivalTotal = new CalcSurvival();

    /**
     * Survival stats for passengers that embarked from Southampton, England.
     */
    private final CalcSurvival embarkedS = new CalcSurvival();

    /**
     * Survival stats for passengers that embarked from Cherbourg, France.
     */
    private final CalcSurvival embarkedC = new CalcSurvival();

    /**
     * Survival stats for passengers that embarked from Queenstown, England.
     */
    private final CalcSurvival embarkedQ = new CalcSurvival();

    /**
     * Histogram of embark locations.
     */
    private final CalcHistogram embarkedHisto = new CalcHistogram();

    /**
     * @return Passengers with the title "master", mean age.
     */
    public CalcMean getMeanMaster() {
        return meanMaster;
    }

    /**
     * @return Passengers with the title "mr", mean age.
     */
    public CalcMean getMeanMr() {
        return meanMr;
    }

    /**
     * @return Passengers with the title "miss", mean age.
     */
    public CalcMean getMeanMiss() {
        return meanMiss;
    }

    /**
     * @return Passengers with the title "mrs", mean age.
     */
    public CalcMean getMeanMrs() {
        return meanMrs;
    }

    /**
     * @return Passengers with a military title, mean age.
     */
    public CalcMean getMeanMilitary() {
        return meanMilitary;
    }

    /**
     * @return Passengers with a noble title, mean age.
     */
    public CalcMean getMeanNobility() {
        return meanNobility;
    }

    /**
     * @return Passengers with the title "dr", mean age.
     */
    public CalcMean getMeanDr() {
        return meanDr;
    }

    /**
     * @return Passengers with the title "rev", mean age.
     */
    public CalcMean getMeanClergy() {
        return meanClergy;
    }

    /**
     * @return Mean age for all passengers.
     */
    public CalcMean getMeanTotal() {
        return meanTotal;
    }

    /**
     * @return Survival stats for passengers with a title of "master".
     */
    public CalcSurvival getSurvivalMaster() {
        return survivalMaster;
    }

    /**
     * @return Survival stats for passengers with a title of "mr".
     */
    public CalcSurvival getSurvivalMr() {
        return survivalMr;
    }

    /**
     * @return Survival stats for passengers with a title of "miss".
     */
    public CalcSurvival getSurvivalMiss() {
        return survivalMiss;
    }

    /**
     * @return Survival stats for passengers with a title of "mrs".
     */
    public CalcSurvival getSurvivalMrs() {
        return survivalMrs;
    }

    /**
     * @return Survival stats for passengers with a military title.
     */
    public CalcSurvival getSurvivalMilitary() {
        return survivalMilitary;
    }

    /**
     * @return Survival stats for passengers with a noble title.
     */
    public CalcSurvival getSurvivalNobility() {
        return survivalNobility;
    }

    /**
     * @return Survival stats for passengers with a title of "dr".
     */
    public CalcSurvival getSurvivalDr() {
        return survivalDr;
    }

    /**
     * @return Survival stats for passengers with a title of "clergy".
     */
    public CalcSurvival getSurvivalClergy() {
        return survivalClergy;
    }

    /**
     * @return Survival stats on the total number of passengers.
     */
    public CalcSurvival getSurvivalTotal() {
        return survivalTotal;
    }

    /**
     * @return Survival stats for passengers that embarked from Southampton, England.
     */
    public CalcSurvival getEmbarkedS() {
        return embarkedS;
    }

    /**
     * @return Survival stats for passengers that embarked from Cherbourg, France.
     */
    public CalcSurvival getEmbarkedC() {
        return embarkedC;
    }

    /**
     * @return Survival stats for passengers that embarked from Queenstown, England.
     */
    public CalcSurvival getEmbarkedQ() {
        return embarkedQ;
    }

    /**
     * @return Histogram of embark locations.
     */
    public CalcHistogram getEmbarkedHisto() {
        return embarkedHisto;
    }

    /**
     * @return Mean age for male passengers.
     */
    public CalcMean getMeanMale() {
        return meanMale;
    }

    /**
     * @return Mean age for female passengers.
     */
    public CalcMean getMeanFemale() {
        return meanFemale;
    }

    /**
     * @return Mean fare for first class.
     */
    public CalcMean getMeanFare1() {
        return meanFare1;
    }

    /**
     * @return Mean fare for second class.
     */
    public CalcMean getMeanFare2() {
        return meanFare2;
    }

    /**
     * @return Mean fare for second class.
     */
    public CalcMean getMeanFare3() {
        return meanFare3;
    }

    /**
     * Dump all stats to stdout.
     */
    public void dump() {
        System.out.println("Mean Master: Mean Age: " + meanMaster.calculate() + " " + survivalMaster.toString());
        System.out.println("Mr.: Mean Age: " + meanMr.calculate() + " " + survivalMr.toString());
        System.out.println("Miss.: Mean Age: " + meanMiss.calculate() + " " + survivalMiss.toString());
        System.out.println("Mrs.: Mean Age: " + meanMrs.calculate() + " " + survivalMrs.toString());
        System.out.println("Military: Mean Age: " + meanMrs.calculate() + " " + survivalMilitary.toString());
        System.out.println("Clergy: Mean Age: " + meanClergy.calculate() + " " + survivalClergy.toString());
        System.out.println("Nobility: Mean Age: " + meanNobility.calculate() + " " + survivalNobility.toString());
        System.out.println("Dr: Mean Age: " + meanDr.calculate() + " " + survivalDr.toString());
        System.out.println("Total known survival: Mean Age: " + meanTotal.calculate() + " " + survivalTotal.toString());
        System.out.println();
        System.out.println("Embarked Queenstown: Mean Age: " + embarkedQ.toString());
        System.out.println("Embarked Southampton: Mean Age: " + embarkedS.toString());
        System.out.println("Embarked Cherbourg: Mean Age: " + embarkedC.toString());
        System.out.println("Most common embarked: Mean Age: " + this.embarkedHisto.max());
        System.out.println();
        System.out.println("Mean Age Male: " + this.meanMale.calculate());
        System.out.println("Mean Age Female: " + this.meanFemale.calculate());
        System.out.println();
        System.out.println("Mean Fair 1st Class: " + this.meanFare1.calculate());
        System.out.println("Mean Fair 2st Class: " + this.meanFare2.calculate());
        System.out.println("Mean Fair 3st Class: " + this.meanFare3.calculate());


    }
}
