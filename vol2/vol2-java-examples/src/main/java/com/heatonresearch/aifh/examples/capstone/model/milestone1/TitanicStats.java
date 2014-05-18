package com.heatonresearch.aifh.examples.capstone.model.milestone1;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/16/14
 * Time: 6:52 AM
 * To change this template use File | Settings | File Templates.
 */
public class TitanicStats {
    private final CalcMean meanMaster = new CalcMean();
    private final CalcMean meanMr = new CalcMean();
    private final CalcMean meanMiss = new CalcMean();
    private final CalcMean meanMrs = new CalcMean();
    private final CalcMean meanMilitary = new CalcMean();
    private final CalcMean meanNobility = new CalcMean();
    private final CalcMean meanDr = new CalcMean();
    private final CalcMean meanClergy = new CalcMean();
    private final CalcMean meanTotal = new CalcMean();
    private final CalcMean meanMale = new CalcMean();
    private final CalcMean meanFemale = new CalcMean();

    private final CalcMean meanFare1 = new CalcMean();
    private final CalcMean meanFare2 = new CalcMean();
    private final CalcMean meanFare3 = new CalcMean();

    private final CalcSurvival survivalMaster = new CalcSurvival();
    private final CalcSurvival survivalMr = new CalcSurvival();
    private final CalcSurvival survivalMiss = new CalcSurvival();
    private final CalcSurvival survivalMrs = new CalcSurvival();
    private final CalcSurvival survivalMilitary = new CalcSurvival();
    private final CalcSurvival survivalNobility = new CalcSurvival();
    private final CalcSurvival survivalDr = new CalcSurvival();
    private final CalcSurvival survivalClergy = new CalcSurvival();
    private final CalcSurvival survivalTotal = new CalcSurvival();

    private final CalcSurvival embarkedS = new CalcSurvival();
    private final CalcSurvival embarkedC = new CalcSurvival();
    private final CalcSurvival embarkedQ = new CalcSurvival();

    private final CalcHistogram embarkedHisto = new CalcHistogram();

    public CalcMean getMeanMaster() {
        return meanMaster;
    }

    public CalcMean getMeanMr() {
        return meanMr;
    }

    public CalcMean getMeanMiss() {
        return meanMiss;
    }

    public CalcMean getMeanMrs() {
        return meanMrs;
    }

    public CalcMean getMeanMilitary() {
        return meanMilitary;
    }

    public CalcMean getMeanNobility() {
        return meanNobility;
    }

    public CalcMean getMeanDr() {
        return meanDr;
    }

    public CalcMean getMeanClergy() {
        return meanClergy;
    }

    public CalcMean getMeanTotal() {
        return meanTotal;
    }

    public CalcSurvival getSurvivalMaster() {
        return survivalMaster;
    }

    public CalcSurvival getSurvivalMr() {
        return survivalMr;
    }

    public CalcSurvival getSurvivalMiss() {
        return survivalMiss;
    }

    public CalcSurvival getSurvivalMrs() {
        return survivalMrs;
    }

    public CalcSurvival getSurvivalMilitary() {
        return survivalMilitary;
    }

    public CalcSurvival getSurvivalNobility() {
        return survivalNobility;
    }

    public CalcSurvival getSurvivalDr() {
        return survivalDr;
    }

    public CalcSurvival getSurvivalClergy() {
        return survivalClergy;
    }

    public CalcSurvival getSurvivalTotal() {
        return survivalTotal;
    }

    public CalcSurvival getEmbarkedS() {
        return embarkedS;
    }

    public CalcSurvival getEmbarkedC() {
        return embarkedC;
    }

    public CalcSurvival getEmbarkedQ() {
        return embarkedQ;
    }

    public CalcHistogram getEmbarkedHisto() {
        return embarkedHisto;
    }

    public CalcMean getMeanMale() {
        return meanMale;
    }

    public CalcMean getMeanFemale() {
        return meanFemale;
    }

    public CalcMean getMeanFare1() {
        return meanFare1;
    }

    public CalcMean getMeanFare2() {
        return meanFare2;
    }

    public CalcMean getMeanFare3() {
        return meanFare3;
    }

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
