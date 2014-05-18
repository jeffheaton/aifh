package com.heatonresearch.aifh.examples.capstone.model.milestone1;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/17/14
 * Time: 9:13 AM
 * To change this template use File | Settings | File Templates.
 */
public class CalcSurvival {

    private int countMale;
    private int countFemale;
    private int maleSurvive;
    private int femaleSurvive;

    public void update(boolean male, boolean survived) {
        if( male ) {
            countMale++;
        } else {
            countFemale++;
        }
        if( survived ) {
            if( male ) {
                this.maleSurvive++;
            } else {
                this.femaleSurvive++;
            }
        }
    }

    public int getMaleSurvive() {
        return maleSurvive;
    }

    public int getFemaleSurvive() {
        return femaleSurvive;
    }

    public int getCount() {
        return this.countFemale+this.countMale;
    }

    public int getCountMale() {
        return this.countMale;
    }

    public int getCountFemale() {
        return this.countFemale;
    }

    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("(");

        result.append("Count: ");
        result.append(getCount());

        if( getCount()>0 ) {
            double pct = (double)(this.femaleSurvive+this.maleSurvive)/getCount();
            result.append(", survived: ");
            result.append(pct);
        }

        if( getCountMale()>0 ) {
            double pct = (double)(this.maleSurvive)/(this.countMale);
            result.append(", male.survived: ");
            result.append(pct);
        }

        if( getCountFemale()>0 ) {
            double pct = (double)(this.femaleSurvive)/(this.countFemale);
            result.append(", female.survived: ");
            result.append(pct);
        }

        result.append(")");
        return result.toString();
    }
}
