package com.heatonresearch.aifh.examples.capstone.model.milestone1;

/**
 * Used to calculate survival by male and female.
 */
public class CalcSurvival {
    /**
     * The count of males.
     */
    private int countMale;

    /**
     * The count of females.
     */
    private int countFemale;

    /**
     * The count of male survivors.
     */
    private int maleSurvive;

    /**
     * The count of female survivors.
     */
    private int femaleSurvive;

    /**
     * Update for a passenger.
     * @param male True, if passenger was male.
     * @param survived True, if passenger survived.
     */
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

    /**
     * @return The number of male survivors.
     */
    public int getMaleSurvive() {
        return maleSurvive;
    }

    /**
     * @return The number of female survivors.
     */
    public int getFemaleSurvive() {
        return femaleSurvive;
    }

    /**
     * @return The total count of passengers.
     */
    public int getCount() {
        return this.countFemale+this.countMale;
    }

    /**
     * @return The number of male passengers.
     */
    public int getCountMale() {
        return this.countMale;
    }

    /**
     * @return The number of female passengers.
     */
    public int getCountFemale() {
        return this.countFemale;
    }

    /**
     * {@inheritDoc}
     */
    @Override
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
