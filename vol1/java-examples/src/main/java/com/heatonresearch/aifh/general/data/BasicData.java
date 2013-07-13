package com.heatonresearch.aifh.general.data;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/13/13
 * Time: 12:24 PM
 * To change this template use File | Settings | File Templates.
 */
public class BasicData implements UnsupervisedData {
    private final double[] input;
    private final double[] ideal;
    private String label;

    public BasicData(int theInputDimensions) {
        this(theInputDimensions, 0, null);
    }

    public BasicData(int theInputDimensions, int theIdealDimensions) {
        this(theInputDimensions, theIdealDimensions, null);
    }

    public BasicData(int theInputDimensions, int theIdealDimensions, String theLabel) {
        this.label = theLabel;
        this.input = new double[theInputDimensions];
        this.ideal = new double[theIdealDimensions];
    }

    public BasicData(double[] theInputData, double[] theIdealData, String theLabel) {
        this.label = theLabel;
        this.input = theInputData;
        this.ideal = theIdealData;
    }

    public BasicData(double[] theInputData, String theLabel) {
        this(theInputData, new double[0], theLabel);
    }

    public BasicData(double[] theInputData) {
        this(theInputData, null);
    }

    @Override
    public double[] getInput() {
        return input;
    }

    public double[] getIdeal() {
        return ideal;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(final String label) {
        this.label = label;
    }
}
