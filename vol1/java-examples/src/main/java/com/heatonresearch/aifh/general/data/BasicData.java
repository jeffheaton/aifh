package com.heatonresearch.aifh.general.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 7/13/13
 * Time: 12:24 PM
 * To change this template use File | Settings | File Templates.
 */
public class BasicData {
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

    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append("[BasicData: input:");
        result.append(Arrays.toString(this.input));
        result.append(", ideal:");
        result.append(Arrays.toString(this.ideal));
        result.append(", label:");
        result.append(this.label);
        result.append("]");

        return result.toString();
    }

    public static List<BasicData> convertArrays(final double[][] inputData, final double[][] idealData) {
        List<BasicData> result = new ArrayList<BasicData>();
        int inputCount = inputData[0].length;
        int idealCount = idealData[0].length;

        for (int row = 0; row < inputData.length; row++) {
            BasicData dataRow = new BasicData(inputCount, idealCount);
            System.arraycopy(inputData[row], 0, dataRow.getInput(), 0, inputCount);
            System.arraycopy(idealData[row], 0, dataRow.getIdeal(), 0, idealCount);
            result.add(dataRow);
        }

        return result;
    }

}
