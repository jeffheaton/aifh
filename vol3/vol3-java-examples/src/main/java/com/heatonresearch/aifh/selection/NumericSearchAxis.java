package com.heatonresearch.aifh.selection;

public class NumericSearchAxis implements SearchAxis {
    private final double start;
    private final double stop;
    private final double step;
    private double currentState;

    public NumericSearchAxis(double start, double stop, double step) {
        this.start = start;
        this.stop = stop;
        this.step = step;
    }

    public double getStart() {
        return start;
    }

    public double getStop() {
        return stop;
    }

    public double getStep() {
        return step;
    }

    @Override
    public void reset() {
        this.currentState = this.start;
    }

    @Override
    public boolean advance() {
        this.currentState+=this.step;
        if( this.currentState>=this.stop) {
            this.currentState=this.start;
            return true;
        }
        return false;
    }

    @Override
    public Object currentState() {
        return new Double(this.currentState);
    }
}
