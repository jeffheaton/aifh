package com.heatonresearch.aifh.selection;

import java.util.ArrayList;
import java.util.List;

public abstract class ModelSelection {
    private final List<SearchAxis> searchAxises = new ArrayList<SearchAxis>();


    public List<SearchAxis> getSearchAxises() {
        return searchAxises;
    }

    public void addNumericAxis(double start, double stop, double step) {
        this.searchAxises.add(new NumericSearchAxis(start,stop,step));
    }

    public void addCategoryAxis(String[] list) {
        this.searchAxises.add(new CategorySearchAxis(list));
    }

    public abstract Object[] next();
}
