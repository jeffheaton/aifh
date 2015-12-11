package com.heatonresearch.aifh.selection;

import java.util.ArrayList;
import java.util.List;

public class CategorySearchAxis implements SearchAxis {
    private final List<String> categories = new ArrayList<String>();
    private int currentIndex = 0;

    public CategorySearchAxis(String[] catList) {
        for(String str:catList) {
            this.categories.add(str);
        }
    }

    public List<String> getCategories() {
        return this.categories;
    }

    @Override
    public void reset() {
        this.currentIndex = 0;
    }

    @Override
    public boolean advance() {
        this.currentIndex++;
        if( this.currentIndex>=this.categories.size() ) {
            this.currentIndex = 0;
            return true;
        }
        return false;
    }

    @Override
    public Object currentState() {
        return this.categories.get(this.currentIndex);
    }
}
