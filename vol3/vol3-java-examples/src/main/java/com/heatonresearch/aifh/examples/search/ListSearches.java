package com.heatonresearch.aifh.examples.search;

import com.heatonresearch.aifh.selection.GridModelSelection;
import com.heatonresearch.aifh.selection.RandomModelSelection;

import java.util.Arrays;

public class ListSearches {
    public static void main(String[] args) {

        System.out.println("Grid Search");
        GridModelSelection grid = new GridModelSelection();
        grid.addCategoryAxis(new String[] {"sigmoid", "ReLU", "tanh"});
        grid.addNumericAxis(1,10,1);
        Object[] list;

        while( (list=grid.next()) != null )
        {
            System.out.println(Arrays.toString(list));
        }

        System.out.println("Random Search");

        RandomModelSelection rnd = new RandomModelSelection();
        rnd.addCategoryAxis(new String[] {"sigmoid", "ReLU", "tanh"});
        rnd.addNumericAxis(1,10,1);

        for(int i=0;i<10;i++) {
            System.out.println(Arrays.toString(rnd.next()));
        }

    }
}
