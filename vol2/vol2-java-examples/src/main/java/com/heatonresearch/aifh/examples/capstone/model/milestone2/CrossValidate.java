package com.heatonresearch.aifh.examples.capstone.model.milestone2;

import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.randomize.GenerateRandom;

import java.util.ArrayList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/17/14
 * Time: 2:43 PM
 * To change this template use File | Settings | File Templates.
 */
public class CrossValidate {

    private final List<CrossValidateFold> folds = new ArrayList<CrossValidateFold>();

    public CrossValidate(int k, List<BasicData> training, GenerateRandom rnd) {
        List<BasicData> temp = new ArrayList < BasicData >();
        temp.addAll(training);

        // Setup k validation sets.
        for(int i=0;i<k;i++) {
            folds.add(new CrossValidateFold());
        }

        // Divide over the k sets.
        int leaveOutSet = 0;

        while(temp.size()>0) {
            int idx = rnd.nextInt(temp.size());
            BasicData item = temp.get(idx);
            temp.remove(idx);

            this.folds.get(leaveOutSet).getValidationSet().add(item);
            for(int includeSet = 0; includeSet <this.folds.size();includeSet++) {
                if( includeSet!=leaveOutSet ) {
                    this.folds.get(includeSet).getTrainingSet().add(item);
                }
            }

            leaveOutSet++;
            if( leaveOutSet>=k ) {
                leaveOutSet = 0;
            }
        }
    }

    public List<CrossValidateFold> getFolds() {
        return this.folds;
    }


    public double getScore() {
        double sum = 0;
        for(CrossValidateFold fold: this.folds) {
            sum+=fold.getScore();
        }
        return sum/this.folds.size();
    }

    public int size() {
        return this.folds.size();
    }
}
