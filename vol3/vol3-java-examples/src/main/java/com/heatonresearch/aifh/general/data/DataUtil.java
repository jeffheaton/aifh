package com.heatonresearch.aifh.general.data;

import com.heatonresearch.aifh.error.ErrorCalculation;
import com.heatonresearch.aifh.learning.ClassificationAlgorithm;
import com.heatonresearch.aifh.learning.RegressionAlgorithm;
import com.heatonresearch.aifh.randomize.GenerateRandom;
import com.heatonresearch.aifh.randomize.MersenneTwisterGenerateRandom;
import com.heatonresearch.aifh.util.ArrayUtil;

import java.util.ArrayList;
import java.util.List;

/**
 * Several dataset utilities.
 */
public class DataUtil {
    /**
     * Split a list into two sublists by randomly shuffling the values (without replacement).
     * @param list The list to split/shuffle.
     * @param ratio The size of the first retuerned list.
     * @param rnd A random number generator to split the lists.
     * @param <T> The type that the lists contain.
     * @return A list containing the two split lists.
     */
    public static <T> List<List<T>> split(final List<T> list, final double ratio, final GenerateRandom rnd) {
        List<List<T>> result = new ArrayList<>();
        int aCount = (int)(list.size() * ratio);

        List<T> a = new ArrayList<>();
        List<T> b = new ArrayList<>();
        result.add(a);
        result.add(b);

        b.addAll(list);

        for(int i=0;i<aCount;i++) {
            int idx = rnd.nextInt(0,b.size());
            a.add(b.get(idx));
            b.remove(idx);
        }

        return result;
    }

    /**
     * Split a list into two sublists by randomly shuffling the values (without replacement).
     * A new Mersenne twister random number generator will be used.
     * @param list The list to split/shuffle.
     * @param ratio The size of the first retuerned list.
     * @param <T> The type that the lists contain.
     * @return A list containing the two split lists.
     */
    public static <T> List<List<T>> split(final List<T> list, final double ratio) {
        return split(list,ratio,new MersenneTwisterGenerateRandom());
    }

    public static double calculateRegressionError(final List<BasicData> dataset,
                                                  RegressionAlgorithm model,
                                                  ErrorCalculation calc) {
        calc.clear();
        for(BasicData item: dataset) {
            double[] output = model.computeRegression(item.getInput());
            calc.updateError(output, item.getIdeal(), 1.0);
        }

        return calc.calculate();
    }

    public static double calculateClassificationError(
            List<BasicData> data,
            ClassificationAlgorithm model) {
        int total = 0;
        int correct = 0;

        for(BasicData pair : data ) {
            int ideal = ArrayUtil.indexOfLargest(pair.getIdeal());
            int actual = model.computeClassification(pair.getInput());
            if( actual==ideal )
                correct++;
            total++;
        }
        return (double)(total-correct) / (double)total;

    }

}
