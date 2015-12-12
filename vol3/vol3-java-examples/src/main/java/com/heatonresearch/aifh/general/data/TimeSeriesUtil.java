package com.heatonresearch.aifh.general.data;

import java.util.ArrayList;
import java.util.List;

public class TimeSeriesUtil {
    public static List<BasicData> slidingWindow(
            double[][] dataset,
            int inputWindow,
            int predictedWindow,
            int[] inputColumns,
            int[] predictedColumns ) {

        List<BasicData> result = new ArrayList<>();

        int totalWindowSize = inputWindow+predictedWindow;

        int datasetIndex = 0;

        while((dataset.length - datasetIndex)>=totalWindowSize) {
            BasicData item = new BasicData(
                    inputWindow * inputColumns.length,
                    predictedWindow * predictedColumns.length);

            // input columns
            int inputIdx = 0;
            for (int i = 0; i < inputWindow; i++) {
                for (int j = 0; j < inputColumns.length; j++) {
                    item.getInput()[inputIdx++] = dataset[datasetIndex + i][inputColumns[j]];
                }
            }
            // predicted columns
            int predictIdx = 0;
            for (int i = 0; i < predictedWindow; i++) {
                for (int j = 0; j < predictedColumns.length; j++) {
                    item.getIdeal()[predictIdx++] = dataset[datasetIndex + inputWindow + i][predictedColumns[j]];
                }
            }

            datasetIndex++;
            // add the data item
            result.add(item);
        }

        return result;
    }
}
