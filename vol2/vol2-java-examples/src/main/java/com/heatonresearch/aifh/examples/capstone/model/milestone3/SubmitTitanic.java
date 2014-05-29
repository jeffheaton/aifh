/*
 * Artificial Intelligence for Humans
 * Volume 2: Nature Inspired Algorithms
 * Java Version
 * http://www.aifh.org
 * http://www.jeffheaton.com
 *
 * Code repository:
 * https://github.com/jeffheaton/aifh
 *
 * Copyright 2014 by Jeff Heaton
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * For more information on Heaton Research copyrights, licenses
 * and trademarks visit:
 * http://www.heatonresearch.com/copyright
 */
package com.heatonresearch.aifh.examples.capstone.model.milestone3;

import au.com.bytecode.opencsv.CSVWriter;
import com.heatonresearch.aifh.examples.capstone.model.TitanicConfig;
import com.heatonresearch.aifh.examples.capstone.model.milestone1.NormalizeTitanic;
import com.heatonresearch.aifh.examples.capstone.model.milestone1.TitanicStats;
import com.heatonresearch.aifh.examples.capstone.model.milestone2.CrossValidate;
import com.heatonresearch.aifh.examples.capstone.model.milestone2.CrossValidateFold;
import com.heatonresearch.aifh.examples.capstone.model.milestone2.FitTitanic;
import com.heatonresearch.aifh.general.data.BasicData;
import com.heatonresearch.aifh.learning.RBFNetwork;

import java.io.*;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

/**
 * The final titanic milestone.  We use the test data from Kaggle and prepare a submission.
 */
public class SubmitTitanic {

    /**
     * Prepare a Kaggle submission for Titanic.
     *
     * @param dataPath    The data path.
     * @param bestNetwork The best network.
     * @param cross       The cross validated data.
     */
    public void submit(File dataPath, RBFNetwork bestNetwork, CrossValidate cross) {
        try {
            String now = new SimpleDateFormat("yyyyMMddhhmm").format(new Date());
            File trainingPath = new File(dataPath, TitanicConfig.TrainingFilename);
            File testPath = new File(dataPath, TitanicConfig.TestFilename);
            int score = (int) (cross.getScore() * 10000);
            File submitPath = new File(dataPath, "submit-" + now + "_" + score + ".csv");
            File submitInfoPath = new File(dataPath, "submit-" + now + ".txt");

            PrintWriter pw = new PrintWriter(new FileWriter(submitInfoPath));
            pw.println("Crossvalidation stats:");
            for (int i = 0; i < cross.size(); i++) {
                CrossValidateFold fold = cross.getFolds().get(i);
                pw.println("Fold #" + (i + 1) + " : Score: " + fold.getScore());
            }
            pw.println("Average Score: " + cross.getScore());
            pw.println();
            pw.println(Arrays.toString(bestNetwork.getLongTermMemory()));
            pw.close();


            FileOutputStream fos = new FileOutputStream(submitPath);
            CSVWriter csv = new CSVWriter(new OutputStreamWriter(fos));
            csv.writeNext(new String[]{"PassengerId", "Survived"});

            TitanicStats stats = new TitanicStats();
            NormalizeTitanic.analyze(stats, trainingPath);
            NormalizeTitanic.analyze(stats, testPath);

            List<String> ids = new ArrayList<String>();
            List<BasicData> training = NormalizeTitanic.normalize(stats, testPath, ids,
                    TitanicConfig.InputNormalizeLow,
                    TitanicConfig.InputNormalizeHigh,
                    TitanicConfig.PredictSurvive,
                    TitanicConfig.PredictPerish);

            int idx = 0;
            for (BasicData data : training) {
                double[] output = bestNetwork.computeRegression(data.getInput());
                int survived = output[0] > 0.5 ? 1 : 0;

                String[] line = {ids.get(idx), "" + survived};
                csv.writeNext(line);
                idx++;
            }

            csv.close();
            fos.close();
        } catch (IOException ex) {
            ex.printStackTrace();
        }

    }

    /**
     * The main method.
     *
     * @param args The path to the data file.
     */
    public static void main(String[] args) {
        String filename;
        if (args.length != 1) {
            filename = System.getProperty("FILENAME");
            if( filename==null ) {
                System.out.println("Please call this program with a single parameter that specifies your data directory.\n" +
                        "If you are calling with gradle, consider:\n" +
                        "gradle runCapstoneTitanic3 -Pdata_path=[path to your data directory]\n");
                System.exit(0);
            }
        } else {
            filename = args[0];
        }

        File dataPath = new File(filename);

        FitTitanic fit = new FitTitanic();
        fit.process(dataPath);

        RBFNetwork bestNetwork = fit.getBestNetwork();

        SubmitTitanic submit = new SubmitTitanic();
        submit.submit(dataPath, bestNetwork, fit.getCrossvalidation());
    }
}
