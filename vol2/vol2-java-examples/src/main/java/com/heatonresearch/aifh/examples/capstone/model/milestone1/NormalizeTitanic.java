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
package com.heatonresearch.aifh.examples.capstone.model.milestone1;

import au.com.bytecode.opencsv.CSVReader;
import au.com.bytecode.opencsv.CSVWriter;
import com.heatonresearch.aifh.examples.capstone.model.TitanicConfig;
import com.heatonresearch.aifh.examples.util.FormatNumeric;
import com.heatonresearch.aifh.general.data.BasicData;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This capstone project shows how to apply some of the techniques in this book to data science.  The data set used
 * is the Kaggle titanic data set.  You can find that data set here.
 * <p/>
 * http://www.kaggle.com/c/titanic-gettingStarted
 * <p/>
 * There are three parts to this assignment.
 * <p/>
 * Part 1: Obtain and normalize data, extrapolate features
 * Part 2: Cross validate and select model hyperparameters
 * Part 3: Build Kaggle submission file
 * <p/>
 * This is part 1 of the project.
 * <p/>
 * [age,sex-male,pclass,sibsp,parch,fare,embarked-c,embarked-q,embarked-s,name-mil,name-nobility,name-dr,name-clergy]
 */
public class NormalizeTitanic {

    /**
     * Analyze and generate stats for titanic data.
     *
     * @param stats    The stats for titanic.
     * @param filename The file to analyze.
     * @return The passenger count.
     * @throws IOException Errors reading file.
     */
    public static int analyze(TitanicStats stats, File filename) throws IOException {
        int count = 0;
        Map<String, Integer> headerMap = new HashMap<String, Integer>();

        InputStream istream = new FileInputStream(filename);
        CSVReader reader = new CSVReader(new InputStreamReader(istream));

        String[] header = reader.readNext();
        for (int i = 0; i < header.length; i++) {
            headerMap.put(header[i].toLowerCase(), i);
        }

        int ageIndex = headerMap.get("age");
        int nameIndex = headerMap.get("name");
        int sexIndex = headerMap.get("sex");
        int indexEmbarked = headerMap.get("embarked");
        int indexFare = headerMap.get("fare");
        int indexPclass = headerMap.get("pclass");

        int survivedIndex = -1;

        // test data does not have survived
        if (headerMap.containsKey("survived")) {
            survivedIndex = headerMap.get("survived");
        }

        String[] nextLine;

        while ((nextLine = reader.readNext()) != null) {
            count++;
            String name = nextLine[nameIndex];
            String ageStr = nextLine[ageIndex];
            String sexStr = nextLine[sexIndex];
            String embarkedStr = nextLine[indexEmbarked];

            // test data does not have survived, do not use survived boolean if using test data!
            boolean survived = false;
            if (survivedIndex != -1) {
                String survivedStr = nextLine[survivedIndex];
                survived = survivedStr.equals("1");
            }

            if (indexEmbarked != -1) {
                embarkedStr = nextLine[indexEmbarked];
            }

            // calculate average fare per class
            String strFare = nextLine[indexFare];
            if (strFare.length() > 0) {
                double fare = Double.parseDouble(strFare);
                String pclass = nextLine[indexPclass];
                if (pclass.equals("1")) {
                    stats.getMeanFare1().update(fare);
                } else if (pclass.equals("2")) {
                    stats.getMeanFare2().update(fare);
                } else if (pclass.equals("3")) {
                    stats.getMeanFare3().update(fare);
                }
            }


            boolean isMale = sexStr.equalsIgnoreCase("male");
            double age;

            // Only compute survival stats on training data
            if (survivedIndex != -1) {
                if (embarkedStr.equals("Q")) {
                    stats.getEmbarkedQ().update(isMale, survived);
                } else if (embarkedStr.equals("S")) {
                    stats.getEmbarkedS().update(isMale, survived);
                } else if (embarkedStr.equals("C")) {
                    stats.getEmbarkedC().update(isMale, survived);
                }
            }

            stats.getEmbarkedHisto().update(embarkedStr);

            // Only compute survival stats on training data.
            if (survivedIndex != -1) {
                stats.getSurvivalTotal().update(isMale, survived);
            }

            if (survivedIndex != -1) {
                if (name.contains("Master.")) {
                    stats.getSurvivalMaster().update(isMale, survived);
                } else if (name.contains("Mr.")) {
                    stats.getSurvivalMr().update(isMale, survived);
                } else if (name.contains("Miss.") || name.contains("Mlle.")) {
                    stats.getSurvivalMiss().update(isMale, survived);
                } else if (name.contains("Mrs.") || name.contains("Mme.")) {
                    stats.getSurvivalMrs().update(isMale, survived);
                } else if (name.contains("Col.") || name.contains("Capt.") || name.contains("Major.")) {
                    stats.getSurvivalMilitary().update(isMale, survived);
                } else if (name.contains("Countess.") || name.contains("Lady.") || name.contains("Sir.") || name.contains("Don.") || name.contains("Dona.") || name.contains("Jonkheer.")) {
                    stats.getSurvivalNobility().update(isMale, survived);
                } else if (name.contains("Dr.")) {
                    stats.getSurvivalDr().update(isMale, survived);
                } else if (name.contains("Rev.")) {
                    stats.getSurvivalClergy().update(isMale, survived);
                }
            }

            if (ageStr.length() > 0) {
                age = Double.parseDouble(ageStr);

                // Update general mean age for male/female
                if (isMale) {
                    stats.getMeanMale().update(age);
                } else {
                    stats.getMeanFemale().update(age);
                }

                // Update the total average age
                stats.getMeanTotal().update(age);

                if (name.contains("Master.")) {
                    stats.getMeanMaster().update(age);
                    // Only compute survival stats on training data.
                    if (survivedIndex != -1) {
                        stats.getSurvivalMaster().update(isMale, survived);
                    }
                } else if (name.contains("Mr.")) {
                    stats.getMeanMr().update(age);
                    // Only compute survival stats on training data.
                    if (survivedIndex != -1) {
                        stats.getSurvivalMr().update(isMale, survived);
                    }
                } else if (name.contains("Miss.") || name.contains("Mlle.")) {
                    stats.getMeanMiss().update(age);
                    // Only compute survival stats on training data.
                    if (survivedIndex != -1) {
                        stats.getSurvivalMiss().update(isMale, survived);
                    }
                } else if (name.contains("Mrs.") || name.contains("Mme.")) {
                    stats.getMeanMrs().update(age);
                    // Only compute survival stats on training data.
                    if (survivedIndex != -1) {
                        stats.getSurvivalMrs().update(isMale, survived);
                    }
                } else if (name.contains("Col.") || name.contains("Capt.") || name.contains("Major.")) {
                    stats.getMeanMilitary().update(age);
                    // Only compute survival stats on training data.
                    if (survivedIndex != -1) {
                        stats.getSurvivalMilitary().update(isMale, survived);
                    }
                } else if (name.contains("Countess.") || name.contains("Lady.") || name.contains("Sir.") || name.contains("Don.") || name.contains("Dona.") || name.contains("Jonkheer.")) {
                    stats.getMeanNobility().update(age);
                    // Only compute survival stats on training data.
                    if (survivedIndex != -1) {
                        stats.getSurvivalNobility().update(isMale, survived);
                    }
                } else if (name.contains("Dr.")) {
                    stats.getMeanDr().update(age);
                    // Only compute survival stats on training data.
                    if (survivedIndex != -1) {
                        stats.getSurvivalDr().update(isMale, survived);
                    }
                } else if (name.contains("Rev.")) {
                    stats.getMeanClergy().update(age);
                    // Only compute survival stats on training data.
                    if (survivedIndex != -1) {
                        stats.getSurvivalClergy().update(isMale, survived);
                    }
                }
            }
        }
        return count;
    }

    /**
     * Normalize to a range.
     *
     * @param x              The value to normalize.
     * @param dataLow        The low end of the range of the data.
     * @param dataHigh       The high end of the range of the data.
     * @param normalizedLow  The normalized low end of the range of data.
     * @param normalizedHigh The normalized high end of the range of data.
     * @return The normalized value.
     */
    public static double rangeNormalize(double x, double dataLow, double dataHigh, double normalizedLow, double normalizedHigh) {
        return ((x - dataLow)
                / (dataHigh - dataLow))
                * (normalizedHigh - normalizedLow) + normalizedLow;
    }

    public static List<BasicData> normalize(TitanicStats stats, File filename, List<String> ids,
                                            double inputLow, double inputHigh,
                                            double predictSurvive, double predictPerish) throws IOException {
        List<BasicData> result = new ArrayList<BasicData>();

        Map<String, Integer> headerMap = new HashMap<String, Integer>();

        InputStream istream = new FileInputStream(filename);
        CSVReader reader = new CSVReader(new InputStreamReader(istream));

        String[] header = reader.readNext();
        for (int i = 0; i < header.length; i++) {
            headerMap.put(header[i].toLowerCase(), i);
        }

        int ageIndex = headerMap.get("age");
        int nameIndex = headerMap.get("name");
        int sexIndex = headerMap.get("sex");
        int indexEmbarked = headerMap.get("embarked");
        int indexPclass = headerMap.get("pclass");
        int indexSibsp = headerMap.get("sibsp");
        int indexParch = headerMap.get("parch");
        int indexFare = headerMap.get("fare");
        int indexId = headerMap.get("passengerid");
        int survivedIndex = -1;

        // test data does not have survived
        if (headerMap.containsKey("survived")) {
            survivedIndex = headerMap.get("survived");
        }

        String[] nextLine;

        while ((nextLine = reader.readNext()) != null) {
            BasicData data = new BasicData(TitanicConfig.InputFeatureCount, 1);

            String name = nextLine[nameIndex];
            String sex = nextLine[sexIndex];
            String embarked = nextLine[indexEmbarked];
            String id = nextLine[indexId];

            // Add record the passenger id, if requested
            if (ids != null) {
                ids.add(id);
            }

            boolean isMale = sex.equalsIgnoreCase("male");


            // age
            double age;

            // do we have an age for this person?
            if (nextLine[ageIndex].length() == 0) {
                // age is missing, interpolate using name
                if (name.contains("Master.")) {
                    age = stats.getMeanMaster().calculate();
                } else if (name.contains("Mr.")) {
                    age = stats.getMeanMr().calculate();
                } else if (name.contains("Miss.") || name.contains("Mlle.")) {
                    age = stats.getMeanMiss().calculate();
                } else if (name.contains("Mrs.") || name.contains("Mme.")) {
                    age = stats.getMeanMrs().calculate();
                } else if (name.contains("Col.") || name.contains("Capt.") || name.contains("Major.")) {
                    age = stats.getMeanMiss().calculate();
                } else if (name.contains("Countess.") || name.contains("Lady.") || name.contains("Sir.") || name.contains("Don.") || name.contains("Dona.") || name.contains("Jonkheer.")) {
                    age = stats.getMeanNobility().calculate();
                } else if (name.contains("Dr.")) {
                    age = stats.getMeanDr().calculate();
                } else if (name.contains("Rev.")) {
                    age = stats.getMeanClergy().calculate();
                } else {
                    if (isMale) {
                        age = stats.getMeanMale().calculate();
                    } else {
                        age = stats.getMeanFemale().calculate();
                    }
                }
            } else {
                age = Double.parseDouble(nextLine[ageIndex]);

            }
            data.getInput()[0] = rangeNormalize(age, 0, 100, inputLow, inputHigh);

            // sex-male
            data.getInput()[1] = isMale ? inputHigh : inputLow;

            // pclass
            double pclass = Double.parseDouble(nextLine[indexPclass]);
            data.getInput()[2] = rangeNormalize(pclass, 1, 3, inputLow, inputHigh);

            // sibsp
            double sibsp = Double.parseDouble(nextLine[indexSibsp]);
            data.getInput()[3] = rangeNormalize(sibsp, 0, 10, inputLow, inputHigh);

            // parch
            double parch = Double.parseDouble(nextLine[indexParch]);
            data.getInput()[4] = rangeNormalize(parch, 0, 10, inputLow, inputHigh);

            // fare
            String strFare = nextLine[indexFare];
            double fare;

            if (strFare.length() == 0) {
                if (((int) pclass) == 1) {
                    fare = stats.getMeanFare1().calculate();
                } else if (((int) pclass) == 2) {
                    fare = stats.getMeanFare2().calculate();
                } else if (((int) pclass) == 3) {
                    fare = stats.getMeanFare3().calculate();
                } else {
                    // should not happen, we would have a class other than 1,2,3.
                    // however, if that DID happen, use the median class (2).
                    fare = stats.getMeanFare2().calculate();
                }
            } else {
                fare = Double.parseDouble(nextLine[indexFare]);
            }
            data.getInput()[5] = rangeNormalize(fare, 0, 500, inputLow, inputHigh);

            // embarked-c
            data.getInput()[6] = embarked.trim().equalsIgnoreCase("c") ? inputHigh : inputLow;

            // embarked-q
            data.getInput()[7] = embarked.trim().equalsIgnoreCase("q") ? inputHigh : inputLow;

            // embarked-s
            data.getInput()[8] = embarked.trim().equalsIgnoreCase("s") ? inputHigh : inputLow;

            // name-mil
            data.getInput()[9] = (name.contains("Col.") || name.contains("Capt.") || name.contains("Major.")) ? inputHigh : inputLow;

            // name-nobility
            data.getInput()[10] = (name.contains("Countess.") || name.contains("Lady.") || name.contains("Sir.") || name.contains("Don.") || name.contains("Dona.") || name.contains("Jonkheer.")) ? inputHigh : inputLow;

            // name-dr
            data.getInput()[11] = (name.contains("Dr.")) ? inputHigh : inputLow;


            // name-clergy
            data.getInput()[12] = (name.contains("Rev.")) ? inputHigh : inputLow;

            // add the new row
            result.add(data);

            // add survived, if it exists
            if (survivedIndex != -1) {
                int survived = Integer.parseInt(nextLine[survivedIndex]);
                data.getIdeal()[0] = (survived == 1) ? predictSurvive : predictPerish;
            }

        }

        return result;
    }

    /**
     * The main method.
     *
     * @param args The arguments.
     */
    public static void main(String[] args) {
        String filename;

        if (args.length != 1) {
            filename = System.getProperty("FILENAME");
            if( filename==null ) {
                System.out.println("Please call this program with a single parameter that specifies your data directory.\n" +
                        "If you are calling with gradle, consider:\n" +
                "gradle runCapstoneTitanic1 -Pdata_path=[path to your data directory]\n");
                System.exit(0);
            }
        } else {
            filename = args[0];
        }

        File dataPath = new File(filename);
        File trainingPath = new File(dataPath, TitanicConfig.TrainingFilename);
        File testPath = new File(dataPath, TitanicConfig.TestFilename);
        File normalizePath = new File(dataPath, TitanicConfig.NormDumpFilename);


        try {
            TitanicStats stats = new TitanicStats();
            analyze(stats, trainingPath);
            analyze(stats, testPath);
            stats.dump();

            List<String> ids = new ArrayList<String>();
            List<BasicData> training = normalize(stats, trainingPath, ids,
                    TitanicConfig.InputNormalizeLow,
                    TitanicConfig.InputNormalizeHigh,
                    TitanicConfig.PredictSurvive,
                    TitanicConfig.PredictPerish);

            // Write out the normalized file, mainly so that you can examine it.
            // This file is not actually used by the program.
            FileOutputStream fos = new FileOutputStream(normalizePath);
            CSVWriter csv = new CSVWriter(new OutputStreamWriter(fos));

            csv.writeNext(new String[]{
                    "id",
                    "age", "sex-male", "pclass", "sibsp", "parch", "fare",
                    "embarked-c", "embarked-q", "embarked-s", "name-mil", "name-nobility", "name-dr", "name-clergy"
            });

            int idx = 0;
            for (BasicData data : training) {
                String[] line = {
                        ids.get(idx++),
                        FormatNumeric.formatDouble(data.getInput()[0], 5),
                        FormatNumeric.formatDouble(data.getInput()[1], 5),
                        FormatNumeric.formatDouble(data.getInput()[2], 5),
                        FormatNumeric.formatDouble(data.getInput()[3], 5),
                        FormatNumeric.formatDouble(data.getInput()[4], 5),
                        FormatNumeric.formatDouble(data.getInput()[5], 5),
                        FormatNumeric.formatDouble(data.getInput()[6], 5),
                        FormatNumeric.formatDouble(data.getInput()[7], 5),
                        FormatNumeric.formatDouble(data.getInput()[8], 5),
                        FormatNumeric.formatDouble(data.getInput()[9], 5),
                        FormatNumeric.formatDouble(data.getInput()[10], 5),
                        FormatNumeric.formatDouble(data.getInput()[11], 5),
                        FormatNumeric.formatDouble(data.getInput()[12], 5),
                        FormatNumeric.formatDouble(data.getIdeal()[0], 5)

                };

                csv.writeNext(line);
            }

            csv.close();
            fos.close();

        } catch (IOException ex) {
            ex.printStackTrace();
        }


    }
}
