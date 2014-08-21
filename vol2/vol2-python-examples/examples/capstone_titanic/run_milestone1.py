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