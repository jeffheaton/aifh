package com.heatonresearch.aifh.examples.classic.logic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Create a hard-wired (weights directly set) neural network for the logic gates: and, or, not & xor.
 */
public class LogicExample {

    /**
     * Display a truth table and query the neural network.
     * @param inputs The inputs.
     * @param output The output.
     */
    public static void truthTable(List<InputNeuron> inputs, RegularNeuron output) {
        double[] v = new double[inputs.size()];
        boolean done = false;

        while(!done) {

            for(int i=0;i<inputs.size();i++) {
                inputs.get(i).setValue(v[i]);
            }

            double o = output.compute();
            System.out.println(Arrays.toString(v) + " : " + o);

            // Roll forward to next row

            int i = 0;
            while(i<v.length) {
                v[i] += 1;
                if(v[i] > 1) {
                    v[i] = 0;
                    i += 1;
                } else {
                    break;
                }
            }

            if( i == v.length) {
                done = true;
            }
        }
    }

    /**
     * Create a neural network for AND.
     */
    public static void processAnd() {
        System.out.println("Boolean AND");
        List<InputNeuron> inputs = new ArrayList<>();
        inputs.add(new InputNeuron());
        inputs.add(new InputNeuron());


        RegularNeuron output = new RegularNeuron(-1.5);
        output.getParents().add(new Connection(1,inputs.get(0)));
        output.getParents().add(new Connection(1,inputs.get(1)));
        truthTable(inputs,output);
    }

    /**
     * Create a neural network for OR.
     */
    public static void processOr() {
        System.out.println("Boolean OR");
        List<InputNeuron> inputs = new ArrayList<>();
        inputs.add(new InputNeuron());
        inputs.add(new InputNeuron());


        RegularNeuron output = new RegularNeuron(-0.5);
        output.getParents().add(new Connection(1,inputs.get(0)));
        output.getParents().add(new Connection(1,inputs.get(1)));
        truthTable(inputs,output);
    }

    /**
     * Create a neural network for NOT.
     */
    public static void processNot() {
        System.out.println("Boolean NOT");
        List<InputNeuron> inputs = new ArrayList<>();
        inputs.add(new InputNeuron());

        RegularNeuron output = new RegularNeuron(0.5);
        output.getParents().add(new Connection(-1,inputs.get(0)));
        truthTable(inputs,output);
    }

    /**
     * Create a neural network for XOR.
     */
    public static void processXor() {
        System.out.println("Boolean XOR");
        List<InputNeuron> inputs = new ArrayList<>();
        inputs.add(new InputNeuron());
        inputs.add(new InputNeuron());

        List<RegularNeuron> hidden1 = new ArrayList<>();
        hidden1.add(new RegularNeuron(-0.5));
        hidden1.add(new RegularNeuron(-1.5));
        hidden1.get(0).getParents().add(new Connection(1,inputs.get(0)));
        hidden1.get(0).getParents().add(new Connection(1,inputs.get(1)));
        hidden1.get(1).getParents().add(new Connection(1,inputs.get(0)));
        hidden1.get(1).getParents().add(new Connection(1,inputs.get(1)));

        RegularNeuron hidden2 = new RegularNeuron(0.5);
        hidden2.getParents().add(new Connection(-1,hidden1.get(1)));
        RegularNeuron output = new RegularNeuron(-1.5);
        output.getParents().add(new Connection(1,hidden1.get(0)));
        output.getParents().add(new Connection(1,hidden2));
        truthTable(inputs,output);
    }


    /**
     * Program entry point.
     * @param args Not used.
     */
    public static void main(String[] args) {
        processAnd();
        processOr();
        processNot();
        processXor();

    }
}
