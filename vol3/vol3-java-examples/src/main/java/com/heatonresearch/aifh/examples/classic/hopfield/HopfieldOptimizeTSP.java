/*
 * Encog(tm) Java Examples v3.3
 * http://www.heatonresearch.com/encog/
 * https://github.com/encog/encog-java-examples
 *
 * Copyright 2008-2014 Heaton Research, Inc.
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
package com.heatonresearch.aifh.examples.classic.hopfield;

import com.heatonresearch.aifh.energetic.BoltzmannMachine;
import com.heatonresearch.aifh.energetic.HopfieldTankNetwork;
import com.heatonresearch.aifh.learning.MLMethod;
import com.heatonresearch.aifh.learning.TrainAnneal;
import com.heatonresearch.aifh.learning.TrainPSO;

/**
 * Use a Boltzmann machine to solve the Traveling Salesman Problem.
 *
 */
public class HopfieldOptimizeTSP {

    /**
     * Coefficient for constraint that a city not be visited more than once. 500 value provided by Hopfield & Tank.
     */
    public static final double A = 500;

    /**
     * Coefficient for constraint that a path position not contain more than one city.  500 value provided
     * by Hopfield & Tank.
     */
    public static final double B = 500;

    /**
     * Coefficient for constraint that the entire matrix not contain more cells with a value of 1 than the total
     * number of cities.  200 value provided by Hopfield & Tank.
     */
    public static final double C = 200;

    /**
     * Coefficient that determines the degree to which a short path is considered.
     */
    public static final double D = 500;

    public static final int NUM_CITIES = 10;
    public static final int NEURON_COUNT = NUM_CITIES * NUM_CITIES;

    private double[][] distance;

    public double sqr(double x) {
        return x * x;
    }

    public void createCities() {
        double x1, x2, y1, y2;
        double alpha1, alpha2;

        this.distance = new double[NUM_CITIES][NUM_CITIES];

        for (int n1 = 0; n1 < NUM_CITIES; n1++) {
            for (int n2 = 0; n2 < NUM_CITIES; n2++) {
                alpha1 = ((double) n1 / NUM_CITIES) * 2 * Math.PI;
                alpha2 = ((double) n2 / NUM_CITIES) * 2 * Math.PI;
                x1 = Math.cos(alpha1);
                y1 = Math.sin(alpha1);
                x2 = Math.cos(alpha2);
                y2 = Math.sin(alpha2);
                distance[n1][n2] = Math.sqrt(sqr(x1 - x2) + sqr(y1 - y2));
            }
        }
    }


    public double lengthOfTour(double[] data) {
        double result;
        int city1,city2;

        result = 0;
        for (int path = 0; path < NUM_CITIES-1; path++) {
            for (city1 = 0; city1 < NUM_CITIES; city1++) {
                if (getGridElement(data,city1,path)>0 )
                    break;
            }
            for (city2 = 0; city2 < NUM_CITIES; city2++) {
                if (getGridElement(data,city2,path+1)>0 )
                    break;
            }
            if(city1>=NUM_CITIES || city2>=NUM_CITIES) {
                return Double.MAX_VALUE;
            }
            result += distance[city1][city2];
        }
        return result;
    }

    public double getGridElement(double[] grid, int row, int col) {
        return grid[(row*NUM_CITIES)+col]>0.5?1:0;
    }

    public double calculateEnergy(double[] state) {

        // Calculate the first term
        // Zero if and only if each row has no more than one "1"
        double term1 = 0;
        for(int x=0;x<NUM_CITIES;x++) {
            for(int i=0;i<NUM_CITIES;i++) {
                for(int j=0;j<NUM_CITIES;j++) {
                    if( j!=i ) {
                        term1+=getGridElement(state,x,i) * getGridElement(state,x,j);
                    }
                }
            }
        }
        term1*=A/2;

        // Calculate the second term
        // Zero if and only if each column has no more than one "1"
        double term2 = 0;
        for(int i=0;i<NUM_CITIES;i++) {
            for(int x=0;x<NUM_CITIES;x++) {
                for(int y=0;y<NUM_CITIES;y++) {
                    if( x!=y ) {
                        term2+=getGridElement(state,x,i) * getGridElement(state,y,i);
                    }
                }
            }
        }
        term2*=B/2;

        // Calculate the third term
        double term3 = 0;
        for(int x=0;x<NUM_CITIES;x++) {
            for(int i=0;i<NUM_CITIES;i++) {
                term3+=Math.pow(getGridElement(state,x,i)-NUM_CITIES,2);
            }
        }
        term3*=B/2;

        return term1+term2+term3+lengthOfTour(state);

    }

    public String displayTour(double[] data) {
        StringBuilder result = new StringBuilder();

        int n1, n2;
        boolean first;

        for (int path = 0; path < NUM_CITIES; path++) {
            first = true;
            result.append("[");
            for (int city = 0; city < NUM_CITIES; city++) {
                if (data[city * NUM_CITIES + path]>0.5) {
                    if (first) {
                        first = false;
                        result.append(city);
                    } else {
                        // Actually this is an invalid state, but display as best we can
                        result.append(", " + city);
                    }
                }
            }
            result.append("]");
            if (path != NUM_CITIES - 1) {
                result.append(" -> ");
            }
        }
        return result.toString();
    }


    public void run() {
        HopfieldTankNetwork network[] = new HopfieldTankNetwork[30];
        createCities();

        for(int i=0;i<network.length;i++) {
            network[i] = new HopfieldTankNetwork(NEURON_COUNT);
            network[i].reset();
        }

        TrainPSO anneal = new TrainPSO(network, new ObjectiveFunctionTSP(this));
        for(int i=0;i<1000;i++) {
            anneal.iteration();
            System.out.println("Iteration: " + i + ", energy=" + anneal.getLastError());
        }

        //this.calculateEnergy(network.getCurrentState());
        //System.out.println(displayTour(network.getCurrentState()));

        //System.out.println("Final Length: " + this.lengthOfTour(boltz.getCurrentState()) );
    }

    public static void main(String[] args) {
        HopfieldOptimizeTSP program = new HopfieldOptimizeTSP();
        program.run();
    }

}
