package com.heatonresearch.aifh.examples.neat.hyperneat;

import org.encog.mathutil.IntPair;
import org.encog.neural.hyperneat.HyperNEATCODEC;
import org.encog.neural.hyperneat.substrate.Substrate;
import org.encog.neural.hyperneat.substrate.SubstrateFactory;
import org.encog.neural.neat.NEATNetwork;
import org.encog.neural.neat.NEATPopulation;
import org.encog.neural.neat.training.NEATGenome;

import javax.swing.*;
import java.awt.*;
import java.util.Random;

/**
 * Created by jeff on 11/21/15.
 */
public class DisplayBoxesPanel extends JPanel {
    /**
     * The serial.
     */
    private static final long serialVersionUID = 1L;
    private BoxTrialCase testCase = new BoxTrialCase(new Random());
    private NEATPopulation pop;
    private int resolution = BoxTrialCase.BASE_RESOLUTION;

    public DisplayBoxesPanel(NEATPopulation thePopulation) {
        testCase.initTestCase(0);
        this.pop = thePopulation;
    }

    @Override
    public void paint(Graphics g) {

        NEATGenome genome = (NEATGenome) this.pop.getBestGenome();
        Substrate substrate = SubstrateFactory.factorSandwichSubstrate(resolution, resolution);
        HyperNEATCODEC codec = new HyperNEATCODEC();
        NEATNetwork phenotype = (NEATNetwork) codec.decode(this.pop, substrate, genome);

        TrialEvaluation trial = new TrialEvaluation(phenotype, this.testCase);
        IntPair actualPos = trial.query(resolution);

        // clear what was there before
        g.setColor(Color.white);
        g.fillRect(0, 0, getWidth(), getHeight());

        //
        int boxWidth = this.getWidth()/resolution;
        int boxHeight = this.getHeight()/resolution;
        double delta = 2.0 / resolution;
        int index = 0;

        for(int row = 0; row < resolution; row++ ) {
            double y = -1 + (row*delta);
            int boxY = row * boxHeight;
            for(int col = 0; col< resolution; col++ ) {
                double x = -1 + (col*delta);
                int boxX = col*boxWidth;

                if( this.testCase.getPixel(x, y)>0 ) {
                    g.setColor(Color.blue);
                    g.fillRect(boxX, boxY, boxWidth, boxHeight);
                } else {
                    double d = trial.getOutput().getData(index);
                    int c = trial.normalize(d,255);
                    g.setColor(new Color(255,c,255));
                    g.fillRect(boxX, boxY, boxWidth, boxHeight);
                    g.setColor(Color.black);
                    g.drawRect(boxX, boxY, boxWidth, boxHeight);
                    g.drawRect(boxX+1, boxY+1, boxWidth-2, boxHeight-2);
                }
                index++;
            }
        }

        g.setColor(Color.red);
        g.fillRect(actualPos.getX()*boxWidth, actualPos.getY()*boxHeight, boxWidth, boxHeight);
    }

    public void createNewCase(int theResolution) {
        Random r = new Random();
        this.resolution = theResolution;
        this.testCase.initTestCase(r.nextInt(3));
        this.repaint();
    }
}
