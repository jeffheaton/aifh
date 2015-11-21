package com.heatonresearch.aifh.examples.neat.hyperneat;

import org.encog.neural.neat.NEATPopulation;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 * Display the boxes on the screen using Swing.
 */
public class DisplayBoxes extends JFrame implements ActionListener  {
    /**
     * The serial id.
     */
    private static final long serialVersionUID = 1L;
    public static final String[] RESOLUTIONS = { "11", "22", "33", "44", "55" };
    private JComboBox resolution;
    private DisplayBoxesPanel display;
    private JButton newCase;

    public DisplayBoxes(NEATPopulation thePopulation) {
        setSize(400,400);
        setLayout(new BorderLayout());
        JPanel buttonPanel = new JPanel();
        buttonPanel.add(this.resolution=new JComboBox(RESOLUTIONS));
        buttonPanel.add(newCase=new JButton("New Case"));
        this.add(buttonPanel,BorderLayout.NORTH);
        this.display = new DisplayBoxesPanel(thePopulation);
        this.add(this.display, BorderLayout.CENTER);

        this.newCase.addActionListener(this);
    }

    @Override
    public void actionPerformed(ActionEvent evt) {
        if( evt.getSource()==this.newCase ) {
            this.display.createNewCase(Integer.parseInt(this.resolution.getSelectedItem().toString()));
        }
    }
}
