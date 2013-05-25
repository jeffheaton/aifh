package com.heatonresearch.aifh.normalize;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import com.heatonresearch.aifh.AIFHError;
import junit.framework.TestCase;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import java.util.ArrayList;

@RunWith(JUnit4.class)
public class TestEquilateral extends TestCase {

    @Test (expected=AIFHError.class)
    public void testTooFew() {
        Equilateral eq = new Equilateral(2,-1,1);

    }

    @Test
    public void testEncode() {
        Equilateral eq = new Equilateral(3,-1,1);
        double[] d = eq.encode(1);
        assertThat(d[0],is(closeTo(-0.866,0.001)));
        assertThat(d[1],is(closeTo(0.5,0.001)));

    }

}
