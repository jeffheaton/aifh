package com.heatonresearch.aifh.normalize;

import com.heatonresearch.aifh.AIFHError;
import junit.framework.TestCase;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

@RunWith(JUnit4.class)
public class TestEquilateral extends TestCase {

    @Test(expected = AIFHError.class)
    public void testTooFew() {
        Equilateral eq = new Equilateral(2, -1, 1);

    }

    @Test
    public void testEncode() {
        Equilateral eq = new Equilateral(3, -1, 1);
        double[] d = eq.encode(1);
        assertThat(d[0], is(closeTo(-0.866, 0.001)));
        assertThat(d[1], is(closeTo(0.5, 0.001)));
    }

    @Test
    public void testDecode() {
        Equilateral eq = new Equilateral(3, -1, 1);
        double[] d0 = {0.866, 0.5};
        double[] d1 = {-0.866, 0.5};
        double[] d2 = {0, -1};
        assertThat(eq.decode(d0), is(equalTo(0)));
        assertThat(eq.decode(d1), is(equalTo(1)));
        assertThat(eq.decode(d2), is(equalTo(2)));
    }

    @Test(expected = AIFHError.class)
    public void testError() {
        Equilateral eq = new Equilateral(3, -1, 1);
        eq.encode(10);
    }
}
