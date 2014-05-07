package com.heatonresearch.aifh.aco;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/7/14
 * Time: 4:42 AM
 * To change this template use File | Settings | File Templates.
 */
public interface CostGraph {
    double cost(int sourceNode, int targetNode);
    int graphSize();
}
