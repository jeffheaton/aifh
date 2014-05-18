package com.heatonresearch.aifh.examples.capstone.model.milestone1;

import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: jheaton
 * Date: 5/16/14
 * Time: 6:56 AM
 * To change this template use File | Settings | File Templates.
 */
public class CalcHistogram {
    private Map<String,Integer> histogram = new HashMap<String,Integer>();

    public void update(String key) {
        if( this.histogram.containsKey(key) ) {
            int count = this.histogram.get(key);
            this.histogram.put(key,count+1);
        } else {
            this.histogram.put(key,1);
        }
    }

    public String max() {
        int maxCount = 0;
        String result = null;

        for(String key:this.histogram.keySet()) {
            int count = this.histogram.get(key);
            if( (result==null) || (maxCount<count) || (maxCount==count && result.compareTo(key)<0)) {
                result = key;
                maxCount = count;
            }
        }
        return result;
    }

    public String min() {
        int maxCount = 0;
        String result = null;

        for(String key:this.histogram.keySet()) {
            int count = this.histogram.get(key);
            if( (result==null) || (maxCount>count) || (maxCount==count && result.compareTo(key)>0)) {
                result = key;
                maxCount = count;
            }
        }
        return result;
    }

    public Map<String,Integer> getHistogram() {
        return this.histogram;
    }
}
