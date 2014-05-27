package com.heatonresearch.aifh.examples.capstone.model.milestone1;

import java.util.HashMap;
import java.util.Map;

/**
 * Calculate a histogram of string values.  A count is kept for each unique value.
 */
public class CalcHistogram {

    /**
     * The counters for each unique value.
     */
    private Map<String,Integer> histogram = new HashMap<String,Integer>();

    public void update(String key) {

        // See if we already have an entry
        if( this.histogram.containsKey(key) ) {
            int count = this.histogram.get(key);
            this.histogram.put(key,count+1);
        } else {
            // no entry, so create one at 1.
            this.histogram.put(key,1);
        }
    }

    /**
     * @return The string key that has the greatest frequency.
     */
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

    /**
     * @return The string key with the lowest frequency.
     */
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

    /**
     * @return The histogram map.
     */
    public Map<String,Integer> getHistogram() {
        return this.histogram;
    }
}
