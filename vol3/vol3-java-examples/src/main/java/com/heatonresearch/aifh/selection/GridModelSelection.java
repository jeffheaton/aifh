package com.heatonresearch.aifh.selection;

public class GridModelSelection extends ModelSelection {

    private boolean done = false;


    public Object[] next() {
        if( done ) {
            return null;
        }

        Object[] result = new Object[getSearchAxises().size()];

        for(int i=0;i<result.length;i++) {
            result[i] = getSearchAxises().get(i).currentState();
        }

        int idx = 0;
        while(getSearchAxises().get(idx).advance()) {
            idx++;
            if( idx>=getSearchAxises().size() ) {
                done = true;
                break;
            }
        }


        return result;
    }
}
