package com.heatonresearch.aifh.normalize;

import java.util.HashMap;
import java.util.Map;

/**
 * Provides a mapping between categories and their indexes.
 */
public class CategoryMap {
    /**
     * Categories to index.
     */
    private final Map<String, Integer> cat2index;

    /**
     * Indexes to category.
     */
    private final Map<Integer, String> index2cat;

    /**
     * Construct the object.
     * @param classes Categories to index.
     */
    public CategoryMap(Map<String, Integer> classes) {
        this.cat2index = classes;

        this.index2cat = new HashMap<>();
        for (final Map.Entry<String, Integer> entry : classes.entrySet()) {
            this.index2cat.put(entry.getValue(), entry.getKey());
        }
    }

    /**
     * @return A mapping from category to index.
     */
    public Map<String, Integer> getCatToIndex() {
        return cat2index;
    }

    /**
     * @return A mapping from index to category.
     */
    public Map<Integer, String> getIndexToCat() {
        return index2cat;
    }
}
