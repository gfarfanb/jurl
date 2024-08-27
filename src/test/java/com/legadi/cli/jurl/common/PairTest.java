package com.legadi.cli.jurl.common;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class PairTest {

    @Test
    public void pairValidation() {
        Pair<String, String> pair = new Pair<>("left", "right");

        Assertions.assertEquals("left", pair.getLeft());
        Assertions.assertEquals("right", pair.getRight());
    }
}
