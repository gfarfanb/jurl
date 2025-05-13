package com.legadi.cli.jurl.model;

import java.util.ArrayList;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class GroupConfigTest {

    @Test
    public void setterGetterValidation() {
        GroupConfig model = new GroupConfig();

        model.setActive("FIRST");
        model.setCollection(new ArrayList<>());

        Assertions.assertEquals("FIRST", model.getActive());
        Assertions.assertNotNull(model.getCollection());
        Assertions.assertTrue(model.getCollection().isEmpty());
    }
}
