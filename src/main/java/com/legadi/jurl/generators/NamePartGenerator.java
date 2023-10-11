package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.nextString;
import static com.legadi.jurl.common.LoaderUtils.loadInternalLines;

import java.util.List;

public abstract class NamePartGenerator implements Generator {

    protected static final String WOMAN_GENDER = "WOMAN";

    protected String createNamePart(String file) {
        List<String> content = loadInternalLines(file);
        int count = random().nextInt(1) + 1;

        return nextString(count, content.size(),
            (i, index) -> content.get(index) + ' ').trim();
    }
}
