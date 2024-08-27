package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.CommonUtils.nextString;
import static com.legadi.cli.jurl.common.CommonUtils.trim;
import static com.legadi.cli.jurl.common.LoaderUtils.loadAndCacheInternalLines;

import java.util.List;

public abstract class NamePartGenerator implements Generator {

    protected static final String WOMAN_GENDER = "WOMAN";

    protected String createNamePart(String file) {
        List<String> content = loadAndCacheInternalLines(file);
        int count = random().nextInt(1) + 1;

        return trim(
            nextString(count, content.size(),
                (i, index) -> content.get(index) + ' ')
        );
    }
}
