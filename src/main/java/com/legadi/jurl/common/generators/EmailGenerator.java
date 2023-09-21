package com.legadi.jurl.common.generators;

import static com.legadi.jurl.common.CommonUtils.nextString;
import static com.legadi.jurl.common.LoaderUtils.loadInternalLines;

import java.util.List;
import java.util.Random;

import com.legadi.jurl.common.Settings;

public class EmailGenerator implements Generator {

    private static final String EMAIL_PREFIX = "EMAIL:";
    private static final int RANGE_LENGTH = 10;

    private final Random random = new Random();

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(EMAIL_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        String nickname = nextString(random.nextInt(RANGE_LENGTH) + RANGE_LENGTH);
        List<String> domains = loadInternalLines("domains.txt");
        int index = (int) (domains.size() * Math.random());
        return nickname + "@" + domains.get(index);
    }
}
