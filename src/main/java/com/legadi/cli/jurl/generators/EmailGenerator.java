package com.legadi.cli.jurl.generators;

import static com.legadi.cli.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.cli.jurl.common.CommonUtils.nextString;
import static com.legadi.cli.jurl.common.LoaderUtils.loadAndCacheInternalLines;

import java.util.List;

import com.legadi.cli.jurl.common.Settings;

public class EmailGenerator implements Generator {

    public static final int RANGE_LENGTH = 10;

    @Override
    public String name() {
        return "EMAIL";
    }

    @Override
    public String getValue(Settings settings, String param) {
        String nickname;
        if(isNotBlank(param)) {
            nickname = nextString(Integer.parseInt(param));
        } else {
            nickname = nextString(random().nextInt(RANGE_LENGTH) + RANGE_LENGTH);
        }
        List<String> domains = loadAndCacheInternalLines("domains.txt");
        int index = (int) (domains.size() * Math.random());
        return nickname + "@" + domains.get(index);
    }
}
