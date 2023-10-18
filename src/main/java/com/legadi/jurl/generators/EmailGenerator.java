package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextString;
import static com.legadi.jurl.common.LoaderUtils.loadAndCacheInternalLines;
import static com.legadi.jurl.model.GeneratorType.EMAIL;

import java.util.List;

import com.legadi.jurl.common.Settings;

public class EmailGenerator implements Generator {

    public static final int RANGE_LENGTH = 10;

    @Override
    public String tag() {
        return EMAIL.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(param);
        String nickname;
        if(isNotBlank(arg)) {
            nickname = nextString(Integer.parseInt(arg));
        } else {
            nickname = nextString(random().nextInt(RANGE_LENGTH) + RANGE_LENGTH);
        }
        List<String> domains = loadAndCacheInternalLines("domains.txt");
        int index = (int) (domains.size() * Math.random());
        return nickname + "@" + domains.get(index);
    }
}
