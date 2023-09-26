package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextString;
import static com.legadi.jurl.common.CommonUtils.strip;
import static com.legadi.jurl.common.LoaderUtils.loadInternalLines;

import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;

import com.legadi.jurl.common.Settings;

public class LoremIpsumGenerator implements Generator {

    private static final String LOREM_PREFIX = "LOREM_IPSUM:";
    private static final int DEFAULT_LENGTH = 100;
    private static final int RANGE_LENGTH = 10;

    private final Random random = new Random();

    @Override
    public boolean accepts(Settings settings, String param) {
        return param.startsWith(LOREM_PREFIX);
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(LOREM_PREFIX, param);
        if(isNotBlank(arg)) {
            return createText(Integer.parseInt(arg));
        } else {
            return createText(DEFAULT_LENGTH);
        }
    }
    
    private String createText(int words) {
        List<String> content = loadInternalLines("lorem-ipsum.txt");
        AtomicInteger sentenceLength = new AtomicInteger(1);
        String text =  nextString(words, content.size(),
            (i, index) -> get(sentenceLength, content, i, index));

        return strip(text, ".") + ".";
    }

    private String get(AtomicInteger sentenceLength, List<String> content, int i, int index) {
        String element = content.get(index);
        int sentenceCount = sentenceLength.decrementAndGet();

        if(sentenceCount == 0) {
            sentenceLength.set(nextSencenteLength());
            element = ". "
                + element.substring(0, 1).toUpperCase()
                + element.substring(1);
        } else {
            element = ' ' + element;
        }

        return element;
    }

    private int nextSencenteLength() {
        return random.nextInt(RANGE_LENGTH) + RANGE_LENGTH;
    }
}
