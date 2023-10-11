package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.nextString;
import static com.legadi.jurl.common.CommonUtils.strip;
import static com.legadi.jurl.common.LoaderUtils.loadInternalLines;
import static com.legadi.jurl.model.GeneratorType.LOREM_IPSUM;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import com.legadi.jurl.common.Settings;

public class LoremIpsumGenerator implements Generator {

    public static final int DEFAULT_WORDS = 100;

    private static final int RANGE_LENGTH = 10;

    @Override
    public String tag() {
        return LOREM_IPSUM.tag();
    }

    @Override
    public String getValue(Settings settings, String param) {
        String arg = extractArg(param);
        if(isNotBlank(arg)) {
            return createText(Integer.parseInt(arg));
        } else {
            return createText(DEFAULT_WORDS);
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
        return random().nextInt(RANGE_LENGTH) + RANGE_LENGTH;
    }
}
