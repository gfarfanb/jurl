package com.legadi.jurl.generators;

import static com.legadi.jurl.common.CommonUtils.avoidFirstZero;
import static com.legadi.jurl.common.CommonUtils.nextNumber;

public abstract class NumberGenerator implements Generator {

    public static final int DEFAULT_LENGTH = 5;
    public static final int DEFAULT_DECIMAL_LENGTH = 2;

    private static final String ZERO_REPLACER = "1";

    protected String numberPart(int length) {
        return avoidFirstZero(nextNumber(length), ZERO_REPLACER);
    }
}
