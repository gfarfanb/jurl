package com.legadi.jurl.common;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class TrustedManagerTest {

    @Test
    public void instantiateTrustedManager() {
        TrustedManager trustedManager = new TrustedManager();

        Assertions.assertNull(trustedManager.getAcceptedIssuers());
        Assertions.assertDoesNotThrow(() -> trustedManager.checkClientTrusted(null, null));
        Assertions.assertDoesNotThrow(() -> trustedManager.checkServerTrusted(null, null));
    }
}
