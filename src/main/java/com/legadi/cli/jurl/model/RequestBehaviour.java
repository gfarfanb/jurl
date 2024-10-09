package com.legadi.cli.jurl.model;

public enum RequestBehaviour {

    CURL_ONLY(true),
    PRINT_ONLY(true),
    REQUEST(false);

    private final boolean printBehaviour;

    private RequestBehaviour(boolean printBehaviour) {
        this.printBehaviour = printBehaviour;
    }

    public boolean isPrintBehaviour() {
        return printBehaviour;
    }
}
