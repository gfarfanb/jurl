package com.legadi.jurl.options;

public enum Option {

    ENV("--env", "-e", 1),
    CURL("--curl", "-c", 1),
    SET("--set", "-s", 2),
    AUTH_BASIC("--auth-basic", "-ab", 1),
    AUTH_TOKEN("--auth-token", "-at", 1),
    TIMES("--times", "-t", 1),
    OPEN("--open", "-o", 0),
    MOCK("--mock", "-m", 0),
    HELP("--help", "-h", 0);

    private final String opt;
    private final String alias;
    private final int numArgs;

    private Option(String opt, String alias, int numArgs) {
        this.opt = opt;
        this.alias = alias;
        this.numArgs = numArgs;
    }

    public String getOpt() {
        return opt;
    }

    public String getAlias() {
        return alias;
    }

    public int getNumArgs() {
        return numArgs;
    }

    public static Option valueOfOpt(String opt) {
        for(Option option : values()) {
            if(option.opt.equals(opt) || option.alias.equals(opt)) {
                return option;
            }
        }
        return null;
    }
}
