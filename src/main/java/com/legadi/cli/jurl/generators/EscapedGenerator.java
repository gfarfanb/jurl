package com.legadi.cli.jurl.generators;

import com.legadi.cli.jurl.common.Settings;
import com.legadi.cli.jurl.common.annotations.Named;

@Named(name = "ESCAPED")
public class EscapedGenerator implements Generator {

    @Override
    public String getValue(Settings settings, String param) {
        EscapedChars escaped = EscapedChars.valueOfName(param);
        if(escaped != null) {
            return escaped.getReplacement();
        } else {
            return param;
        }
    }

    public enum EscapedChars {

        AT("@"),
        EQUAL("="),
        HASH("#"),
        DASH("-"),
        SEMICOLON(";"),
        COMMA(","),
        PERCENT("%"),
        UNDERSCORE("_"),
        QUOTATION("\""),
        PERIOD("."),
        PIPE("|"),
        COLON(":"),
        DOLLAR("$"),
        AND("&"),
        LCURLY("{"),
        RCURLY("}"),
        LSQUARE("["),
        RSQUARE("]"),
        QUESTION("?"),
        SLASH("/"),
        BACKWARD("\\"),
        EXCLAMATION("!"),
        GT(">"),
        LT("<"),
        LF(System.lineSeparator());

        private final String replacement;

        private EscapedChars(String replacement) {
            this.replacement = replacement;
        }

        public String getReplacement() {
            return replacement;
        }

        public static EscapedChars valueOfName(String name) {
            for(EscapedChars escaped : values()) {
                if(escaped.name().equalsIgnoreCase(name)) {
                    return escaped;
                }
            }
            return null;
        }
    }
}
