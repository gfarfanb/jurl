package com.legadi.jurl.assertions;

import static com.legadi.jurl.common.CommonUtils.isNumeric;
import static com.legadi.jurl.common.CommonUtils.trim;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.legadi.jurl.common.Pair;

public abstract class OperatorAssertionFunction implements AssertionFunction {

    public final Pattern datePattern = Pattern.compile("^\\[(.*)\\]:(.*)$");

    @Override
    public boolean apply(String[] args) {
        if(isNumeric(args[0]) && isNumeric(args[1])) {
            return test(new BigDecimal(args[0]), new BigDecimal(args[1]));
        }

        Optional<Pair<String, String>> patternDate1 = validateDate(args[0]);
        Optional<Pair<String, String>> patternDate2 = validateDate(args[1]);

        if(patternDate1.isPresent() && patternDate2.isPresent()) {
            DateTimeFormatter formatter1 = DateTimeFormatter.ofPattern(patternDate1.get().getLeft());
            DateTimeFormatter formatter2 = DateTimeFormatter.ofPattern(patternDate2.get().getLeft());
            LocalDateTime date1 = LocalDateTime.from(formatter1.parse(patternDate1.get().getRight()));
            LocalDateTime date2 = LocalDateTime.from(formatter2.parse(patternDate2.get().getRight()));
            return test(date1, date2);
        }

        return test(args[0], args[1]);
    }

    protected abstract boolean test(BigDecimal arg1, BigDecimal arg2);

    protected abstract boolean test(LocalDateTime arg1, LocalDateTime arg2);

    protected abstract boolean test(String arg1, String arg2);

    private Optional<Pair<String, String>> validateDate(String arg) {
        if(arg == null) {
            return Optional.empty();
        }

        Matcher matcher = datePattern.matcher(trim(arg));

        if(matcher.find()) {
            String pattern = trim(matcher.group(1));
            String value = trim(matcher.group(2));
            return Optional.of(new Pair<>(pattern, value));
        } else {
            return Optional.empty();
        }
    }
}
