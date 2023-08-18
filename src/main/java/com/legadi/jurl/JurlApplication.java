package com.legadi.jurl;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.temporal.ChronoField;
import java.util.Arrays;

public class JurlApplication {

    public static void main(String[] args) throws Exception {
        if(args != null && args.length > 0) {
            System.out.println("Hello world: " + Arrays.toString(args));
        } else {
            System.out.println("Hello world!!" + LocalDate.now().toString() + "." + LocalTime.now().getLong(ChronoField.MILLI_OF_DAY));
        }
    }

}
