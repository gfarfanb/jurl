package com.legadi.jurl.common;

import static com.legadi.jurl.common.CommonUtils.ALPHA_NUMERIC_STRING;
import static com.legadi.jurl.common.CommonUtils.NUMERIC_STRING;
import static com.legadi.jurl.common.CommonUtils.avoidFirstZero;
import static com.legadi.jurl.common.CommonUtils.getOrDefault;
import static com.legadi.jurl.common.CommonUtils.isBlank;
import static com.legadi.jurl.common.CommonUtils.isEmpty;
import static com.legadi.jurl.common.CommonUtils.isNotBlank;
import static com.legadi.jurl.common.CommonUtils.isNotEmpty;
import static com.legadi.jurl.common.CommonUtils.isNumeric;
import static com.legadi.jurl.common.CommonUtils.nextIndex;
import static com.legadi.jurl.common.CommonUtils.nextNumber;
import static com.legadi.jurl.common.CommonUtils.nextString;
import static com.legadi.jurl.common.CommonUtils.strip;
import static com.legadi.jurl.common.CommonUtils.stripEnd;
import static com.legadi.jurl.common.CommonUtils.stripStart;
import static com.legadi.jurl.common.CommonUtils.trim;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class CommonUtilsTest {

    private static final String SYMBOLS = "|!\"#$%&/()=?¡'¿°¬´+¨*~{}[]^`,.-;:_";

    @Test
    public void isNotBlankValidation() {
        Assertions.assertTrue(isNotBlank("a"));
        Assertions.assertTrue(isNotBlank(" a"));
        Assertions.assertTrue(isNotBlank(" a "));
        Assertions.assertFalse(isNotBlank(null));
        Assertions.assertFalse(isNotBlank(""));
        Assertions.assertFalse(isNotBlank(" "));
    }

    @Test
    public void isBlankValidation() {
        Assertions.assertTrue(isBlank(null));
        Assertions.assertTrue(isBlank(""));
        Assertions.assertTrue(isBlank(" "));
        Assertions.assertFalse(isBlank("a"));
        Assertions.assertFalse(isBlank(" a"));
        Assertions.assertFalse(isBlank(" a "));
    }

    @Test
    public void isNotEmptyCollectionValidation() {
        Collection<String> collection = null;

        Assertions.assertTrue(isNotEmpty(Arrays.asList("a")));
        Assertions.assertFalse(isNotEmpty(collection));
        Assertions.assertFalse(isNotEmpty(Collections.emptyList()));
    }

    @Test
    public void isEmptyCollectionValidation() {
        Collection<String> collection = null;

        Assertions.assertTrue(isEmpty(collection));
        Assertions.assertTrue(isEmpty(Collections.emptyList()));
        Assertions.assertFalse(isEmpty(Arrays.asList("a")));
    }

    @Test
    public void isNotEmptyMapValidation() {
        Map<String, String> map = new HashMap<>();
        Map<String, String> mapNull = null;
        map.put("a", "A");

        Assertions.assertTrue(isNotEmpty(map));
        Assertions.assertFalse(isNotEmpty(mapNull));
        Assertions.assertFalse(isNotEmpty(Collections.emptyMap()));
    }

    @Test
    public void isEmptyMapValidation() {
        Map<String, String> map = new HashMap<>();
        Map<String, String> mapNull = null;
        map.put("a", "A");

        Assertions.assertTrue(isEmpty(mapNull));
        Assertions.assertTrue(isEmpty(Collections.emptyMap()));
        Assertions.assertFalse(isEmpty(map));
    }

    @Test
    public void isNotEmptyArrayValidation() {
        String[] array = null;

        Assertions.assertTrue(isNotEmpty(new String[] { "a" }));
        Assertions.assertFalse(isNotEmpty(array));
        Assertions.assertFalse(isNotEmpty(new String[0]));
    }

    @Test
    public void isEmptyArrayValidation() {
        String[] array = null;

        Assertions.assertTrue(isEmpty(array));
        Assertions.assertTrue(isEmpty(new String[0]));
        Assertions.assertFalse(isEmpty(new String[] { "a" }));
    }

    @Test
    public void trimValidation() {
        Assertions.assertNull(trim(null));
        Assertions.assertEquals("", trim(""));
        Assertions.assertEquals("a", trim(" a "));
        Assertions.assertEquals(". a .", trim(". a ."));
    }

    @Test
    public void stripValidation() {
        Assertions.assertNull(strip(null, null));
        Assertions.assertEquals("", strip("", null));
        Assertions.assertEquals("", strip("", ""));
        Assertions.assertEquals("a", strip(" a ", null));
        Assertions.assertEquals("a", strip(" a ", " "));
        Assertions.assertEquals("a", strip(" a ", ""));
        Assertions.assertEquals("a", strip(". a .", "."));
        Assertions.assertEquals("a", strip(". a .", ". "));
    }

    @Test
    public void stripStartValidation() {
        Assertions.assertNull(stripStart(null, null));
        Assertions.assertEquals("", stripStart("", null));
        Assertions.assertEquals("", stripStart("", ""));
        Assertions.assertEquals("a", stripStart(" a", null));
        Assertions.assertEquals("a", stripStart(" a", " "));
        Assertions.assertEquals("a", stripStart(" a", ""));
        Assertions.assertEquals("a", stripStart(". a", "."));
        Assertions.assertEquals("a", stripStart(". a", ". "));
    }

    @Test
    public void stripEndValidation() {
        Assertions.assertNull(stripEnd(null, null));
        Assertions.assertEquals("", stripEnd("", null));
        Assertions.assertEquals("", stripEnd("", ""));
        Assertions.assertEquals("a", stripEnd("a ", null));
        Assertions.assertEquals("a", stripEnd("a ", " "));
        Assertions.assertEquals("a", stripEnd("a ", ""));
        Assertions.assertEquals("a", stripEnd("a .", "."));
        Assertions.assertEquals("a", stripEnd("a .", ". "));
    }

    @Test
    public void isNumericValidation() {
        Assertions.assertTrue(isNumeric("0"));
        Assertions.assertTrue(isNumeric("0.0"));
        Assertions.assertTrue(isNumeric("-1"));
        Assertions.assertTrue(isNumeric("-1.1"));
        Assertions.assertTrue(isNumeric("-1.1"));
        Assertions.assertFalse(isNumeric(null));
        Assertions.assertFalse(isNumeric(""));
        Assertions.assertFalse(isNumeric(" "));
        Assertions.assertFalse(isNumeric("a"));
        Assertions.assertFalse(isNumeric(" 0"));
        Assertions.assertFalse(isNumeric(" -1"));
    }

    @Test
    public void getOrDefaultValidation() {
        Assertions.assertEquals("a", getOrDefault("a", "b"));
        Assertions.assertEquals("b", getOrDefault(null, "b"));
    }

    @Test
    public void avoidFirstZeroValidation() {
        Assertions.assertNull(avoidFirstZero(null, "1"));
        Assertions.assertEquals("", avoidFirstZero("", "1"));
        Assertions.assertEquals("a", avoidFirstZero("a", "1"));
        Assertions.assertEquals("1", avoidFirstZero("0", "1"));
        Assertions.assertEquals("10", avoidFirstZero("00", "1"));
        Assertions.assertEquals("100", avoidFirstZero("000", "1"));
        Assertions.assertEquals("1.5", avoidFirstZero("0.5", "1"));
        Assertions.assertEquals("-1", avoidFirstZero("-1", "1"));
        Assertions.assertEquals("-0.5", avoidFirstZero("-0.5", "1"));
    }

    @Test
    public void nextStringValidation() {
        String generated = nextString(1000);

        Assertions.assertEquals(1000, generated.length());

        for(char c : generated.toCharArray()) {
            Assertions.assertTrue(ALPHA_NUMERIC_STRING.contains(Character.toString(c)));
            Assertions.assertFalse(SYMBOLS.contains(Character.toString(c)));
        }
    }

    @Test
    public void nextNumberValidation() {
        String generated = nextNumber(1000);

        Assertions.assertEquals(1000, generated.length());

        for(char c : generated.toCharArray()) {
            Assertions.assertTrue(NUMERIC_STRING.contains(Character.toString(c)));
            Assertions.assertFalse(SYMBOLS.contains(Character.toString(c)));
        }
    }

    @Test
    public void nextStringDifferentSource() {
        String generated = nextString(1000, SYMBOLS.length(),
            (i, index) -> SYMBOLS.charAt(index));

        Assertions.assertEquals(1000, generated.length());

        for(char c : generated.toCharArray()) {
            Assertions.assertTrue(SYMBOLS.contains(Character.toString(c)));
            Assertions.assertFalse(ALPHA_NUMERIC_STRING.contains(Character.toString(c)));
            Assertions.assertFalse(NUMERIC_STRING.contains(Character.toString(c)));
        }
    }

    @Test
    public void nextIndexValidation() {
        for(int i = 0; i < 1000; i++) {
            int generated = nextIndex(i);

            Assertions.assertTrue(generated >= 0);
            Assertions.assertTrue(i == 0 || generated < i);
        }
    }
}
