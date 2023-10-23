package com.legadi.jurl.executor.mixer;

import static com.legadi.jurl.common.LoaderUtils.instantiate;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import com.legadi.jurl.common.Pair;
import com.legadi.jurl.exception.CommandException;

public class BodyMixerRegistry {

    private static final List<Pair<Predicate<String>, Supplier<BodyMixer>>> MIXERS = new LinkedList<>();

    static {
        registerMixer(JsonBodyMixer::new);
    }

    private BodyMixerRegistry() {}

    public static void registerMixer(String mixerClass) {
        registerMixer(() -> instantiate(mixerClass));
    }

    public static void registerMixer(Supplier<BodyMixer> mixerSupplier) {
        BodyMixer mixer = mixerSupplier.get();
        MIXERS.add(new Pair<>(type -> mixer.accepts(type), mixerSupplier));
    }

    public static BodyMixer findByBodyType(String bodyType) {
        List<BodyMixer> mixers = MIXERS
            .stream()
            .filter(p -> p.getLeft().test(bodyType))
            .map(Pair::getRight)
            .map(Supplier::get)
            .collect(Collectors.toCollection(ArrayList::new));

        if(mixers.isEmpty()) {
            throw new CommandException("Unable to obtain mixer for:" + bodyType);
        }

        BodyMixer lastMixer = mixers.get(mixers.size() - 1);
        return lastMixer;
    }
}
