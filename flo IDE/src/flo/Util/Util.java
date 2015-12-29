package flo.Util;

import java.util.function.Function;
import java.util.function.ToIntFunction;

/**
 * Random utility functions
 */
public class Util {

    /**
     * Finds the maximum element in a list based on the given function. Returns
     * the default value if the list is empty.
     *
     * @param list
     * @param fun
     * @return
     */
    public static <T> int maxBy(final Iterable<T> list,
        final ToIntFunction<T> fun) {
        int max = Integer.MIN_VALUE;
        for (final T t : list) {
            final int val = fun.applyAsInt(t);
            if (val > max)
                max = val;
        }
        return max;
    }

    /**
     * Applies the function to each element in the list and returns the first
     * value that matches the given element, if one exists.
     * 
     * @param elem
     * @param list
     * @param fun
     * @return
     */
    public static <T, R> T findBy(final R elem, final Iterable<T> list,
        final Function<T, R> fun) {
        for (final T t : list)
            if (fun.apply(t) == elem)
                return t;
        return null;
    }
}
