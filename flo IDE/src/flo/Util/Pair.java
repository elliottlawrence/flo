package flo.Util;

import java.util.ArrayList;
import java.util.List;

/**
 * Since Java is stupid and doesn't have a built in tuple type
 */
public class Pair<T1, T2> {
    public final T1 x;
    public final T2 y;

    public Pair(final T1 x, final T2 y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Converts a list of pairs into a list of elements of the first type
     *
     * @param list
     * @return
     */
    public static <T1, T2> List<T1> unzip1(final List<Pair<T1, T2>> list) {
        final ArrayList<T1> ret = new ArrayList<T1>();
        list.forEach(p -> ret.add(p.x));
        return ret;
    }
}
