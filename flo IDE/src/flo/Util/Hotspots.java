package flo.Util;

import java.util.ArrayList;

/**
 * Generic class for implementing a list of hotspots
 */
@SuppressWarnings("serial")
public class Hotspots<T1 extends Shape, T2> extends ArrayList<Pair<T1, T2>> {

    public Pair<T1, T2> getContainingShape(final Pnt p) {
        for (int i = size() - 1; i >= 0; i--) {
            final Pair<T1, T2> pair = get(i);
            final T1 shape = pair.x;
            if (shape.contains(p))
                return pair;
        }
        return null;
    }
}
