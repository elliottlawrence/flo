package flo.Util;

import org.eclipse.swt.graphics.Point;

/**
 * The SWT Point class is extremely limited. And it's final. Because fuck you.
 *
 */
public class Pnt {

    public final int x;
    public final int y;

    public Pnt(final int x, final int y) {
        this.x = x;
        this.y = y;
    }

    public Pnt(final Point p) {
        x = p.x;
        y = p.y;
    }

    public Point toPoint() {
        return new Point(x, y);
    }

    /**
     * Scalar multiplication
     *
     * @param s
     * @return
     */
    public Pnt scalarMult(final double s) {
        return new Pnt((int) (x * s), (int) (y * s));
    }

    public Pnt plus(final Pnt p) {
        return new Pnt(x + p.x, y + p.y);
    }

    public Pnt minus(final Pnt p) {
        return new Pnt(x - p.x, y - p.y);
    }

    /**
     * Squared distance between two points
     *
     * @param p
     * @return
     */
    public static int distSq(final Pnt p1, final Pnt p2) {
        return (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y);
    }
}
