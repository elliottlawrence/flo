package flo.Util;

import org.eclipse.swt.graphics.Point;

public class Circle implements Shape {

    public Point center;
    public int r;

    public Circle(final int x, final int y, final int r) {
        center = new Point(x, y);
        this.r = r;
    }

    /**
     * Returns true if this circle contains the given point
     *
     * @param x
     * @param y
     * @return true if the point is in the circle
     */
    @Override
    public boolean contains(final int x, final int y) {
        final int dd = (center.x - x) * (center.x - x)
                + (center.y - y) * (center.y - y);
        return dd <= r * r;
    }

    @Override
    public String toString() {
        return "Circle { x = " + center.x + ", y = " + center.y + ", r = " + r
                + " }";
    }
}
