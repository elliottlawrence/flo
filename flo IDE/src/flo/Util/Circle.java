package flo.Util;

public class Circle implements Shape {

    public Pnt center;
    public int r;

    public Circle(final int x, final int y, final int r) {
        center = new Pnt(x, y);
        this.r = r;
    }

    public Circle(final Pnt center, final int r) {
        this.center = center;
        this.r = r;
    }

    /**
     * Returns true if this circle contains the given point
     *
     * @param p
     * @return true if the point is in the circle
     */
    @Override
    public boolean contains(final Pnt p) {
        return Pnt.distSq(center, p) <= r * r;
    }

    @Override
    public String toString() {
        return "Circle { x = " + center.x + ", y = " + center.y + ", r = " + r
            + " }";
    }
}
