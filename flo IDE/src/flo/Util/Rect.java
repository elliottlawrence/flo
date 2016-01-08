package flo.Util;

import java.util.List;

import org.eclipse.swt.graphics.Rectangle;

/**
 * Essentially, just a rectangle that implements the Shape interface (which it
 * technically already does)
 */
public class Rect implements Shape {

    public final Rectangle rect;

    public Rect(final int x, final int y, final int w, final int h) {
        rect = new Rectangle(x, y, w, h);
    }

    public Rect(final Pnt loc, final int w, final int h) {
        rect = new Rectangle(loc.x, loc.y, w, h);
    }

    @Override
    public boolean contains(final Pnt p) {
        return rect.contains(p.toPoint());
    }

    /**
     * The location of the rectangle's upper right corner
     *
     * @return
     */
    public Pnt getLocation() {
        return new Pnt(rect.x, rect.y);
    }

    /**
     * Translates a rectangle by the given offset
     *
     * @return
     */
    public Rect translate(final int dx, final int dy) {
        return new Rect(rect.x + dx, rect.y + dy, rect.width, rect.height);
    }

    /**
     * Stretches the width and height by the given amount
     *
     * @return
     */
    public Rect stretch(final int dw, final int dh) {
        return new Rect(rect.x, rect.y, rect.width + dw, rect.height + dh);
    }

    /**
     * Calculates the bounds of the minimum rectangle that contains all the
     * given rectangles
     *
     * @param rects
     * @return
     */
    public static Rect getContainingRect(final List<Rect> rects) {
        int minX = Integer.MAX_VALUE;
        int minY = Integer.MAX_VALUE;
        int maxX = Integer.MIN_VALUE;
        int maxY = Integer.MIN_VALUE;

        if (rects.isEmpty())
            return new Rect(0, 0, 0, 0);

        for (final Rect rect : rects) {
            final Rectangle r = rect.rect;

            if (r.x < minX)
                minX = r.x;
            if (r.y < minY)
                minY = r.y;
            if (r.x + r.width > maxX)
                maxX = r.x + r.width;
            if (r.y + r.height > maxY)
                maxY = r.y + r.height;
        }

        return new Rect(minX, minY, maxX - minX, maxY - minY);
    }

    @Override
    public String toString() {
        return "Rect: " + rect.x + ", " + rect.y + ", " + rect.width + ", "
            + rect.height;
    }
}
