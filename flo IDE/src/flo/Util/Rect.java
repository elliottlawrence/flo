package flo.Util;

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

	@Override
	public boolean contains(final int x, final int y) {
		return rect.contains(x, y);
	}
}
