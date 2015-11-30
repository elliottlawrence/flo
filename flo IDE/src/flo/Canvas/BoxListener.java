package flo.Canvas;

import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;

import flo.Pair;

public class BoxListener extends MouseAdapter implements MouseMoveListener {

	private final FloCanvas floCanvas;

	public BoxListener(final FloCanvas floCanvas) {
		this.floCanvas = floCanvas;

		// Add this as a listener to the canvas
		this.floCanvas.addMouseListener(this);
		this.floCanvas.addMouseMoveListener(this);
	}

	/**
	 * The currently selected box (if there is one)
	 */
	private int clickedBoxID = -1;

	public int getClickedBoxID() {
		return clickedBoxID;
	}

	/**
	 * Variables for drag events
	 */
	private boolean drag = false;
	private int draggedBoxID;
	private final Point dragOffset = new Point(0, 0);

	@Override
	public void mouseDown(final MouseEvent e) {
		if (e.button != 1)
			return;

		// Reset variable
		clickedBoxID = -1;

		// See if the user clicked on a box
		final Pair<Rectangle, Integer> pair = floCanvas.getContainingBox(e.x, e.y);
		if (pair != null) {
			final Rectangle rect = pair.x;
			clickedBoxID = pair.y;

			drag = true;
			draggedBoxID = pair.y;
			dragOffset.x = rect.x - e.x;
			dragOffset.y = rect.y - e.y;
		}
	}

	@Override
	public void mouseUp(final MouseEvent e) {
		drag = false;
	}

	@Override
	public void mouseMove(final MouseEvent e) {
		if (drag)
			floCanvas.getFloGraph().getCurrentBoxDefinition().setBoxLocation(draggedBoxID,
					new Point(dragOffset.x + e.x, dragOffset.y + e.y));
	}
}
