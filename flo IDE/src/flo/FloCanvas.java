package flo;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

/**
 * The canvas where code is primarily edited.
 */
public class FloCanvas extends Canvas {

	private final FloGraph floGraph;

	private final Color darkGray = new Color(getDisplay(), 50, 50, 50);
	private final Color mediumGray = new Color(getDisplay(), 60, 60, 60);
	private final Color white = new Color(getDisplay(), 255, 255, 255);
	private final Color blue = new Color(getDisplay(), 29, 61, 150);

	public FloCanvas(final Composite parent, final FloGraph floGraph) {
		super(parent, SWT.NO_BACKGROUND);
		this.floGraph = floGraph;

		// Listen for when the current box definition changes
		this.floGraph.addBoxDefinitionSelectedObserver(e -> redraw());

		final BoxDefinition currentBoxDefinition = this.floGraph.getCurrentBoxDefinition();
		if (currentBoxDefinition != null) {
			// Listen for when new boxes are added
			currentBoxDefinition.addBoxAddedObserver(e -> redraw());

			// Listen for when the box interface is renamed
			currentBoxDefinition.getBoxInterface().addBoxInterfaceRenamedObserver(e -> redraw());
		}

		addPaintListener(e -> paintCanvas(e.gc));
	}

	private void paintCanvas(final GC gc) {
		// Draw background
		gc.setBackground(darkGray);
		final Point size = getSize();
		gc.fillRectangle(0, 0, size.x, size.y);

		// Draw lines
		gc.setForeground(mediumGray);
		final int lineSeparationWidth = 50;
		for (int i = lineSeparationWidth; i < size.x; i += lineSeparationWidth)
			gc.drawLine(i, 0, i, size.y);
		for (int i = lineSeparationWidth; i < size.y; i += lineSeparationWidth)
			gc.drawLine(0, i, size.x, i);

		// Draw the box interface
		final BoxDefinition bd = floGraph.getCurrentBoxDefinition();
		if (bd == null)
			return;
		final BoxInterface bi = bd.getBoxInterface();

		// Draw the box name
		gc.setForeground(white);
		final Point stringExtent = gc.stringExtent(bi.getName());
		gc.drawText(bi.getName(), (getClientArea().width - stringExtent.x) / 2, 10);

		// Draw the boxes
		gc.setBackground(blue);
		for (final Pair<BoxInterface, Point> pair : bd.getBoxes().values()) {
			final BoxInterface box = pair.x;
			final Point point = pair.y;
			gc.fillRoundRectangle(point.x, point.y, 100, 75, 15, 15);
		}
	}
}
