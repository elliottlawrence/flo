package flo;

import java.util.ArrayList;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

import flo.floGraph.BoxDefinition;
import flo.floGraph.BoxInterface;
import flo.floGraph.FloGraph;
import flo.floGraph.Input;

/**
 * The canvas where code is primarily edited.
 */
public class FloCanvas extends Canvas {

	private FloGraph floGraph;

	public FloCanvas(final Composite parent, final FloGraph floGraph) {
		super(parent, SWT.NO_BACKGROUND);
		this.floGraph = floGraph;

		// Listen for when the current box definition changes in any way
		this.floGraph.addCurrentBoxDefinitionObserver(e -> redraw());

		// Draw the canvas
		addPaintListener(e -> paintCanvas(e.gc));

		// Listen for mouse clicks and drags
		addMouseListener(mouseAdapter);
		addMouseMoveListener(mouseMoveListener);

		// final DropTarget target = new DropTarget(this, DND.DROP_COPY);
		// target.setTransfer(new Transfer[] {});
		// target.addDropListener(new DropTargetAdapter() {
		//
		// @Override
		// public void dragEnter(final DropTargetEvent event) {
		// // TODO Auto-generated method stub
		//
		// }
		//
		// @Override
		// public void drop(final DropTargetEvent event) {
		// if (event.data != null)
		// System.out.println("Holy shit!");
		// }
		// });
	}

	/**
	 * Variables for mouse events
	 */
	private boolean drag = false;
	private int draggedBoxID;
	private final Point dragOffset = new Point(0, 0);
	private ArrayList<Pair<Rectangle, Integer>> rectangles = new ArrayList<Pair<Rectangle, Integer>>();

	/**
	 * Listen for mouse down and drag events so the use can move boxes around.
	 */
	private final MouseAdapter mouseAdapter = new MouseAdapter() {
		@Override
		public void mouseDown(final MouseEvent e) {
			for (int i = rectangles.size() - 1; i >= 0; i--) {
				final Pair<Rectangle, Integer> pair = rectangles.get(i);
				final Rectangle rect = pair.x;
				if (rect.contains(e.x, e.y)) {
					drag = true;
					draggedBoxID = pair.y;
					dragOffset.x = rect.x - e.x;
					dragOffset.y = rect.y - e.y;
					break;
				}
			}
		}

		@Override
		public void mouseUp(final MouseEvent e) {
			drag = false;
		}
	};

	private final MouseMoveListener mouseMoveListener = e -> {
		if (drag)
			floGraph.getCurrentBoxDefinition().setBoxLocation(draggedBoxID,
					new Point(dragOffset.x + e.x, dragOffset.y + e.y));
	};

	private final Color black = new Color(getDisplay(), 0, 0, 0);
	private final Color darkGray = new Color(getDisplay(), 50, 50, 50);
	private final Color mediumGray = new Color(getDisplay(), 60, 60, 60);
	private final Color white = new Color(getDisplay(), 255, 255, 255);
	private final Color blue = new Color(getDisplay(), 59, 91, 180);
	private final Color darkBlue = new Color(getDisplay(), 39, 71, 180);

	/**
	 * Constants required for drawing boxes etc.
	 */
	private static final int CIRCLE_RADIUS = 7;
	private static final int ARC_RADIUS = 10;
	private static final int SHADOW_OFFSET = 4;
	private static final int TEXT_PADDING = 5;
	private static final int CIRCLE_PADDING = 12;
	private static final int TOTAL_SIDE_PADDING = 50;
	private static final int OUTPUT_Y_OFFSET = 15;

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
		drawBoxInterface(gc, bd.getBoxInterface());

		// Draw the boxes
		final ArrayList<Pair<Rectangle, Integer>> rectangles = new ArrayList<Pair<Rectangle, Integer>>();
		final Map<Integer, Pair<BoxInterface, Point>> boxes = bd.getBoxes();
		for (final Integer ID : boxes.keySet()) {
			final Pair<BoxInterface, Point> pair = boxes.get(ID);
			rectangles.add(new Pair<Rectangle, Integer>(drawBox(gc, pair.x, pair.y), ID));
		}

		this.rectangles = rectangles;
	}

	private void drawBoxInterface(final GC gc, final BoxInterface bi) {
		final Point stringExtent = gc.textExtent(bi.getName());

		final Rectangle clientArea = getClientArea();
		final int width = (int) (stringExtent.x + 1.5 * TOTAL_SIDE_PADDING);
		final int height = stringExtent.y + 2 * TEXT_PADDING;
		final int x = (clientArea.width - width) / 2;

		// Draw the shadow
		gc.setAlpha(50);
		gc.setBackground(black);
		gc.fillRoundRectangle(x + SHADOW_OFFSET, OUTPUT_Y_OFFSET + SHADOW_OFFSET, width, height, ARC_RADIUS,
				ARC_RADIUS);

		// Draw the background
		gc.setAlpha(255);
		gc.setBackground(blue);
		gc.fillRoundRectangle(x, OUTPUT_Y_OFFSET, width, height, ARC_RADIUS, ARC_RADIUS);

		// Draw the box name
		gc.setForeground(white);
		drawCenteredString(gc, bi.getName(), clientArea.width / 2, OUTPUT_Y_OFFSET + height / 2);

		// Draw the output
		gc.setBackground(white);
		gc.fillOval(x + CIRCLE_PADDING, OUTPUT_Y_OFFSET + (height - CIRCLE_RADIUS) / 2, CIRCLE_RADIUS, CIRCLE_RADIUS);
	}

	private Rectangle drawBox(final GC gc, final BoxInterface bi, final Point point) {
		final Point stringExtent = gc.textExtent(bi.getName());

		// Calculate the size of the box
		int width, height;
		final int topHeight = stringExtent.y + 2 * TEXT_PADDING;
		final int numInputs = bi.getInputs().size();

		if (numInputs == 0) {
			width = stringExtent.x + TOTAL_SIDE_PADDING;
			height = topHeight;
		} else {
			int maxInputWidth = 0;
			for (final Input i : bi.getInputs()) {
				final int inputWidth = gc.textExtent(i.getName()).x;
				maxInputWidth = inputWidth > maxInputWidth ? inputWidth : maxInputWidth;
			}

			width = Math.max(maxInputWidth + CIRCLE_PADDING + 2 * TEXT_PADDING + TOTAL_SIDE_PADDING,
					stringExtent.x + TOTAL_SIDE_PADDING);
			height = stringExtent.y * (numInputs + 1) + TEXT_PADDING * (numInputs + 3);
		}

		final Rectangle rect = new Rectangle(point.x, point.y, width, height);

		// Draw the shadow
		gc.setAlpha(50);
		gc.setBackground(black);
		gc.fillRoundRectangle(point.x + SHADOW_OFFSET, point.y + SHADOW_OFFSET, width, height, ARC_RADIUS, ARC_RADIUS);

		// Draw the background
		gc.setAlpha(255);
		gc.setBackground(blue);
		gc.fillRoundRectangle(point.x, point.y, width, height, ARC_RADIUS, ARC_RADIUS);

		if (numInputs > 0) {
			gc.setBackground(darkBlue);
			gc.fillRoundRectangle(point.x, point.y + topHeight, width, height - topHeight, ARC_RADIUS, ARC_RADIUS);
			gc.fillRectangle(point.x, point.y + topHeight, width, height - topHeight - ARC_RADIUS);
		}

		// Draw box name
		drawCenteredString(gc, bi.getName(), point.x + width / 2, point.y + topHeight / 2);

		// Draw output
		gc.setBackground(white);
		gc.fillOval(point.x + width - CIRCLE_PADDING, point.y + (topHeight - CIRCLE_RADIUS) / 2, CIRCLE_RADIUS,
				CIRCLE_RADIUS);

		// Draw inputs
		int y = point.y + topHeight + TEXT_PADDING + stringExtent.y / 2;
		for (final Input i : bi.getInputs()) {
			gc.fillOval(point.x + CIRCLE_PADDING - CIRCLE_RADIUS / 2, y - CIRCLE_RADIUS / 2, CIRCLE_RADIUS,
					CIRCLE_RADIUS);
			gc.drawText(i.getName(), point.x + CIRCLE_PADDING + 2 * TEXT_PADDING, y - stringExtent.y / 2, true);
			y += stringExtent.y + TEXT_PADDING;
		}

		return rect;
	}

	private void drawCenteredString(final GC gc, final String string, final int x, final int y) {
		final Point stringExtent = gc.textExtent(string);
		gc.drawText(string, x - stringExtent.x / 2, y - stringExtent.y / 2, true);
	}

	// Because Eclipse is retarded
	private void removeFinal(final FloGraph fg) {
		floGraph = fg;
	}
}
