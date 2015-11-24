package flo;

import java.util.ArrayList;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import flo.floGraph.BoxDefinition;
import flo.floGraph.BoxInterface;
import flo.floGraph.FloGraph;
import flo.floGraph.Input;

/**
 * The canvas where code is primarily edited.
 */
public class FloCanvas extends Canvas {

	private FloGraph floGraph;

	// Reference to the current instance so other controls can reference it
	private final FloCanvas thisCanvas = this;

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
	}

	/**
	 * Variables for drag events
	 */
	private boolean drag = false;
	private int draggedBoxID;
	private final Point dragOffset = new Point(0, 0);

	/**
	 * Hotspots for mouse events
	 */
	private ArrayList<Pair<Rectangle, Integer>> boxRectangles;
	private ArrayList<Pair<Rectangle, Integer>> boxNameRectangles;

	/**
	 * Listen for mouse down and drag events so the user can move boxes around.
	 * Also listen for double clicks so the user can edit box names.
	 */
	private final MouseListener mouseAdapter = new MouseListener() {
		@Override
		public void mouseDown(final MouseEvent e) {
			for (int i = boxRectangles.size() - 1; i >= 0; i--) {
				final Pair<Rectangle, Integer> pair = boxRectangles.get(i);
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

		@Override
		public void mouseDoubleClick(final MouseEvent e) {
			for (int i = boxNameRectangles.size() - 1; i >= 0; i--) {
				final Pair<Rectangle, Integer> pair = boxNameRectangles.get(i);
				final Rectangle rect = pair.x;
				if (rect.contains(e.x, e.y)) {
					final int ID = pair.y;

					// Create new editor
					final Text textEditor = new Text(thisCanvas, SWT.NONE);
					textEditor.setLocation(rect.x, rect.y);
					textEditor.setSize(rect.width, rect.height);
					textEditor.selectAll();
					textEditor.setFocus();

					// Save text when focus is lost
					textEditor.addFocusListener(new FocusAdapter() {
						@Override
						public void focusLost(final FocusEvent event) {
							setBoxName(ID, textEditor.getText());
							textEditor.dispose();
						}
					});

					// Save text on Return, discard on Escape
					textEditor.addTraverseListener(e1 -> {
						switch (e1.detail) {
						case SWT.TRAVERSE_RETURN:
							setBoxName(ID, textEditor.getText());
							// fall through
						case SWT.TRAVERSE_ESCAPE:
							textEditor.dispose();
							e1.doit = false;
						}
					});

					break;
				}
			}
		}

		/**
		 * Change a box's name
		 */
		private void setBoxName(final int ID, final String name) {
			final Pair<BoxInterface, Point> bip = floGraph.getCurrentBoxDefinition().getBoxes().get(ID);
			final BoxInterface bi = bip.x;
			bi.setName(name);
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
		boxRectangles = new ArrayList<Pair<Rectangle, Integer>>();
		boxNameRectangles = new ArrayList<Pair<Rectangle, Integer>>();
		final Map<Integer, Pair<BoxInterface, Point>> boxes = bd.getBoxes();
		for (final Integer ID : boxes.keySet()) {
			final Pair<BoxInterface, Point> pair = boxes.get(ID);
			drawBox(gc, pair.x, pair.y, ID);
		}
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

	private void drawBox(final GC gc, final BoxInterface bi, final Point point, final int ID) {
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

		// Add this to the list of box rectangles
		final Rectangle boxRect = new Rectangle(point.x, point.y, width, height);
		boxRectangles.add(new Pair<Rectangle, Integer>(boxRect, ID));

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
		final Rectangle boxNameRect = drawCenteredString(gc, bi.getName(), point.x + width / 2,
				point.y + topHeight / 2);

		// Add this to the list of box name rectangles
		boxNameRectangles.add(new Pair<Rectangle, Integer>(boxNameRect, ID));

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
	}

	/**
	 * Draw a string centered at a point
	 *
	 * @param gc
	 * @param string
	 * @param x
	 * @param y
	 * @return The string's bounding rectangle
	 */
	private Rectangle drawCenteredString(final GC gc, final String string, final int x, final int y) {
		final Point stringExtent = gc.textExtent(string);
		final int rectX = x - stringExtent.x / 2;
		final int rectY = y - stringExtent.y / 2;
		gc.drawText(string, rectX, rectY, true);
		return new Rectangle(rectX, rectY, stringExtent.x, stringExtent.y);
	}

	// Because Eclipse is retarded
	@SuppressWarnings("unused")
	private void removeFinal(final FloGraph fg) {
		floGraph = fg;
	}
}
