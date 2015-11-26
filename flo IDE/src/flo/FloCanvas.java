package flo;

import java.util.ArrayList;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
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
import flo.floGraph.Output;

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

		// Listen for mouse clicks and drags so the user can move boxes
		addMouseListener(boxMouseListener);
		addMouseMoveListener(boxMouseMoveListener);

		// Listen for double clicks so the user can rename things
		addMouseListener(renameDoubleClickListener);

		addMouseListener(cableClickListener);
	}

	/**
	 * Hotspots for mouse events
	 */
	private final Rectangle boxInterfaceRectangle = new Rectangle(0, 0, 0, 0);
	private final ArrayList<Pair<Rectangle, Integer>> boxRectangles = new ArrayList<Pair<Rectangle, Integer>>();
	private final ArrayList<Pair<Rectangle, Integer>> boxNameRectangles = new ArrayList<Pair<Rectangle, Integer>>();
	private final ArrayList<Pair<Rectangle, Input>> inputNameRectangles = new ArrayList<Pair<Rectangle, Input>>();
	private final ArrayList<Pair<Circle, Input>> inputCircles = new ArrayList<Pair<Circle, Input>>();
	private final ArrayList<Pair<Circle, Output>> outputCircles = new ArrayList<Pair<Circle, Output>>();

	/**
	 * Variables for drag events
	 */
	private boolean drag = false;
	private int draggedBoxID;
	private final Point dragOffset = new Point(0, 0);

	/**
	 * Listen for mouse down events so the user can move boxes around.
	 */
	private final MouseAdapter boxMouseListener = new MouseAdapter() {
		@Override
		public void mouseDown(final MouseEvent e) {
			if (e.button != 1)
				return;

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
	};

	/**
	 * Listen for mouse move events so the user can move boxes around.
	 */
	private final MouseMoveListener boxMouseMoveListener = e -> {
		if (drag)
			floGraph.getCurrentBoxDefinition().setBoxLocation(draggedBoxID,
					new Point(dragOffset.x + e.x, dragOffset.y + e.y));
	};

	/**
	 * Listen for double clicks so the user can rename things
	 */
	private final MouseAdapter renameDoubleClickListener = new MouseAdapter() {
		@Override
		public void mouseDoubleClick(final MouseEvent e) {
			// Listen for double clicks on the box names
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
				}
			}

			// Listen for double clicks on the input names
			for (int i = inputNameRectangles.size() - 1; i >= 0; i--) {
				final Pair<Rectangle, Input> pair = inputNameRectangles.get(i);
				final Rectangle rect = pair.x;
				if (rect.contains(e.x, e.y)) {
					final Input input = pair.y;

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
							setInputName(input, textEditor.getText());
							textEditor.dispose();
						}
					});

					// Save text on Return, discard on Escape
					textEditor.addTraverseListener(e1 -> {
						switch (e1.detail) {
						case SWT.TRAVERSE_RETURN:
							setInputName(input, textEditor.getText());
							// fall through
						case SWT.TRAVERSE_ESCAPE:
							textEditor.dispose();
							e1.doit = false;
						}
					});

				}
			}

			// Listen for double clicks on the box interface name
			if (boxInterfaceRectangle.contains(e.x, e.y)) {
				// Create new editor
				final Text textEditor = new Text(thisCanvas, SWT.NONE);
				textEditor.setLocation(boxInterfaceRectangle.x, boxInterfaceRectangle.y);
				textEditor.setSize(boxInterfaceRectangle.width, boxInterfaceRectangle.height);
				textEditor.selectAll();
				textEditor.setFocus();

				// Save text when focus is lost
				textEditor.addFocusListener(new FocusAdapter() {
					@Override
					public void focusLost(final FocusEvent event) {
						setBoxInterfaceName(textEditor.getText());
						textEditor.dispose();
					}
				});

				// Save text on Return, discard on Escape
				textEditor.addTraverseListener(e1 -> {
					switch (e1.detail) {
					case SWT.TRAVERSE_RETURN:
						setBoxInterfaceName(textEditor.getText());
						// fall through
					case SWT.TRAVERSE_ESCAPE:
						textEditor.dispose();
						e1.doit = false;
					}
				});
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

		/**
		 * Change an input's name
		 */
		private void setInputName(final Input input, final String name) {
			input.setName(name);
		}

		/**
		 * Change the box interface's name
		 */
		private void setBoxInterfaceName(final String name) {
			floGraph.getCurrentBoxDefinition().getBoxInterface().setName(name);
		}
	};

	private boolean inputHasBeenClicked = false;
	private boolean outputHasBeenClicked = false;

	private final MouseAdapter cableClickListener = new MouseAdapter() {
		@Override
		public void mouseDown(final MouseEvent e) {
			if (e.button != 1)
				return;

			for (int i = inputCircles.size() - 1; i >= 0; i--) {
				final Pair<Circle, Input> pair = inputCircles.get(i);
				final Circle circle = pair.x;
				if (circle.contains(e.x, e.y)) {
					if (outputHasBeenClicked)
						// Reset variables
						inputHasBeenClicked = outputHasBeenClicked = false;
					else
						inputHasBeenClicked = true;
					return;
				}
			}

			for (int i = outputCircles.size() - 1; i >= 0; i--) {
				final Pair<Circle, Output> pair = outputCircles.get(i);
				final Circle circle = pair.x;
				if (circle.contains(e.x, e.y)) {
					if (inputHasBeenClicked)
						// Reset variables
						inputHasBeenClicked = outputHasBeenClicked = false;
					else
						outputHasBeenClicked = true;
					return;
				}
			}
		}
	};

	private final MouseMoveListener cableMouseMoveListener = e -> {
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
		resetHotspots();

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
		final Map<Integer, Pair<BoxInterface, Point>> boxes = bd.getBoxes();
		for (final Integer ID : boxes.keySet()) {
			final Pair<BoxInterface, Point> pair = boxes.get(ID);
			drawBox(gc, pair.x, pair.y, ID);
		}
	}

	private void resetHotspots() {
		boxRectangles.clear();
		boxNameRectangles.clear();
		inputNameRectangles.clear();
		inputCircles.clear();
		outputCircles.clear();
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

		// Draw the outline
		gc.setForeground(black);
		gc.drawRoundRectangle(x - 1, OUTPUT_Y_OFFSET - 1, width + 1, height + 1, ARC_RADIUS, ARC_RADIUS);

		// Draw the background
		gc.setAlpha(255);
		gc.setBackground(blue);
		gc.fillRoundRectangle(x, OUTPUT_Y_OFFSET, width, height, ARC_RADIUS, ARC_RADIUS);

		// Draw the box name
		gc.setForeground(white);
		drawCenteredString(gc, bi.getName(), clientArea.width / 2, OUTPUT_Y_OFFSET + height / 2);

		// Set the box interface name hotspot
		boxInterfaceRectangle.x = (clientArea.width - stringExtent.x) / 2;
		boxInterfaceRectangle.y = OUTPUT_Y_OFFSET + (height - stringExtent.y) / 2;
		boxInterfaceRectangle.width = stringExtent.x;
		boxInterfaceRectangle.height = stringExtent.y;

		// Draw the circle
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

		// Draw the outline
		gc.setForeground(black);
		gc.drawRoundRectangle(point.x - 1, point.y - 1, width + 1, height + 1, ARC_RADIUS, ARC_RADIUS);

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
		gc.setForeground(white);
		final Rectangle boxNameRect = drawCenteredString(gc, bi.getName(), point.x + width / 2,
				point.y + topHeight / 2);

		// Add this to the list of box name rectangles
		boxNameRectangles.add(new Pair<Rectangle, Integer>(boxNameRect, ID));

		// Draw output
		gc.setBackground(white);
		gc.fillOval(point.x + width - CIRCLE_PADDING, point.y + (topHeight - CIRCLE_RADIUS) / 2, CIRCLE_RADIUS,
				CIRCLE_RADIUS);

		// Add this to the list of output circles
		final Circle outputCircle = new Circle(point.x + width - CIRCLE_PADDING + CIRCLE_RADIUS / 2,
				point.y + topHeight / 2, CIRCLE_RADIUS);
		outputCircles.add(new Pair<Circle, Output>(outputCircle, bi.getOutput()));

		// Draw inputs
		int y = point.y + topHeight + TEXT_PADDING + stringExtent.y / 2;
		for (final Input i : bi.getInputs()) {
			final int rectX = point.x + CIRCLE_PADDING + 2 * TEXT_PADDING;
			final int rectY = y - stringExtent.y / 2;
			final Point textExtent = gc.textExtent(i.getName());

			gc.fillOval(point.x + CIRCLE_PADDING - CIRCLE_RADIUS / 2, y - CIRCLE_RADIUS / 2, CIRCLE_RADIUS,
					CIRCLE_RADIUS);
			gc.drawText(i.getName(), rectX, rectY, true);

			// Add this to the list of input name rectangles
			final Rectangle inputRect = new Rectangle(rectX, rectY, textExtent.x, textExtent.y);
			inputNameRectangles.add(new Pair<Rectangle, Input>(inputRect, i));

			// Add this to the list of input circles
			final Circle inputCircle = new Circle(point.x + CIRCLE_PADDING, y, CIRCLE_RADIUS);
			inputCircles.add(new Pair<Circle, Input>(inputCircle, i));

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
