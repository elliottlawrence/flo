package flo;

import java.util.ArrayList;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Path;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;

import flo.floGraph.BoxDefinition;
import flo.floGraph.BoxInterface;
import flo.floGraph.Cable;
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

		// Listen for clicks on inputs and outputs so users can make cables
		addMouseListener(cableClickListener);
		addMouseMoveListener(cableMouseMoveListener);

		// Listen for mouse moves to highlight inputs and outputs
		addMouseMoveListener(inputOutputMouseOverListener);

		// Listen for keystrokes
		addKeyListener(keyListener);

		// Redraw on mouse moves
		addMouseMoveListener(e -> redraw());
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
	 * The currently selected box (if there is one)
	 */
	private int clickedBoxID = -1;

	private final KeyAdapter keyListener = new KeyAdapter() {
		@Override
		public void keyPressed(final KeyEvent e) {
			// Delete the currently selected box when the user presses backspace
			if (e.keyCode == 8 && clickedBoxID != -1) {
				floGraph.getCurrentBoxDefinition().removeBox(clickedBoxID);
				redraw();
			}
		}
	};

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

			clickedBoxID = -1;
			for (int i = boxRectangles.size() - 1; i >= 0; i--) {
				final Pair<Rectangle, Integer> pair = boxRectangles.get(i);
				final Rectangle rect = pair.x;
				if (rect.contains(e.x, e.y)) {
					clickedBoxID = pair.y;

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

					return;
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

					return;
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

				return;
			}

			// Listen for double clicks on boxes (but not on anything else)
			for (int i = boxRectangles.size() - 1; i >= 0; i--) {
				final Pair<Rectangle, Integer> pair = boxRectangles.get(i);
				final Rectangle rect = pair.x;
				if (rect.contains(e.x, e.y)) {
					final int ID = pair.y;
					final BoxInterface bi = floGraph.getCurrentBoxDefinition().getBoxes().get(ID).x;
					bi.addInput("newInput");
					redraw();
					return;
				}
			}
		}

		/**
		 * Change a box's name
		 */
		private void setBoxName(final int ID, final String name) {
			// Can't rename to nothing
			if (name.isEmpty())
				return;

			final Pair<BoxInterface, Point> bip = floGraph.getCurrentBoxDefinition().getBoxes().get(ID);
			final BoxInterface bi = bip.x;
			bi.setName(name);
		}

		/**
		 * Change an input's name, or delete the input if its name was cleared
		 */
		private void setInputName(final Input input, final String name) {
			if (name.isEmpty()) {
				// Remove input from box interface
				input.getParent().removeInput(input);

				// Remove cables attached to input
				final Cable cable = input.getCable();
				if (cable != null)
					cable.getParent().removeCable(cable);
			} else
				input.setName(name);
		}

		/**
		 * Change the box interface's name
		 */
		private void setBoxInterfaceName(final String name) {
			floGraph.getCurrentBoxDefinition().getBoxInterface().setName(name);
		}
	};

	/**
	 * Variables for input/output click events
	 */
	private boolean inputHasBeenClicked = false;
	private Input clickedInput;

	private boolean outputHasBeenClicked = false;
	private Output clickedOutput;

	private final Point cableEnd = new Point(0, 0);

	private final MouseAdapter cableClickListener = new MouseAdapter() {
		@Override
		public void mouseDown(final MouseEvent e) {
			if (e.button != 1)
				return;

			// See if user clicked an input
			for (int i = inputCircles.size() - 1; i >= 0; i--) {
				final Pair<Circle, Input> pair = inputCircles.get(i);
				final Circle circle = pair.x;
				if (circle.contains(e.x, e.y)) {
					inputHasBeenClicked = true;
					clickedInput = pair.y;

					if (outputHasBeenClicked) {
						// Make cable
						final Cable cable = new Cable(clickedOutput, clickedInput, floGraph.getCurrentBoxDefinition());
						floGraph.getCurrentBoxDefinition().addCable(cable);

						// Reset variables
						inputHasBeenClicked = outputHasBeenClicked = false;

						redraw();
					} else if (clickedInput.hasCable()) {
						// Delete cable
						final Cable cable = clickedInput.getCable();
						floGraph.getCurrentBoxDefinition().removeCable(cable);

						// Set variables
						inputHasBeenClicked = false;
						outputHasBeenClicked = true;
						clickedOutput = cable.getOutput();
					}
					return;
				}
			}

			// See if user clicked an output
			for (int i = outputCircles.size() - 1; i >= 0; i--) {
				final Pair<Circle, Output> pair = outputCircles.get(i);
				final Circle circle = pair.x;
				if (circle.contains(e.x, e.y)) {
					outputHasBeenClicked = true;
					clickedOutput = pair.y;

					if (inputHasBeenClicked) {
						// Make cable
						final Cable cable = new Cable(clickedOutput, clickedInput, floGraph.getCurrentBoxDefinition());
						floGraph.getCurrentBoxDefinition().addCable(cable);

						// Reset variables
						inputHasBeenClicked = outputHasBeenClicked = false;

						redraw();
					}
					return;
				}
			}

			// Delete existing cable otherwise
			inputHasBeenClicked = outputHasBeenClicked = false;
			redraw();
		}
	};

	private final MouseMoveListener cableMouseMoveListener = e -> {
		cableEnd.x = e.x;
		cableEnd.y = e.y;
	};

	/**
	 * Variables for mousing over inputs/outputs
	 */
	private Input mousedOverInput;
	private Output mousedOverOutput;

	private final MouseMoveListener inputOutputMouseOverListener = e -> {
		// See if user moused over an input
		mousedOverInput = null;
		for (int i = inputCircles.size() - 1; i >= 0; i--) {
			final Pair<Circle, Input> pair = inputCircles.get(i);
			final Circle circle = pair.x;
			if (circle.contains(e.x, e.y)) {
				mousedOverInput = pair.y;
				return;
			}
		}

		// See if user moused over an output
		mousedOverOutput = null;
		for (int i = outputCircles.size() - 1; i >= 0; i--) {
			final Pair<Circle, Output> pair = outputCircles.get(i);
			final Circle circle = pair.x;
			if (circle.contains(e.x, e.y)) {
				mousedOverOutput = pair.y;
				return;
			}
		}
	};

	private final Color black = new Color(getDisplay(), 0, 0, 0);
	private final Color darkGray = new Color(getDisplay(), 50, 50, 50);
	private final Color mediumGray = new Color(getDisplay(), 60, 60, 60);
	private final Color white = new Color(getDisplay(), 255, 255, 255);
	private final Color blue = new Color(getDisplay(), 59, 91, 180);
	private final Color darkBlue = new Color(getDisplay(), 39, 71, 180);
	private final Color yellow = new Color(getDisplay(), 254, 205, 60);

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

		// Draw the cables
		drawCables(gc);
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
		drawInput(gc, bi.getEndInput(), x + CIRCLE_PADDING, OUTPUT_Y_OFFSET + height / 2);
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

		// Draw a yellow outline if this is the currently selected box
		if (ID == clickedBoxID) {
			gc.setAlpha(200);
			gc.setForeground(yellow);
			gc.setLineWidth(2);
			gc.drawRoundRectangle(point.x - 1, point.y - 1, width + 2, height + 2, ARC_RADIUS, ARC_RADIUS);
		}

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
		drawOutput(gc, bi.getOutput(), point.x + width - CIRCLE_PADDING + CIRCLE_RADIUS / 2, point.y + topHeight / 2);

		// Draw inputs
		int y = point.y + topHeight + TEXT_PADDING + stringExtent.y / 2;
		for (final Input i : bi.getInputs()) {
			final int rectX = point.x + CIRCLE_PADDING + 2 * TEXT_PADDING;
			final int rectY = y - stringExtent.y / 2;
			final Point textExtent = gc.textExtent(i.getName());

			drawInput(gc, i, point.x + CIRCLE_PADDING, y);

			gc.drawText(i.getName(), rectX, rectY, true);

			// Add this to the list of input name rectangles
			final Rectangle inputRect = new Rectangle(rectX, rectY, textExtent.x, textExtent.y);
			inputNameRectangles.add(new Pair<Rectangle, Input>(inputRect, i));

			y += stringExtent.y + TEXT_PADDING;
		}
	}

	private void drawInput(final GC gc, final Input input, final int x, final int y) {
		// Draw circle
		if (input.hasCable() || input == mousedOverInput)
			gc.setBackground(yellow);
		else
			gc.setBackground(white);
		gc.fillOval(x - CIRCLE_RADIUS / 2, y - CIRCLE_RADIUS / 2, CIRCLE_RADIUS, CIRCLE_RADIUS);

		// Add this to the list of input circles
		final Circle inputCircle = new Circle(x, y, CIRCLE_RADIUS);
		inputCircles.add(new Pair<Circle, Input>(inputCircle, input));
	}

	private void drawOutput(final GC gc, final Output output, final int x, final int y) {
		// Draw circle
		if (output.hasCable() || output == mousedOverOutput)
			gc.setBackground(yellow);
		else
			gc.setBackground(white);
		gc.fillOval(x - CIRCLE_RADIUS / 2, y - CIRCLE_RADIUS / 2, CIRCLE_RADIUS, CIRCLE_RADIUS);

		// Add this to the list of output circles
		final Circle outputCircle = new Circle(x, y, CIRCLE_RADIUS);
		outputCircles.add(new Pair<Circle, Output>(outputCircle, output));
	}

	private void drawCables(final GC gc) {
		gc.setAlpha(175);

		final ArrayList<Cable> cables = floGraph.getCurrentBoxDefinition().getCables();

		for (final Cable cable : cables) {
			Circle outputCircle = new Circle(0, 0, 0), inputCircle = new Circle(0, 0, 0);

			final Output output = cable.getOutput();
			for (final Pair<Circle, Output> pair : outputCircles)
				if (pair.y == output)
					outputCircle = pair.x;

			final Input input = cable.getInput();
			for (final Pair<Circle, Input> pair : inputCircles)
				if (pair.y == input)
					inputCircle = pair.x;

			// Draw the cable
			drawCableBetweenPoints(gc, outputCircle.center, inputCircle.center);
		}

		// Draw the temporary cable if there is one
		if (inputHasBeenClicked && !outputHasBeenClicked) {
			Circle inputCircle = new Circle(0, 0, 0);
			for (final Pair<Circle, Input> pair : inputCircles)
				if (pair.y == clickedInput)
					inputCircle = pair.x;

			drawCableBetweenPoints(gc, cableEnd, inputCircle.center);

		} else if (outputHasBeenClicked && !inputHasBeenClicked) {
			Circle outputCircle = new Circle(0, 0, 0);
			for (final Pair<Circle, Output> pair : outputCircles)
				if (pair.y == clickedOutput)
					outputCircle = pair.x;

			drawCableBetweenPoints(gc, outputCircle.center, cableEnd);
		}

		gc.setAlpha(255);
	}

	private void drawCableBetweenPoints(final GC gc, final Point start, final Point end) {
		final double xWeight = .5;
		final double yWeight = .9;
		final int midX = (start.x + end.x) / 2;
		final int midY = (start.y + end.y) / 2;
		final int cx1 = (int) (xWeight * start.x + (1 - xWeight) * midX);
		final int cy1 = (int) (yWeight * start.y + (1 - yWeight) * midY);
		final int cx2 = (int) ((1 - xWeight) * midX + xWeight * end.x);
		final int cy2 = (int) ((1 - yWeight) * midY + yWeight * end.y);

		final Path path = new Path(getDisplay());
		gc.setLineCap(SWT.CAP_ROUND);
		path.moveTo(start.x, start.y);
		path.quadTo(cx1, cy1, midX, midY);
		path.quadTo(cx2, cy2, end.x, end.y);

		gc.setLineWidth(4);
		gc.setForeground(black);
		gc.drawPath(path);

		gc.setLineWidth(2);
		gc.setForeground(yellow);
		gc.drawPath(path);
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
