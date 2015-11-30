package flo.Canvas;

import java.util.ArrayList;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Path;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

import flo.Util.Circle;
import flo.Util.Hotspots;
import flo.Util.Pair;
import flo.Util.Rect;
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

	public FloCanvas(final Composite parent, final FloGraph floGraph) {
		super(parent, SWT.NO_BACKGROUND);
		this.floGraph = floGraph;

		// Listen for when the current box definition changes in any way
		this.floGraph.addCurrentBoxDefinitionObserver(e -> redraw());

		// Redraw on mouse moves
		addMouseMoveListener(e -> redraw());

		// Draw the canvas
		addPaintListener(e -> paintCanvas(e.gc));
	}

	private final FloGraph floGraph;

	public FloGraph getFloGraph() {
		return floGraph;
	}

	/**
	 * Event handlers
	 */
	CableListener cableListener = new CableListener(this);
	DoubleClickListener doubleClickListener = new DoubleClickListener(this);
	InputOutputListener inputOutputMouseOverListener = new InputOutputListener(this);
	BoxListener boxListener = new BoxListener(this);
	CanvasKeyListener canvasKeyListener = new CanvasKeyListener(this);

	/**
	 * Hotspots for mouse events
	 */

	private final Rectangle boxInterfaceRectangle = new Rectangle(0, 0, 0, 0);

	public Rectangle getBoxInterfaceRectangle() {
		return boxInterfaceRectangle;
	}

	private final Hotspots<Rect, Integer> boxRectangles = new Hotspots<Rect, Integer>();
	private final Hotspots<Rect, Integer> boxNameRectangles = new Hotspots<Rect, Integer>();
	private final Hotspots<Rect, Input> inputNameRectangles = new Hotspots<Rect, Input>();
	private final Hotspots<Circle, Input> inputCircles = new Hotspots<Circle, Input>();
	private final Hotspots<Circle, Output> outputCircles = new Hotspots<Circle, Output>();

	public Pair<Rect, Integer> getContainingBox(final int x, final int y) {
		return boxRectangles.getContainingShape(x, y);
	}

	public Pair<Rect, Integer> getContainingBoxName(final int x, final int y) {
		return boxNameRectangles.getContainingShape(x, y);
	}

	public Pair<Rect, Input> getContainingInputName(final int x, final int y) {
		return inputNameRectangles.getContainingShape(x, y);
	}

	public Pair<Circle, Input> getContainingInput(final int x, final int y) {
		return inputCircles.getContainingShape(x, y);
	}

	public Pair<Circle, Output> getContainingOutput(final int x, final int y) {
		return outputCircles.getContainingShape(x, y);
	}

	private void resetHotspots() {
		boxRectangles.clear();
		boxNameRectangles.clear();
		inputNameRectangles.clear();
		inputCircles.clear();
		outputCircles.clear();
	}

	/**
	 * Methods related to the currently clicked box
	 */

	public int getClickedBoxID() {
		return boxListener.getClickedBoxID();
	}

	/**
	 * Methods for painting the canvas
	 */

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
			final boolean isInput = bd.getBoxInterface().containsInput(pair.x.getName());
			drawBox(gc, pair.x, pair.y, ID, isInput);
		}

		// Draw the cables
		drawCables(gc);
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

	private void drawBox(final GC gc, final BoxInterface bi, final Point point, final int ID, final boolean isInput) {
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
		final Rect boxRect = new Rect(point.x, point.y, width, height);
		boxRectangles.add(new Pair<Rect, Integer>(boxRect, ID));

		// Draw the shadow
		gc.setAlpha(50);
		gc.setBackground(black);
		gc.fillRoundRectangle(point.x + SHADOW_OFFSET, point.y + SHADOW_OFFSET, width, height, ARC_RADIUS, ARC_RADIUS);

		// Draw a yellow outline if this is the currently selected box
		if (ID == getClickedBoxID()) {
			gc.setAlpha(200);
			gc.setForeground(yellow);
			gc.setLineWidth(2);
			gc.drawRoundRectangle(point.x - 1, point.y - 1, width + 2, height + 2, ARC_RADIUS + 2, ARC_RADIUS + 2);
		} else {
			// Draw the outline
			gc.setForeground(black);
			gc.drawRoundRectangle(point.x - 1, point.y - 1, width + 1, height + 1, ARC_RADIUS, ARC_RADIUS);
		}

		// Draw the background
		if (isInput)
			gc.setBackground(green);
		else
			gc.setBackground(blue);
		gc.setAlpha(255);
		gc.fillRoundRectangle(point.x, point.y, width, height, ARC_RADIUS, ARC_RADIUS);

		if (numInputs > 0) {
			if (isInput)
				gc.setBackground(darkGreen);
			else
				gc.setBackground(darkBlue);
			gc.fillRoundRectangle(point.x, point.y + topHeight, width, height - topHeight, ARC_RADIUS, ARC_RADIUS);
			gc.fillRectangle(point.x, point.y + topHeight, width, height - topHeight - ARC_RADIUS);
		}

		// Draw box name
		gc.setForeground(white);
		final Rect boxNameRect = drawCenteredString(gc, bi.getName(), point.x + width / 2, point.y + topHeight / 2);

		// Add this to the list of box name rectangles
		boxNameRectangles.add(new Pair<Rect, Integer>(boxNameRect, ID));

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
			final Rect inputRect = new Rect(rectX, rectY, textExtent.x, textExtent.y);
			inputNameRectangles.add(new Pair<Rect, Input>(inputRect, i));

			y += stringExtent.y + TEXT_PADDING;
		}
	}

	private void drawInput(final GC gc, final Input input, final int x, final int y) {
		// Draw circle
		if (input.hasCable() || input == inputOutputMouseOverListener.getMousedOverInput())
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
		if (output.hasCable() || output == inputOutputMouseOverListener.getMousedOverOutput())
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
		final boolean inputHasBeenClicked = cableListener.getInputHasBeenClicked();
		final boolean outputHasBeenClicked = cableListener.getOutputHasBeenClicked();
		final Input clickedInput = cableListener.getClickedInput();
		final Output clickedOutput = cableListener.getClickedOutput();
		final Point cableEnd = cableListener.getCableEnd();

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
	private Rect drawCenteredString(final GC gc, final String string, final int x, final int y) {
		final Point stringExtent = gc.textExtent(string);
		final int rectX = x - stringExtent.x / 2;
		final int rectY = y - stringExtent.y / 2;
		gc.drawText(string, rectX, rectY, true);
		return new Rect(rectX, rectY, stringExtent.x, stringExtent.y);
	}

	/**
	 * Color constants
	 */
	private final Color black = new Color(getDisplay(), 0, 0, 0);
	private final Color darkGray = new Color(getDisplay(), 50, 50, 50);
	private final Color mediumGray = new Color(getDisplay(), 60, 60, 60);
	private final Color white = new Color(getDisplay(), 255, 255, 255);
	private final Color blue = new Color(getDisplay(), 59, 91, 180);
	private final Color darkBlue = new Color(getDisplay(), 39, 71, 180);
	private final Color yellow = new Color(getDisplay(), 254, 205, 60);
	private final Color green = new Color(getDisplay(), 122, 194, 87);
	private final Color darkGreen = new Color(getDisplay(), 85, 160, 57);

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
}
