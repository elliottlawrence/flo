package flo.Canvas;

import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
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

    private final FloGraph floGraph;

    // Event handlers
    private final CableListener cableListener = new CableListener(this);
    private final InputOutputListener inputOutputMouseOverListener =
            new InputOutputListener(this);
    private final CanvasMouseListener boxListener =
            new CanvasMouseListener(this);

    // Hotspots for mouse events
    private final Hotspots<Rect, Integer> boxRects =
            new Hotspots<Rect, Integer>();
    private final Hotspots<Rect, Integer> boxNameRects =
            new Hotspots<Rect, Integer>();
    private final Hotspots<Rect, Input> inputNameRects =
            new Hotspots<Rect, Input>();
    private final Hotspots<Circle, Input> inputCircles =
            new Hotspots<Circle, Input>();
    private final Hotspots<Circle, Output> outputCircles =
            new Hotspots<Circle, Output>();

    // Drawing jobs
    private final Drawer drawer = new Drawer();

    // Zoom and scale
    private double zoom = 1.0;
    private Point offset = new Point(0, 0);

    /**
     * Create a FloCanvas used for editing the given FloGraph
     *
     * @param parent
     * @param floGraph
     */
    public FloCanvas(final Composite parent, final FloGraph floGraph) {
        super(parent, SWT.NO_BACKGROUND);
        this.floGraph = floGraph;

        // Anonymous event handlers
        new DoubleClickListener(this);
        new CanvasKeyListener(this);

        // Listen for when the current box definition changes in any way
        this.floGraph.addCurrentBoxDefinitionObserver(e -> redraw());

        // Redraw on mouse moves
        addMouseMoveListener(e -> redraw());

        // Draw the canvas
        addPaintListener(e -> paint(e.gc));
    }

    // Methods related to floGraph

    public FloGraph getFloGraph() {
        return floGraph;
    }

    // Methods related to hotspots

    public Pair<Rect, Integer> getContainingBox(final int x, final int y) {
        return boxRects.getContainingShape(x, y);
    }

    public Pair<Rect, Integer> getContainingBoxName(final int x, final int y) {
        return boxNameRects.getContainingShape(x, y);
    }

    public Pair<Rect, Input> getContainingInputName(final int x, final int y) {
        return inputNameRects.getContainingShape(x, y);
    }

    public Pair<Circle, Input> getContainingInput(final int x, final int y) {
        return inputCircles.getContainingShape(x, y);
    }

    public Pair<Circle, Output> getContainingOutput(final int x, final int y) {
        return outputCircles.getContainingShape(x, y);
    }

    private void resetHotspots() {
        boxRects.clear();
        boxNameRects.clear();
        inputNameRects.clear();
        inputCircles.clear();
        outputCircles.clear();
    }

    /**
     * Calculates the bounds of the minimum rectangle that contains all the
     * boxes
     *
     * @return
     */
    private Rectangle getContainingRectsForBoxes() {
        int minX = Integer.MAX_VALUE;
        int minY = Integer.MAX_VALUE;
        int maxX = Integer.MIN_VALUE;
        int maxY = Integer.MIN_VALUE;

        if (boxRects.isEmpty()) {
            final Point center = getDefaultBoxLocation();
            return new Rectangle(center.x, center.y, 0, 0);
        }

        for (final Pair<Rect, Integer> p : boxRects) {
            final Rectangle r = p.x.rect;

            if (r.x < minX)
                minX = r.x;
            if (r.y < minY)
                minY = r.y;
            if (r.x + r.width > maxX)
                maxX = r.x + r.width;
            if (r.y + r.height > maxY)
                maxY = r.y + r.height;
        }

        return new Rectangle(minX, minY, maxX - minX, maxY - minY);
    }

    // Methods related to the currently clicked box

    public int getClickedBoxID() {
        return boxListener.getClickedBoxID();
    }

    public void setClickedBoxID(final int ID) {
        boxListener.setClickedBoxID(ID);
    }

    // Methods related to zoom and offset

    public double getZoom() {
        return zoom;
    }

    public void setZoom(final double zoom) {
        this.zoom = zoom;

        // Adjust the offset so we're zooming into the center of the canvas

        redraw();
    }

    public Point getOffset() {
        return offset;
    }

    public void setOffset(final Point newOffset) {
        offset = newOffset;
    }

    /**
     * Scales a length based on the current zoom level
     *
     * @param x
     * @return
     */
    private int scale(final int x) {
        return (int) (zoom * x);
    }

    /**
     * Converts relative coordinates to absolute coordinates
     *
     * @param p
     * @return
     */
    private Point relToAbs(final Point p) {
        return new Point(scale(p.x), scale(p.y));
    }

    /**
     * Converts absolute coordinates to relative coordinates
     *
     * @param p
     * @return
     */
    public Point absToRel(final Point p) {
        return new Point((int) (p.x / zoom), (int) (p.y / zoom));
    }

    public Point getDefaultBoxLocation() {
        final Point size = getSize();
        return absToRel(new Point(size.x / 2, size.y / 2));
    }

    // Methods for painting the canvas

    private void paint(final GC gc) {
        resetHotspots();

        // Set GC properties
        gc.setFont(new Font(getDisplay(), ".SF NS Text", scale(13), SWT.NONE));
        gc.setLineCap(SWT.CAP_ROUND);

        paintCanvas(gc);

        // Draw everything
        drawer.draw(gc);
    }

    private void paintCanvas(final GC gc) {
        // Draw background
        final Point size = getSize();
        gc.setBackground(darkGray);
        gc.fillRectangle(0, 0, size.x, size.y);

        // Draw lines
        gc.setForeground(mediumGray);
        final int lineSeparationWidth = 50;
        for (int i = lineSeparationWidth; i < size.x; i += lineSeparationWidth)
            gc.drawLine(i, 0, i, size.y);
        for (int i = lineSeparationWidth; i < size.y; i += lineSeparationWidth)
            gc.drawLine(0, i, size.x, i);

        final BoxDefinition bd = floGraph.getCurrentBoxDefinition();
        if (bd == null)
            return;

        // Draw the boxes
        final Map<Integer, Pair<BoxInterface, Point>> boxes = bd.getBoxes();
        boxes.keySet().forEach(ID -> {
            final Pair<BoxInterface, Point> pair = boxes.get(ID);
            final boolean isInput =
                    bd.getBoxInterface().containsInput(pair.x.getName());
            drawBox(gc, pair.x, relToAbs(pair.y), ID, isInput);
        });

        // Draw the box interface
        drawBoxInterface(gc, bd.getBoxInterface());

        // Draw the cables
        drawCables(gc);
    }

    private void drawBoxInterface(final GC gc, final BoxInterface bi) {
        final Point stringExtent = gc.textExtent(bi.getName());

        // Calculate the bounds of the box
        final Rectangle boundingRect = getContainingRectsForBoxes();
        final Point pos = new Point(boundingRect.x - scale(100),
                boundingRect.y - scale(50));
        final int x = pos.x;
        final int y = pos.y;
        final int width = boundingRect.width + scale(200);
        final int height = boundingRect.height + scale(100);
        final int topHeight = stringExtent.y + scale(2 * TEXT_PADDING);

        // Add this to the list of box rectangles
        boxRects.add(
                new Pair<Rect, Integer>(new Rect(x, y, width, topHeight), -1));

        // Draw the shadow
        gc.setAlpha(50);
        gc.setBackground(black);
        fillTopRoundRectangle(gc, x + scale(SHADOW_OFFSET),
                y + scale(SHADOW_OFFSET), width, topHeight, scale(ARC_RADIUS));

        // Draw the outline
        gc.setForeground(black);
        gc.setLineWidth(scale(2));
        gc.setAlpha(150);
        gc.drawRoundRectangle(x - 1, y - 1, width + 2, height + 2,
                scale(ARC_RADIUS), scale(ARC_RADIUS));
        gc.drawLine(x, y + topHeight + 1, x + width, y + topHeight + 1);

        // Draw the background
        gc.setAlpha(255);
        gc.setBackground(blue);
        fillTopRoundRectangle(gc, x, y, width, topHeight, scale(ARC_RADIUS));

        // Draw the box name and set the box interface name hotspot
        gc.setForeground(white);
        boxNameRects.add(new Pair<Rect, Integer>(drawCenteredString(gc,
                bi.getName(), x + width / 2, y + topHeight / 2), -1));

        // Draw the circle
        drawInput(gc, bi.getOutput().getEndInput(),
                x + width - scale(CIRCLE_PADDING), y + topHeight / 2);

        // Draw inputs
        final int spacing = (height - topHeight) / (bi.getInputs().size() + 1);
        int inputY = y + topHeight + spacing;
        for (final Input i : bi.getInputs()) {
            final int rectX = x + scale(CIRCLE_PADDING + 2 * TEXT_PADDING);
            final int rectY = inputY - stringExtent.y / 2;
            final Point textExtent = gc.textExtent(i.getName());

            drawOutput(gc, i.getStartOutput(), x + scale(CIRCLE_PADDING),
                    inputY);

            gc.drawText(i.getName(), rectX, rectY, true);

            // Add this to the list of input name rectangles
            final Rect inputRect =
                    new Rect(rectX, rectY, textExtent.x, textExtent.y);
            inputNameRects.add(new Pair<Rect, Input>(inputRect, i));

            inputY += spacing;
        }
    }

    private void drawBox(final GC gc, final BoxInterface bi, final Point point,
            final int ID, final boolean isInput) {
        final Point stringExtent = gc.textExtent(bi.getName());

        // Calculate the size of the box
        int width, height;
        final int topHeight = stringExtent.y + scale(2 * TEXT_PADDING);
        final int numInputs = bi.getInputs().size();

        if (numInputs == 0) {
            width = stringExtent.x + scale(TOTAL_SIDE_PADDING);
            height = topHeight;
        } else {
            int maxInputWidth = 0;
            for (final Input i : bi.getInputs()) {
                final int inputWidth = gc.textExtent(i.getName()).x;
                maxInputWidth =
                        inputWidth > maxInputWidth ? inputWidth : maxInputWidth;
            }

            width = Math.max(
                    maxInputWidth + scale(CIRCLE_PADDING + 2 * TEXT_PADDING
                            + TOTAL_SIDE_PADDING),
                    stringExtent.x + scale(TOTAL_SIDE_PADDING));
            height = stringExtent.y * (numInputs + 1)
                    + scale(TEXT_PADDING * (numInputs + 3));
        }

        // Add this to the list of box rectangles
        final Rect boxRect = new Rect(point.x, point.y, width, height);
        boxRects.add(new Pair<Rect, Integer>(boxRect, ID));

        // Draw the shadow
        gc.setAlpha(50);
        gc.setBackground(black);
        gc.fillRoundRectangle(point.x + scale(SHADOW_OFFSET),
                point.y + scale(SHADOW_OFFSET), width, height,
                scale(ARC_RADIUS), scale(ARC_RADIUS));

        // Draw a yellow outline if this is the currently selected box
        if (ID == getClickedBoxID()) {
            gc.setAlpha(200);
            gc.setForeground(yellow);
        } else
            gc.setForeground(black);

        // Draw the outline
        gc.setLineWidth(scale(2));
        gc.drawRoundRectangle(point.x - 1, point.y - 1, width + 2, height + 2,
                scale(ARC_RADIUS), scale(ARC_RADIUS));

        // Draw the background
        gc.setBackground(blue);
        gc.setAlpha(255);
        gc.fillRoundRectangle(point.x, point.y, width, height,
                scale(ARC_RADIUS), scale(ARC_RADIUS));

        if (numInputs > 0) {
            gc.setBackground(darkBlue);
            fillBottomRoundRectangle(gc, point.x, point.y + topHeight, width,
                    height - topHeight, scale(ARC_RADIUS));
        }

        // Draw box name
        gc.setForeground(white);
        final Rect boxNameRect = drawCenteredString(gc, bi.getName(),
                point.x + width / 2, point.y + topHeight / 2);

        // Add this to the list of box name rectangles
        boxNameRects.add(new Pair<Rect, Integer>(boxNameRect, ID));

        // Draw output
        drawOutput(gc, bi.getOutput(), point.x + width - scale(CIRCLE_PADDING),
                point.y + topHeight / 2);

        // Draw inputs
        int y = point.y + topHeight + scale(TEXT_PADDING) + stringExtent.y / 2;
        for (final Input i : bi.getInputs()) {
            final int rectX =
                    point.x + scale(CIRCLE_PADDING + 2 * TEXT_PADDING);
            final int rectY = y - stringExtent.y / 2;
            final Point textExtent = gc.textExtent(i.getName());

            drawInput(gc, i, point.x + scale(CIRCLE_PADDING), y);

            gc.drawText(i.getName(), rectX, rectY, true);

            // Add this to the list of input name rectangles
            final Rect inputRect =
                    new Rect(rectX, rectY, textExtent.x, textExtent.y);
            inputNameRects.add(new Pair<Rect, Input>(inputRect, i));

            y += stringExtent.y + scale(TEXT_PADDING);
        }
    }

    private void drawInput(final GC gc, final Input input, final int x,
            final int y) {
        // Draw circle
        if (input.hasCable()
                || input == inputOutputMouseOverListener.getMousedOverInput())
            gc.setBackground(yellow);
        else
            gc.setBackground(white);
        gc.fillOval(x - scale(CIRCLE_RADIUS / 2), y - scale(CIRCLE_RADIUS / 2),
                scale(CIRCLE_RADIUS), scale(CIRCLE_RADIUS));

        // Add this to the list of input circles
        final Circle inputCircle = new Circle(x, y, scale(CIRCLE_RADIUS));
        inputCircles.add(new Pair<Circle, Input>(inputCircle, input));
    }

    private void drawOutput(final GC gc, final Output output, final int x,
            final int y) {
        // Draw circle
        if (output.hasCable()
                || output == inputOutputMouseOverListener.getMousedOverOutput())
            gc.setBackground(yellow);
        else
            gc.setBackground(white);
        gc.fillOval(x - scale(CIRCLE_RADIUS / 2), y - scale(CIRCLE_RADIUS / 2),
                scale(CIRCLE_RADIUS), scale(CIRCLE_RADIUS));

        // Add this to the list of output circles
        final Circle outputCircle = new Circle(x, y, scale(CIRCLE_RADIUS));
        outputCircles.add(new Pair<Circle, Output>(outputCircle, output));
    }

    private void drawCables(final GC gc) {
        gc.setAlpha(175);

        final List<Cable> cables =
                floGraph.getCurrentBoxDefinition().getCables();

        cables.forEach(cable -> {
            Circle outputCircle = new Circle(0, 0, 0),
                    inputCircle = new Circle(0, 0, 0);

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
        });

        // Draw the temporary cable if there is one
        final boolean inputHasBeenClicked =
                cableListener.getInputHasBeenClicked();
        final boolean outputHasBeenClicked =
                cableListener.getOutputHasBeenClicked();
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

    private void drawCableBetweenPoints(final GC gc, final Point start,
            final Point end) {
        final Path path = new Path(getDisplay());
        path.moveTo(start.x, start.y);

        final int cx1, cy1, midX, midY, cx2, cy2;
        if (start.x < end.x) {
            final double xWeight = .5;
            final double yWeight = .9;
            midX = (start.x + end.x) / 2;
            midY = (start.y + end.y) / 2;
            cx1 = (int) (xWeight * start.x + (1 - xWeight) * midX);
            cy1 = (int) (yWeight * start.y + (1 - yWeight) * midY);
            cx2 = (int) ((1 - xWeight) * midX + xWeight * end.x);
            cy2 = (int) ((1 - yWeight) * midY + yWeight * end.y);
        } else {
            final int deltaX = Math.min(start.x - end.x, 200);
            final double deltaXWeight = .3;
            final double yWeight = .75;
            midX = (start.x + end.x) / 2;
            midY = (start.y + end.y) / 2;
            cx1 = (int) (start.x + deltaXWeight * deltaX);
            cy1 = (int) (yWeight * start.y + (1 - yWeight) * midY);
            cx2 = (int) (end.x - deltaXWeight * deltaX);
            cy2 = (int) ((1 - yWeight) * midY + yWeight * end.y);
        }

        path.quadTo(cx1, cy1, midX, midY);
        path.quadTo(cx2, cy2, end.x, end.y);

        gc.setLineWidth(scale(2) + 2);
        gc.setForeground(black);
        gc.drawPath(path);

        gc.setLineWidth(scale(2));
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
    private Rect drawCenteredString(final GC gc, final String string,
            final int x, final int y) {
        final Point stringExtent = gc.textExtent(string);
        final int rectX = x - stringExtent.x / 2;
        final int rectY = y - stringExtent.y / 2;
        gc.drawText(string, rectX, rectY, true);
        return new Rect(rectX, rectY, stringExtent.x, stringExtent.y);
    }

    /**
     * Draw a filled-in in rectangle with rounded top corners
     *
     * @param gc
     * @param x
     * @param y
     * @param w
     * @param h
     * @param r
     */
    private void fillTopRoundRectangle(final GC gc, final int x, final int y,
            final int w, final int h, final int arcSize) {
        final int r = arcSize / 2;

        final Path path = new Path(getDisplay());
        path.moveTo(x, y + r);
        path.lineTo(x, y + h);
        path.lineTo(x + w, y + h);
        path.lineTo(x + w, y + r);
        path.addArc(x + w - 2 * r, y, 2 * r, 2 * r, 0, 90);
        path.lineTo(x + r, y);
        path.addArc(x, y, 2 * r, 2 * r, 90, 90);

        gc.fillPath(path);
    }

    /**
     * Draw a filled-in in rectangle with rounded bottom corners
     *
     * @param gc
     * @param x
     * @param y
     * @param w
     * @param h
     * @param r
     */
    private void fillBottomRoundRectangle(final GC gc, final int x, final int y,
            final int w, final int h, final int arcSize) {
        final int r = arcSize / 2;

        final Path path = new Path(getDisplay());
        path.moveTo(x, y + h - r);
        path.lineTo(x, y);
        path.lineTo(x + w, y);
        path.lineTo(x + w, y + h - r);
        path.addArc(x + w - 2 * r, y + h - 2 * r, 2 * r, 2 * r, 0, -90);
        path.lineTo(x + r, y + h);
        path.addArc(x, y + h - 2 * r, 2 * r, 2 * r, 270, -90);

        gc.fillPath(path);
    }

    // Color constants

    private final Color black = new Color(getDisplay(), 0, 0, 0);
    private final Color darkGray = new Color(getDisplay(), 50, 50, 50);
    private final Color mediumGray = new Color(getDisplay(), 60, 60, 60);
    private final Color white = new Color(getDisplay(), 255, 255, 255);
    private final Color blue = new Color(getDisplay(), 59, 91, 180);
    private final Color darkBlue = new Color(getDisplay(), 39, 71, 180);
    private final Color yellow = new Color(getDisplay(), 254, 205, 60);

    // Constants required for drawing boxes etc.

    private static final int CIRCLE_RADIUS = 7;
    private static final int ARC_RADIUS = 10;
    private static final int SHADOW_OFFSET = 4;
    private static final int TEXT_PADDING = 5;
    private static final int CIRCLE_PADDING = 12;
    private static final int TOTAL_SIDE_PADDING = 50;
}
