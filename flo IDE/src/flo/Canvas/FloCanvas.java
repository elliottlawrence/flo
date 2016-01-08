package flo.Canvas;

import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Path;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

import flo.Util.Circle;
import flo.Util.Hotspots;
import flo.Util.Pair;
import flo.Util.Pnt;
import flo.Util.Rect;
import flo.Util.Util;
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

    // Relative location of the previous minimum bounding rectangle
    private Pnt minBoundingRectLoc = new Pnt(0, 0);

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

        // Listen for when a new box definition is selected
        this.floGraph.addBoxDefinitionSelectedObserver(
            e -> minBoundingRectLoc = new Pnt(0, 0));

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

    public Pair<Rect, Integer> getContainingBox(final Pnt p) {
        return boxRects.getContainingShape(p);
    }

    public Pair<Rect, Integer> getContainingBoxName(final Pnt p) {
        return boxNameRects.getContainingShape(p);
    }

    public Pair<Rect, Input> getContainingInputName(final Pnt p) {
        return inputNameRects.getContainingShape(p);
    }

    public Pair<Circle, Input> getContainingInput(final Pnt p) {
        return inputCircles.getContainingShape(p);
    }

    public Pair<Circle, Output> getContainingOutput(final Pnt p) {
        return outputCircles.getContainingShape(p);
    }

    private void resetHotspots() {
        boxRects.clear();
        boxNameRects.clear();
        inputNameRects.clear();
        inputCircles.clear();
        outputCircles.clear();
    }

    // Methods related to the currently clicked box

    public int getClickedBoxID() {
        return boxListener.getClickedBoxID();
    }

    public void setClickedBoxID(final int ID) {
        boxListener.setClickedBoxID(ID);
    }

    // Methods related to zoom, offset, and sizing

    public Pnt getCanvasSize() {
        return new Pnt(getSize());
    }

    /**
     * Scales a length based on the current zoom level
     *
     * @param x
     * @return
     */
    private int scale(final int x) {
        return (int) (floGraph.getZoom() * x);
    }

    /**
     * Converts relative coordinates to absolute coordinates
     *
     * @param p
     * @return
     */
    public Pnt relToAbs(final Pnt p) {
        return p.plus(floGraph.getCurrentBoxDefinition().getOffset())
            .scalarMult(floGraph.getZoom())
            .plus(getCanvasSize().scalarMult(0.5));
    }

    /**
     * Converts absolute coordinates to relative coordinates
     *
     * @param p
     * @return
     */
    public Pnt absToRel(final Pnt p) {
        return p.minus(getCanvasSize().scalarMult(0.5))
            .scalarMult(1 / floGraph.getZoom())
            .minus(floGraph.getCurrentBoxDefinition().getOffset());
    }

    public Pnt getDefaultBoxLocation() {
        return minBoundingRectLoc;
    }

    // Methods for painting the canvas

    @Override
    public Font getFont() {
        final FontData fd = getDisplay().getSystemFont().getFontData()[0];
        fd.setHeight(scale(13));
        return new Font(getDisplay(), fd);
    }

    private void paint(final GC gc) {
        resetHotspots();

        // Set GC properties
        gc.setFont(getFont());
        gc.setLineCap(SWT.CAP_ROUND);

        paintCanvas(gc);
    }

    private void paintCanvas(final GC gc) {
        // Draw background
        final Pnt size = getCanvasSize();
        gc.setBackground(darkGray);
        gc.fillRectangle(0, 0, size.x, size.y);

        // Draw lines
        gc.setForeground(mediumGray);
        final int lineSeparationWidth = 50;
        for (int i = lineSeparationWidth; i < size.x; i += lineSeparationWidth)
            gc.drawLine(i, 0, i, size.y);
        for (int i = lineSeparationWidth; i < size.y; i += lineSeparationWidth)
            gc.drawLine(0, i, size.x, i);

        // Draw nothing if no box definition is selected
        final BoxDefinition bd = floGraph.getCurrentBoxDefinition();
        if (bd == null)
            return;

        // Draw the boxes
        final Map<Integer, Pair<BoxInterface, Pnt>> boxes = bd.getBoxes();
        boxes.keySet().forEach(ID -> {
            final Pair<BoxInterface, Pnt> pair = boxes.get(ID);
            drawBox(gc, pair.x, relToAbs(pair.y), ID);
        });

        // Draw the box interface
        drawBox(gc, bd.getBoxInterface(), null, -1);

        // Draw the cables
        drawCables(gc);
    }

    private void drawBox(final GC gc, final BoxInterface bi, Pnt p,
        final int ID) {
        final boolean isMainBox = ID == -1;

        // Calculate the size of the box
        final Pair<Rect, Rect> bounds = isMainBox
            ? calculuateBoxInterfaceSize(gc, bi.getName(), bi.getInputs())
            : calculateBoxSize(gc, p, bi.getName(), bi.getInputs());
        final Rect boxRect = bounds.x;
        final Rect topBoxRect = bounds.y;
        final Rect impBoxRect = isMainBox ? topBoxRect : boxRect;

        if (isMainBox)
            p = boxRect.getLocation();
        final int width = boxRect.rect.width;
        final int height = boxRect.rect.height;
        final int topHeight = topBoxRect.rect.height;

        // Add this to the list of box rectangles
        boxRects.add(new Pair<Rect, Integer>(impBoxRect, ID));

        // Draw the shadow
        gc.setAlpha(50);
        gc.setBackground(black);
        if (isMainBox)
            fillTopRoundRectangle(
                gc,
                topBoxRect.translate(
                    scale(SHADOW_OFFSET),
                    scale(SHADOW_OFFSET)),
                scale(ARC_RADIUS));
        else
            fillRoundRectangle(
                gc,
                boxRect.translate(scale(SHADOW_OFFSET), scale(SHADOW_OFFSET)),
                scale(ARC_RADIUS));

        // Draw the outline
        gc.setForeground(black);
        gc.setLineWidth(scale(2));

        if (isMainBox)
            gc.setAlpha(150);
        // Draw a yellow outline if this is the currently selected box
        else if (ID == getClickedBoxID()) {
            gc.setAlpha(200);
            gc.setForeground(yellow);
        }

        drawRoundRectangle(
            gc,
            boxRect.translate(-1, -1).stretch(2, 2),
            scale(ARC_RADIUS));

        if (isMainBox)
            gc.drawLine(
                p.x,
                p.y + topHeight + 1,
                p.x + width,
                p.y + topHeight + 1);

        // Draw the background
        gc.setAlpha(255);
        gc.setBackground(blue);
        if (isMainBox)
            fillTopRoundRectangle(gc, topBoxRect, scale(ARC_RADIUS));
        else
            fillRoundRectangle(gc, boxRect, scale(ARC_RADIUS));

        final int numInputs = bi.getInputs().size();
        if (!isMainBox && numInputs > 0) {
            gc.setBackground(darkBlue);
            fillBottomRoundRectangle(
                gc,
                boxRect.translate(0, topHeight).stretch(0, -topHeight),
                scale(ARC_RADIUS));
        }

        // Draw box name
        gc.setForeground(white);
        final Rect boxNameRect = drawCenteredString(
            gc,
            bi.getName(),
            p.plus(new Pnt(width, topHeight).scalarMult(0.5)));

        // Add this to the list of box name rectangles
        boxNameRects.add(new Pair<Rect, Integer>(boxNameRect, ID));

        // Draw output
        final Pnt outputLoc =
            new Pnt(width - scale(CIRCLE_PADDING), topHeight / 2);
        if (isMainBox)
            drawInput(gc, bi.getOutput().getEndInput(), p.plus(outputLoc));
        else
            drawOutput(gc, bi.getOutput(), p.plus(outputLoc));

        // Draw inputs
        if (numInputs == 0)
            return;

        final int spacing = (height - topHeight) / numInputs;
        int y = p.y + topHeight + spacing / 2;
        for (final Input i : bi.getInputs()) {
            final Pnt textExtent = textExtent(gc, i.getName());
            final Pnt textLoc = isMainBox
                ? new Pnt(p.x + scale(2 * TEXT_PADDING), y - textExtent.y / 2)
                : new Pnt(p.x + scale(CIRCLE_PADDING + 2 * TEXT_PADDING),
                    y - textExtent.y / 2);
            final Pnt inputLoc = isMainBox
                ? new Pnt(p.x + scale(4 * TEXT_PADDING) + textExtent.x, y)
                : new Pnt(p.x + scale(CIRCLE_PADDING), y);

            if (isMainBox)
                drawOutput(gc, i.getStartOutput(), inputLoc);
            else
                drawInput(gc, i, inputLoc);

            gc.drawText(i.getName(), textLoc.x, textLoc.y, true);

            // Add this to the list of input name rectangles
            final Rect inputRect =
                new Rect(textLoc, textExtent.x, textExtent.y);
            inputNameRects.add(new Pair<Rect, Input>(inputRect, i));

            y += spacing;
        }
    }

    private Pair<Rect, Rect> calculuateBoxInterfaceSize(final GC gc,
        final String boxName, final List<Input> inputs) {
        final Pnt stringExtent = textExtent(gc, boxName);
        final int topHeight = stringExtent.y + scale(2 * TEXT_PADDING);

        // Find the minimum rectangle bounding the boxes
        Rect minBoundingRect;
        if (boxRects.isEmpty())
            minBoundingRect =
                new Rect(relToAbs(minBoundingRectLoc), scale(50), 0);
        else {
            minBoundingRect = Rect.getContainingRect(Pair.unzip1(boxRects));
            minBoundingRectLoc = absToRel(minBoundingRect.getLocation());
        }

        // Adjust with some padding
        final Rectangle boundingRect =
            minBoundingRect
                .translate(
                    scale(-TOTAL_SIDE_PADDING),
                    scale(-TOTAL_SIDE_PADDING))
                .stretch(
                    scale(2 * TOTAL_SIDE_PADDING),
                    scale(2 * TOTAL_SIDE_PADDING)).rect;

        int widthPadding = 0;
        int minHeight = 0;

        // Calculate size needed for inputs
        final int numInputs = inputs.size();
        if (numInputs != 0) {
            final int maxInputWidth =
                Util.maxBy(inputs, i -> textExtent(gc, i.getName()).x);

            widthPadding =
                maxInputWidth + scale(CIRCLE_PADDING + 2 * TEXT_PADDING);
            minHeight = stringExtent.y * numInputs
                + scale(TEXT_PADDING * (numInputs + 2));
        }

        // Calculate final sizes
        final Pnt p =
            new Pnt(boundingRect.x - widthPadding, boundingRect.y - topHeight);
        final int width = Math.max(
            widthPadding + boundingRect.width,
            stringExtent.x + scale(TOTAL_SIDE_PADDING));
        final int height = topHeight + Math.max(boundingRect.height, minHeight);

        final Rect boxRect = new Rect(p, width, height);
        final Rect topBoxRect = new Rect(p, width, topHeight);

        return new Pair<Rect, Rect>(boxRect, topBoxRect);
    }

    private Pair<Rect, Rect> calculateBoxSize(final GC gc, final Pnt p,
        final String boxName, final List<Input> inputs) {
        final Pnt stringExtent = textExtent(gc, boxName);
        final int topHeight = stringExtent.y + scale(2 * TEXT_PADDING);
        int width, height = topHeight;

        // Calculate size needed for inputs
        final int numInputs = inputs.size();
        if (numInputs == 0)
            width = stringExtent.x + scale(TOTAL_SIDE_PADDING);
        else {
            final int maxInputWidth =
                Util.maxBy(inputs, i -> textExtent(gc, i.getName()).x);

            width = Math.max(
                maxInputWidth + scale(
                    CIRCLE_PADDING + 2 * TEXT_PADDING + TOTAL_SIDE_PADDING),
                stringExtent.x + scale(TOTAL_SIDE_PADDING));
            height += stringExtent.y * numInputs
                + scale(TEXT_PADDING * (numInputs + 1));
        }

        final Rect boxRect = new Rect(p, width, height);
        final Rect topBoxRect = new Rect(p, width, topHeight);

        return new Pair<Rect, Rect>(boxRect, topBoxRect);
    }

    private void drawInput(final GC gc, final Input input, final Pnt p) {
        // Draw circle
        if (input.hasCable()
            || input == inputOutputMouseOverListener.getMousedOverInput())
            gc.setBackground(yellow);
        else
            gc.setBackground(white);
        gc.fillOval(
            p.x - scale(CIRCLE_RADIUS / 2),
            p.y - scale(CIRCLE_RADIUS / 2),
            scale(CIRCLE_RADIUS),
            scale(CIRCLE_RADIUS));

        // Add this to the list of input circles
        final Circle inputCircle = new Circle(p, scale(CIRCLE_RADIUS));
        inputCircles.add(new Pair<Circle, Input>(inputCircle, input));
    }

    private void drawOutput(final GC gc, final Output output, final Pnt p) {
        // Draw circle
        if (output.hasCable()
            || output == inputOutputMouseOverListener.getMousedOverOutput())
            gc.setBackground(yellow);
        else
            gc.setBackground(white);
        gc.fillOval(
            p.x - scale(CIRCLE_RADIUS / 2),
            p.y - scale(CIRCLE_RADIUS / 2),
            scale(CIRCLE_RADIUS),
            scale(CIRCLE_RADIUS));

        // Add this to the list of output circles
        final Circle outputCircle = new Circle(p, scale(CIRCLE_RADIUS));
        outputCircles.add(new Pair<Circle, Output>(outputCircle, output));
    }

    private void drawCables(final GC gc) {
        gc.setAlpha(175);

        final List<Cable> cables =
            floGraph.getCurrentBoxDefinition().getCables();

        cables.forEach(cable -> {
            final Circle outputCircle =
                Util.findBy(cable.getOutput(), outputCircles, p -> p.y).x;

            final Circle inputCircle =
                Util.findBy(cable.getInput(), inputCircles, p -> p.y).x;

            // Draw the cable
            drawCableBetweenPoints(gc, outputCircle.center, inputCircle.center);
        });

        // Draw the temporary cable if there is one
        final boolean inputHasBeenClicked =
            cableListener.getInputHasBeenClicked();
        final boolean outputHasBeenClicked =
            cableListener.getOutputHasBeenClicked();
        final Pnt cableEnd = cableListener.getCableEnd();

        if (inputHasBeenClicked && !outputHasBeenClicked) {
            final Circle inputCircle = Util.findBy(
                cableListener.getClickedInput(),
                inputCircles,
                p -> p.y).x;

            drawCableBetweenPoints(gc, cableEnd, inputCircle.center);
        } else if (outputHasBeenClicked && !inputHasBeenClicked) {
            final Circle outputCircle = Util.findBy(
                cableListener.getClickedOutput(),
                outputCircles,
                p -> p.y).x;

            drawCableBetweenPoints(gc, outputCircle.center, cableEnd);
        }

        gc.setAlpha(255);
    }

    private void drawCableBetweenPoints(final GC gc, final Pnt start,
        final Pnt end) {
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

    // Methods related to strings

    /**
     * Draw a string centered at a point
     *
     * @param gc
     * @param string
     * @param p
     * @return The string's bounding rectangle
     */
    private Rect drawCenteredString(final GC gc, final String string,
        final Pnt p) {
        final Pnt stringExtent = textExtent(gc, string);
        final Pnt rectP = p.minus(stringExtent.scalarMult(0.5));
        gc.drawText(string, rectP.x, rectP.y, true);
        return new Rect(rectP, stringExtent.x, stringExtent.y);
    }

    private Pnt textExtent(final GC gc, final String string) {
        return new Pnt(gc.textExtent(string));
    }

    // Methods related to rectangles

    private void fillRoundRectangle(final GC gc, final Rect rect,
        final int arcSize) {
        final Rectangle r = rect.rect;
        gc.fillRoundRectangle(r.x, r.y, r.width, r.height, arcSize, arcSize);
    }

    private void drawRoundRectangle(final GC gc, final Rect rect,
        final int arcSize) {
        final Rectangle r = rect.rect;
        gc.drawRoundRectangle(r.x, r.y, r.width, r.height, arcSize, arcSize);
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
    private void fillTopRoundRectangle(final GC gc, final Rect rect,
        final int arcSize) {
        final int r = arcSize / 2;
        final int x = rect.rect.x;
        final int y = rect.rect.y;
        final int w = rect.rect.width;
        final int h = rect.rect.height;

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
    private void fillBottomRoundRectangle(final GC gc, final Rect rect,
        final int arcSize) {
        final int r = arcSize / 2;
        final int x = rect.rect.x;
        final int y = rect.rect.y;
        final int w = rect.rect.width;
        final int h = rect.rect.height;

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