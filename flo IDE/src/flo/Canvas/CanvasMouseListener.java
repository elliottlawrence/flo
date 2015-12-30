package flo.Canvas;

import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;

import flo.Util.Pair;
import flo.Util.Pnt;
import flo.Util.Rect;
import flo.floGraph.FloGraph;

public class CanvasMouseListener extends MouseAdapter
    implements MouseMoveListener {

    private final FloCanvas floCanvas;

    /**
     * The currently selected box (if there is one)
     */
    private int clickedBoxID = -1;

    // Variables for drag events

    private boolean canvasDrag = false;
    private boolean boxDrag = false;
    private int draggedBoxID;
    private Pnt dragOffset = new Pnt(0, 0);

    public CanvasMouseListener(final FloCanvas floCanvas) {
        this.floCanvas = floCanvas;

        // Add this as a listener to the canvas
        this.floCanvas.addMouseListener(this);
        this.floCanvas.addMouseMoveListener(this);
    }

    public int getClickedBoxID() {
        return clickedBoxID;
    }

    public void setClickedBoxID(final int ID) {
        clickedBoxID = ID;
    }

    @Override
    public void mouseDown(final MouseEvent e1) {
        if (e1.button != 1)
            return;

        // Reset variable
        clickedBoxID = -1;

        // See if the user clicked on a box
        final Pnt e = new Pnt(e1.x, e1.y);
        final Pair<Rect, Integer> pair = floCanvas.getContainingBox(e);
        if (pair != null && pair.y != -1) {
            clickedBoxID = pair.y;

            boxDrag = true;
            draggedBoxID = pair.y;
            dragOffset = pair.x.getLocation().minus(e);
        }
        // Otherwise drag the canvas around
        else {
            final FloGraph floGraph = floCanvas.getFloGraph();
            canvasDrag = true;
            dragOffset = floGraph.getCurrentBoxDefinition().getOffset()
                .minus(e.scalarMult(1 / floGraph.getZoom()));
        }
    }

    @Override
    public void mouseUp(final MouseEvent e) {
        boxDrag = false;
        canvasDrag = false;
    }

    @Override
    public void mouseMove(final MouseEvent e1) {
        final Pnt e = new Pnt(e1.x, e1.y);
        if (boxDrag && draggedBoxID != -1)
            floCanvas.getFloGraph().getCurrentBoxDefinition().setBoxLocation(
                draggedBoxID,
                floCanvas.absToRel(dragOffset.plus(e)));
        else if (canvasDrag) {
            final FloGraph floGraph = floCanvas.getFloGraph();
            floGraph.getCurrentBoxDefinition().setOffset(
                dragOffset.plus(e.scalarMult(1 / floGraph.getZoom())));
        }
    }
}
