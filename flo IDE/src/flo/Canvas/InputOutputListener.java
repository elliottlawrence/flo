package flo.Canvas;

import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;

import flo.Util.Circle;
import flo.Util.Pair;
import flo.Util.Pnt;
import flo.floGraph.Input;
import flo.floGraph.Output;

public class InputOutputListener implements MouseMoveListener {

    private final FloCanvas floCanvas;

    private Input mousedOverInput;
    private Output mousedOverOutput;

    public InputOutputListener(final FloCanvas floCanvas) {
        this.floCanvas = floCanvas;

        // Add this as a listener to the canvas
        this.floCanvas.addMouseMoveListener(this);
    }

    public Input getMousedOverInput() {
        return mousedOverInput;
    }

    public Output getMousedOverOutput() {
        return mousedOverOutput;
    }

    @Override
    public void mouseMove(final MouseEvent e) {
        final Pnt p = new Pnt(e.x, e.y);

        // See if user moused over an input
        mousedOverInput = null;
        final Pair<Circle, Input> pair1 = floCanvas.getContainingInput(p);
        if (pair1 != null) {
            mousedOverInput = pair1.y;
            return;
        }

        // See if user moused over an output
        mousedOverOutput = null;
        final Pair<Circle, Output> pair2 = floCanvas.getContainingOutput(p);
        if (pair2 != null) {
            mousedOverOutput = pair2.y;
            return;
        }
    }

}
