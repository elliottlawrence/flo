package flo.Canvas;

import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;

import flo.Util.Circle;
import flo.Util.Pair;
import flo.Util.Pnt;
import flo.floGraph.Cable;
import flo.floGraph.FloGraph;
import flo.floGraph.Input;
import flo.floGraph.Output;

public class CableListener extends MouseAdapter implements MouseMoveListener {

    private final FloCanvas floCanvas;

    // Variables for click and move events

    private boolean inputHasBeenClicked = false;
    private Input clickedInput;

    private boolean outputHasBeenClicked = false;
    private Output clickedOutput;

    private Pnt cableEnd = new Pnt(0, 0);

    public CableListener(final FloCanvas floCanvas) {
        this.floCanvas = floCanvas;

        // Add this as a listener to the canvas
        this.floCanvas.addMouseListener(this);
        this.floCanvas.addMouseMoveListener(this);
    }

    public boolean getInputHasBeenClicked() {
        return inputHasBeenClicked;
    }

    public Input getClickedInput() {
        return clickedInput;
    }

    public boolean getOutputHasBeenClicked() {
        return outputHasBeenClicked;
    }

    public Output getClickedOutput() {
        return clickedOutput;
    }

    public Pnt getCableEnd() {
        return cableEnd;
    }

    // Event handlers

    @Override
    public void mouseDown(final MouseEvent e) {
        if (e.button != 1)
            return;

        final Pnt p = new Pnt(e.x, e.y);

        // See if user clicked an input
        final Pair<Circle, Input> pair1 = floCanvas.getContainingInput(p);
        if (pair1 != null) {
            inputHasBeenClicked = true;
            clickedInput = pair1.y;

            if (outputHasBeenClicked) {
                // Make cable
                final FloGraph floGraph = floCanvas.getFloGraph();
                final Cable cable = new Cable(clickedOutput, clickedInput,
                    floGraph.getCurrentBoxDefinition());
                floGraph.getCurrentBoxDefinition().addCable(cable);

                // Reset variables
                inputHasBeenClicked = outputHasBeenClicked = false;

                floCanvas.redraw();
            } else if (clickedInput.hasCable()) {
                // Delete cable
                final FloGraph floGraph = floCanvas.getFloGraph();
                final Cable cable = clickedInput.getCable();
                floGraph.getCurrentBoxDefinition().removeCable(cable);

                // Set variables
                inputHasBeenClicked = false;
                outputHasBeenClicked = true;
                clickedOutput = cable.getOutput();
            }
            return;
        }

        // See if user clicked an output
        final Pair<Circle, Output> pair2 = floCanvas.getContainingOutput(p);
        if (pair2 != null) {
            outputHasBeenClicked = true;
            clickedOutput = pair2.y;

            if (inputHasBeenClicked) {
                // Make cable
                final FloGraph floGraph = floCanvas.getFloGraph();
                final Cable cable = new Cable(clickedOutput, clickedInput,
                    floGraph.getCurrentBoxDefinition());
                floGraph.getCurrentBoxDefinition().addCable(cable);

                // Reset variables
                inputHasBeenClicked = outputHasBeenClicked = false;

                floCanvas.redraw();
            }
            return;
        }

        // Delete existing cable otherwise
        inputHasBeenClicked = outputHasBeenClicked = false;
        floCanvas.redraw();
    }

    @Override
    public void mouseMove(final MouseEvent e) {
        cableEnd = new Pnt(e.x, e.y);
    }
}
