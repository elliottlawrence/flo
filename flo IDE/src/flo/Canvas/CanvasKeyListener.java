package flo.Canvas;

import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;

public class CanvasKeyListener extends KeyAdapter {

    private final FloCanvas floCanvas;

    public CanvasKeyListener(final FloCanvas floCanvas) {
        this.floCanvas = floCanvas;

        // Add this as a listener to the canvas
        this.floCanvas.addKeyListener(this);
    }

    @Override
    public void keyPressed(final KeyEvent e) {
        // Delete the currently selected box when the user presses backspace
        final int clickedBoxID = floCanvas.getClickedBoxID();
        if (e.keyCode == 8 && clickedBoxID != -1) {
            floCanvas.getFloGraph().getCurrentBoxDefinition()
                    .removeBox(clickedBoxID);
            floCanvas.redraw();
        }
    }
}
