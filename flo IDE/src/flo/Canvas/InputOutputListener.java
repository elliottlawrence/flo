package flo.Canvas;

import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseMoveListener;

import flo.Circle;
import flo.Pair;
import flo.floGraph.Input;
import flo.floGraph.Output;

public class InputOutputListener implements MouseMoveListener {

	private final FloCanvas floCanvas;

	public InputOutputListener(final FloCanvas floCanvas) {
		this.floCanvas = floCanvas;

		// Add this as a listener to the canvas
		this.floCanvas.addMouseMoveListener(this);
	}

	/**
	 * Variables for mousing over inputs/outputs
	 */
	private Input mousedOverInput;

	public Input getMousedOverInput() {
		return mousedOverInput;
	}

	private Output mousedOverOutput;

	public Output getMousedOverOutput() {
		return mousedOverOutput;
	}

	@Override
	public void mouseMove(final MouseEvent e) {
		// See if user moused over an input
		mousedOverInput = null;
		final Pair<Circle, Input> pair1 = floCanvas.getContainingInput(e.x, e.y);
		if (pair1 != null) {
			mousedOverInput = pair1.y;
			return;
		}

		// See if user moused over an output
		mousedOverOutput = null;
		final Pair<Circle, Output> pair2 = floCanvas.getContainingOutput(e.x, e.y);
		if (pair2 != null) {
			mousedOverOutput = pair2.y;
			return;
		}
	}

}
