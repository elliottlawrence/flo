package flo;

import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

public class FloCanvas {
	
	private final Canvas canvas;
	private FloGraph floGraph;

	public FloCanvas(Composite parent, int style, FloGraph floGraph) {
		canvas = new Canvas(parent, style);
		this.floGraph = floGraph;
	}

}
