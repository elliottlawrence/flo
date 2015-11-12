package flo;

import java.util.Observable;
import java.util.Observer;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;

/**
 * The canvas where code is primarily edited.
 */
public class FloCanvas extends Canvas implements Observer {
	
	private final FloGraph floGraph;
	
	public FloCanvas(Composite parent, FloGraph floGraph) {
		super(parent, SWT.NO_BACKGROUND);
		this.floGraph = floGraph;
		this.floGraph.addObserver(this);
		
		addPaintListener(paintListener);
	}
	
	private String textToDraw;
	
	private final PaintListener paintListener = new PaintListener() {
		@Override
		public void paintControl(PaintEvent e) {
			if (textToDraw != null) {
				e.gc.drawText(textToDraw, 10, 10);
			}
		}
	};

	@Override
	public void update(Observable o, Object arg) {
		Object[] args = (Object []) arg;
		FloGraphChange change = (FloGraphChange) args[0];

		switch (change) {
		case BoxDefinitionSelected:
			BoxDefinition bd = (BoxDefinition) args[1];
			textToDraw = bd.getBoxInterface().getName();
			redraw();
			break;
			
		default:
			break;
		}
	}
}
