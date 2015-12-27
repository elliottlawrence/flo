package flo.Canvas;

import org.eclipse.swt.graphics.GC;

public interface DrawingJob {

    /**
     * Performs the drawing job with the specified graphics context
     *
     * @param gc
     */
    public void draw(GC gc);
}
