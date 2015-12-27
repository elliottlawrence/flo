package flo.Canvas;

import java.util.function.Consumer;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Text;

import flo.Util.Pair;
import flo.Util.Rect;
import flo.floGraph.BoxDefinition;
import flo.floGraph.BoxInterface;
import flo.floGraph.Input;

public class DoubleClickListener extends MouseAdapter {

    private final FloCanvas floCanvas;

    public DoubleClickListener(final FloCanvas floCanvas) {
        this.floCanvas = floCanvas;

        // Add this as a listener to the canvas
        this.floCanvas.addMouseListener(this);
    }

    @Override
    public void mouseDoubleClick(final MouseEvent e) {
        // Listen for double clicks on the box names
        final Pair<Rect, Integer> pair1 =
                floCanvas.getContainingBoxName(e.x, e.y);
        if (pair1 != null) {
            final Rect rect = pair1.x;
            final int ID = pair1.y;

            createEditor(rect.rect,
                    textEditor -> setBoxName(ID, textEditor.getText()));
            return;
        }

        // Listen for double clicks on the input names
        final Pair<Rect, Input> pair2 =
                floCanvas.getContainingInputName(e.x, e.y);
        if (pair2 != null) {
            final Rect rect = pair2.x;
            final Input input = pair2.y;

            createEditor(rect.rect,
                    textEditor -> setInputName(input, textEditor.getText()));
            return;
        }

        // Listen for double clicks on the box interface name
        final Rect boxInterfaceRectangle = floCanvas.getBoxInterfaceRect();
        if (boxInterfaceRectangle.contains(e.x, e.y)) {
            createEditor(boxInterfaceRectangle.rect,
                    textEditor -> setBoxInterfaceName(textEditor.getText()));
            return;
        }

        // Listen for double clicks on boxes (but not on anything else)
        final Pair<Rect, Integer> pair3 = floCanvas.getContainingBox(e.x, e.y);
        if (pair3 != null) {
            final int ID = pair3.y;
            final BoxInterface bi;
            if (ID == -1)
                bi = floCanvas.getFloGraph().getCurrentBoxDefinition()
                        .getBoxInterface();
            else
                bi = floCanvas.getFloGraph().getCurrentBoxDefinition()
                        .getBoxes().get(ID).x;
            bi.addInput("input" + (bi.getInputs().size() + 1));
            floCanvas.redraw();
            return;
        }
    }

    /**
     * Creates a new text editor that runs the given consumer when text is typed
     *
     * @param rect
     * @param consumer
     */
    private void createEditor(final Rectangle rect,
            final Consumer<Text> consumer) {
        // Create new editor
        final Text textEditor = new Text(floCanvas, SWT.NONE);
        textEditor.setLocation(rect.x, rect.y);
        textEditor.setSize(rect.width, rect.height);
        textEditor.selectAll();
        textEditor.setFocus();

        // Save text when focus is lost
        textEditor.addFocusListener(new FocusAdapter() {
            @Override
            public void focusLost(final FocusEvent event) {
                consumer.accept(textEditor);
                textEditor.dispose();
            }
        });

        // Save text on Return, discard on Escape
        textEditor.addTraverseListener(e1 -> {
            switch (e1.detail) {
            case SWT.TRAVERSE_RETURN:
                consumer.accept(textEditor);
                // fall through
            case SWT.TRAVERSE_ESCAPE:
                textEditor.dispose();
                e1.doit = false;
            }
        });
    }

    /**
     * Change a box's name
     */
    private void setBoxName(final int ID, final String name) {
        // Can't rename to nothing
        if (name.isEmpty())
            return;

        final BoxDefinition currentBoxDefinition =
                floCanvas.getFloGraph().getCurrentBoxDefinition();
        final Pair<BoxInterface, Point> bip =
                currentBoxDefinition.getBoxes().get(ID);
        final BoxInterface bi = bip.x;

        // Change the box's name
        bi.setName(name);
    }

    /**
     * Change an input's name, or delete the input if its name was cleared
     */
    private void setInputName(final Input input, final String name) {
        if (name.isEmpty())
            // Remove input from box interface
            input.getParent().removeInput(input);
        else
            input.setName(name);
    }

    /**
     * Change the box interface's name
     */
    private void setBoxInterfaceName(final String name) {
        floCanvas.getFloGraph().getCurrentBoxDefinition().getBoxInterface()
                .setName(name);
    }
}
