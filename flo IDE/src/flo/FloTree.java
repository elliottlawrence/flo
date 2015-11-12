package flo;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TreeEditor;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.TraverseEvent;
import org.eclipse.swt.events.TraverseListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

/**
 * The tree component that displays the modules and box definitions
 * in the Flo Graph.
 */
public class FloTree {
	
	private final Tree tree;
	private FloGraph floGraph;
	private final TreeEditor editor;

	public FloTree(Composite parent, int style, FloGraph floGraph) {
		tree = new Tree(parent, style);
		this.floGraph = floGraph;
		
		editor = new TreeEditor(tree);
		editor.grabHorizontal = true;
		
		new FloGraphTreeListener(this.floGraph, tree);
		tree.addKeyListener(keyListener);
		tree.addSelectionListener(selectionListener);
	}

	private final KeyListener keyListener = new KeyListener() {
		final static int DELETE = 8;
		
		@Override
		public void keyPressed(KeyEvent e) {
			// Delete an item in the tree
			if (e.keyCode == DELETE) {
				TreeItem[] selectedItems = tree.getSelection();
				if (selectedItems.length == 0) return;

				TreeItem selectedItem = selectedItems[0];
				TreeItem parentItem = selectedItem.getParentItem();
				
				// Item is a module
				if (parentItem == null) floGraph.removeModule(selectedItem.getText());
				// Item is a box definition
				else {
					BoxDefinitionContainer parentContainer =
							findBoxDefContainerFromTreeItem(parentItem, floGraph);
					parentContainer.removeBoxDefinition(selectedItem.getText());
				}
			}
		}

		@Override
		public void keyReleased(KeyEvent e) { }	
	};
	
	private final SelectionAdapter selectionListener = new SelectionAdapter() {
		public void widgetDefaultSelected(SelectionEvent e) {
			// Identify the selected row
			final TreeItem item = (TreeItem) e.item;
			if (item == null) return;
			tree.deselect(item);
			
			// Create new editor
			final Text newEditor = new Text(tree, SWT.NONE);
			newEditor.setText(item.getText());
			newEditor.selectAll();
			newEditor.setFocus();
			
			// Save text when focus is lost
			newEditor.addFocusListener(new FocusAdapter() {
				@Override
				public void focusLost(FocusEvent event) {
					Text text = (Text) editor.getEditor();
					setText(text.getText(), item);
					tree.select(item);
					newEditor.dispose();
				}
			});
			
			// Save text on Return, discard on Escape
			newEditor.addTraverseListener(new TraverseListener() {
				@Override
				public void keyTraversed(TraverseEvent e) {
					switch (e.detail) {
					case SWT.TRAVERSE_RETURN:
						setText(newEditor.getText(), item);
						tree.select(item);
						// fall through
					case SWT.TRAVERSE_ESCAPE:
						newEditor.dispose();
						e.doit = false;
					}
				}
			});
			
			editor.setEditor(newEditor, item);
		}
		
		private void setText(String text, TreeItem item) {
			BoxDefinitionContainer container = findBoxDefContainerFromTreeItem(item, floGraph);
			if (container instanceof Module) ((Module) container).setName(text);
			if (container instanceof BoxDefinition)
				((BoxDefinition) container).getBoxInterface().setName(text);
		}
	};

	public Tree getTree() {
		return tree;
	}
	
	/**
	 * Given a TreeItem, find the corresponding object in the Flo Graph.
	 * @param ti
	 * @param floGraph
	 * @return The corresponding object
	 */
	public static BoxDefinitionContainer findBoxDefContainerFromTreeItem(TreeItem ti, FloGraph floGraph) {
		TreeItem parent = ti.getParentItem();
		// Item is a module
		if (parent == null) return floGraph.getModule(ti.getText());
		
		// Item is a box definition
		BoxDefinitionContainer parentContainer = findBoxDefContainerFromTreeItem(parent, floGraph);
		return parentContainer.getBoxDefinition(ti.getText());
	}
}
