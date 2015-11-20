package flo;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.TreeEditor;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

/**
 * The tree component that displays the modules and box definitions in the Flo
 * Graph.
 */
public class FloTree {

	private final FloGraph floGraph;
	private final Tree tree;
	private final TreeEditor editor;

	public FloTree(final Composite parent, final FloGraph floGraph) {
		tree = new Tree(parent, SWT.BORDER);
		tree.addSelectionListener(selectionListener);
		this.floGraph = floGraph;

		new FloGraphTreeListener(floGraph, tree);

		// Create the editor for renaming tree items
		editor = new TreeEditor(tree);
		editor.grabHorizontal = true;

		// Add the context menu
		final Menu menu = new Menu(tree);
		tree.setMenu(menu);

		final MenuItem miDelete = new MenuItem(menu, SWT.NONE);
		miDelete.setText("Delete");
		miDelete.addSelectionListener(deleteListener);
	}

	/**
	 * Delete a tree item
	 */
	private final SelectionAdapter deleteListener = new SelectionAdapter() {
		@Override
		public void widgetSelected(final SelectionEvent e) {
			final TreeItem[] selectedItems = tree.getSelection();
			if (selectedItems.length == 0)
				return;

			final TreeItem selectedItem = selectedItems[0];
			final TreeItem parentItem = selectedItem.getParentItem();

			// Item is a module
			if (parentItem == null)
				floGraph.removeModule(selectedItem.getText());
			// Item is a box definition
			else {
				// If this was the current box definition, set that to null
				if (floGraph.getCurrentBoxDefinition() == findBoxDefContainerFromTreeItem(selectedItem, floGraph))
					floGraph.setCurrentBoxDefinition(null);

				// Now delete it
				final BoxDefinitionContainer parentContainer = findBoxDefContainerFromTreeItem(parentItem, floGraph);
				parentContainer.removeBoxDefinition(selectedItem.getText());
			}
		}
	};

	private final SelectionAdapter selectionListener = new SelectionAdapter() {
		/**
		 * Change the currently selected box definition
		 */
		@Override
		public void widgetSelected(final SelectionEvent e) {
			final TreeItem[] selectedItems = tree.getSelection();
			if (selectedItems.length == 0)
				return;

			final BoxDefinitionContainer selectedContainer = findBoxDefContainerFromTreeItem(selectedItems[0],
					floGraph);
			if (selectedContainer instanceof BoxDefinition)
				floGraph.setCurrentBoxDefinition((BoxDefinition) selectedContainer);
		}

		/**
		 * Rename a tree item when double clicked
		 */
		@Override
		public void widgetDefaultSelected(final SelectionEvent e) {
			// Identify the selected row
			final TreeItem item = (TreeItem) e.item;
			if (item == null)
				return;
			tree.deselect(item);

			// Create new editor
			final Text newEditor = new Text(tree, SWT.NONE);
			newEditor.setText(item.getText());
			newEditor.selectAll();
			newEditor.setFocus();

			// Save text when focus is lost
			newEditor.addFocusListener(new FocusAdapter() {
				@Override
				public void focusLost(final FocusEvent event) {
					final Text text = (Text) editor.getEditor();
					setText(text.getText(), item);
					tree.select(item);
					newEditor.dispose();
				}
			});

			// Save text on Return, discard on Escape
			newEditor.addTraverseListener(e1 -> {
				switch (e1.detail) {
				case SWT.TRAVERSE_RETURN:
					setText(newEditor.getText(), item);
					tree.select(item);
					// fall through
				case SWT.TRAVERSE_ESCAPE:
					newEditor.dispose();
					e1.doit = false;
				}
			});

			editor.setEditor(newEditor, item);
		}

		private void setText(final String text, final TreeItem item) {
			final BoxDefinitionContainer container = findBoxDefContainerFromTreeItem(item, floGraph);
			if (container instanceof Module)
				((Module) container).setName(text);
			if (container instanceof BoxDefinition)
				((BoxDefinition) container).getBoxInterface().setName(text);
		}
	};

	public Tree getTree() {
		return tree;
	}

	/**
	 * Given a TreeItem, find the corresponding object in the Flo Graph
	 *
	 * @param ti
	 * @return The corresponding object
	 */
	public static BoxDefinitionContainer findBoxDefContainerFromTreeItem(final TreeItem ti, final FloGraph floGraph) {
		final TreeItem parent = ti.getParentItem();
		// Item is a module
		if (parent == null)
			return floGraph.getModule(ti.getText());

		// Item is a box definition
		return findBoxDefContainerFromTreeItem(parent, floGraph).getBoxDefinition(ti.getText());
	}

	/**
	 * Given a BoxDefinitionContainer, find the corresponding TreeItem in the
	 * Tree
	 *
	 * @param container
	 * @param tree
	 * @param floGraph
	 * @return The corresponding TreeItem
	 */
	public static TreeItem findTreeItemFromBoxDefContainer(final BoxDefinitionContainer container, final Tree tree,
			final FloGraph floGraph) {
		if (container == null)
			throw new IllegalArgumentException();
		final BoxDefinitionContainer parentContainer = container.getParent();
		// This is a module
		if (parentContainer == null)
			return tree.getItem(floGraph.getModules().indexOf(container));

		// This is a box definition
		return findTreeItemFromBoxDefContainer(parentContainer, tree, floGraph)
				.getItem(parentContainer.getBoxDefinitions().indexOf(container));
	}
}
