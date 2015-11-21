package flo;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.TreeItem;

import flo.floGraph.BoxDefinition;
import flo.floGraph.BoxDefinitionContainer;
import flo.floGraph.FloGraph;
import flo.floGraph.Module;

/**
 * A listener that keeps the contents of a tree item up to date with the
 * contents of a Box Definition Container
 */
public class BoxDefinitionContainerTreeListener {

	private final TreeItem treeItem;

	public BoxDefinitionContainerTreeListener(final FloGraph floGraph, final BoxDefinitionContainer container,
			final TreeItem treeItem) {
		this.treeItem = treeItem;

		// Listen for when box definitions are added
		container.addBoxDefinitionAddedObserver(e -> {
			final BoxDefinition bd = e.boxDefinition;
			final TreeItem ti = new TreeItem(treeItem, SWT.NONE);
			ti.setText(bd.getBoxInterface().getName());
			new BoxDefinitionContainerTreeListener(floGraph, bd, ti);

			// Expand the parent
			treeItem.setExpanded(true);
		});

		// Listen for when box definitions are removed
		container.addBoxDefinitionRemovedObserver(e -> {
			final int index = e.index;
			final TreeItem ti = treeItem.getItem(index);
			ti.removeAll();
			ti.dispose();

			// Select another box definition (if there is one)
			if (treeItem.getItemCount() > index)
				floGraph.setCurrentBoxDefinition(
						(BoxDefinition) FloTree.findBoxDefContainerFromTreeItem(treeItem.getItem(index), floGraph));
			// treeItem.getParent().select(treeItem.getItem(index));
			else if (treeItem.getItemCount() == index && index > 0) {
				floGraph.setCurrentBoxDefinition(
						(BoxDefinition) FloTree.findBoxDefContainerFromTreeItem(treeItem.getItem(index - 1), floGraph));
				treeItem.getParent().select(treeItem.getItem(index - 1));
			} else {
				final BoxDefinitionContainer bdc = FloTree.findBoxDefContainerFromTreeItem(treeItem, floGraph);
				if (bdc instanceof BoxDefinition)
					floGraph.setCurrentBoxDefinition((BoxDefinition) bdc);
				// treeItem.getParent().select(treeItem);
			}
		});

		// For modules, listen for when the module is renamed
		if (container instanceof Module)
			((Module) container).addModuleRenamedObserver(e -> {
				treeItem.setText(((Module) container).getName());
			});

		// For box definitions, listen for when the box interface is renamed
		if (container instanceof BoxDefinition)
			((BoxDefinition) container).getBoxInterface().addBoxInterfaceRenamedObserver(e -> {
				treeItem.setText(((BoxDefinition) container).getBoxInterface().getName());
			});

		setInitialContents(container, floGraph);
	}

	private void setInitialContents(final BoxDefinitionContainer container, final FloGraph floGraph) {
		TreeItem tiBd;
		for (final BoxDefinition bd : container.getBoxDefinitions()) {
			tiBd = new TreeItem(treeItem, SWT.NONE);
			tiBd.setText(bd.getBoxInterface().getName());
			new BoxDefinitionContainerTreeListener(floGraph, bd, tiBd);
		}
	}
}
