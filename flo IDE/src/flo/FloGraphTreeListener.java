package flo;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

/**
 * A listener that keeps the contents of a tree up to date with the contents of
 * a Flo Graph
 */
public class FloGraphTreeListener {

	private final Tree tree;

	public FloGraphTreeListener(final FloGraph floGraph, final Tree tree) {
		this.tree = tree;

		// Listen for when modules are added
		floGraph.addModuleAddedObserver(e -> {
			final Module m = e.module;
			final TreeItem ti = new TreeItem(tree, SWT.NONE);
			ti.setText(m.getName());
			new BoxDefinitionContainerTreeListener(m, ti);

			// Select the module
			tree.select(ti);
		});

		// Listen for when modules are removed
		floGraph.addModuleRemovedObserver(e -> {
			final int index = e.index;
			final TreeItem ti = tree.getItem(index);
			ti.removeAll();
			ti.dispose();

			// Select another module (if there is one)
			if (tree.getItemCount() > index)
				tree.select(tree.getItem(index));
			else if (tree.getItemCount() == index && index > 0)
				tree.select(tree.getItem(index - 1));
		});

		// Listen for when the current box definition changes
		floGraph.addBoxDefinitionSelectedObserver(e -> {
			final BoxDefinition bd = e.boxDefinition;
			if (bd == null)
				return;
			final TreeItem ti = FloTree.findTreeItemFromBoxDefContainer(bd, tree, floGraph);
			tree.select(ti);
		});

		setInitialContents(floGraph);
	}

	private void setInitialContents(final FloGraph floGraph) {
		TreeItem ti;
		for (final Module m : floGraph.getModules()) {
			ti = new TreeItem(tree, SWT.NONE);
			ti.setText(m.getName());
			new BoxDefinitionContainerTreeListener(m, ti);
		}

		// Select the first box definition if there is one
		final ArrayList<Module> modules = floGraph.getModules();
		if (!modules.isEmpty()) {
			final ArrayList<BoxDefinition> bds = modules.get(0).getBoxDefinitions();
			if (!bds.isEmpty())
				floGraph.setCurrentBoxDefinition(bds.get(0));
		}
	}
}
