package flo;

import java.util.ArrayList;
import java.util.Observable;
import java.util.Observer;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

/**
 * A listener that keeps the contents of a tree up to date with the contents of
 * a Flo Graph.
 */
public class FloGraphTreeListener implements Observer {

	private final Tree tree;

	public FloGraphTreeListener(final FloGraph floGraph, final Tree tree) {
		this.tree = tree;
		floGraph.addObserver(this);
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

	@Override
	public void update(final Observable o, final Object arg) {
		final FloGraph floGraph = (FloGraph) o;
		final Object[] args = (Object[]) arg;
		final FloGraphChange change = (FloGraphChange) args[0];

		final TreeItem ti;
		switch (change) {
		case ModuleAdded:
			final Module module = (Module) args[1];
			ti = new TreeItem(tree, SWT.NONE);
			ti.setText(module.getName());
			new BoxDefinitionContainerTreeListener(module, ti);

			// Select the new module
			tree.select(ti);
			break;

		case ModuleRemoved:
			final int index = (int) args[1];
			ti = tree.getItem(index);
			ti.removeAll();
			ti.dispose();

			// Select another module (if there is one)
			if (tree.getItemCount() > index)
				tree.select(tree.getItem(index));
			else if (tree.getItemCount() == index && index > 0)
				tree.select(tree.getItem(index - 1));
			break;

		case BoxDefinitionSelected:
			if (!tree.isFocusControl()) {
				final BoxDefinition bd = (BoxDefinition) args[1];
				ti = FloTree.findTreeItemFromBoxDefContainer(bd, tree, floGraph);
				tree.select(ti);
			}
			break;

		default:
			break;
		}
	}
}
