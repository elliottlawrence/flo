package flo;

import java.util.Observable;
import java.util.Observer;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

/**
 * A listener that keeps the contents of a tree up to date with
 * the contents of a Flo Graph.
 */
public class FloGraphTreeListener implements Observer {

	private final Tree tree;
	
	public FloGraphTreeListener(FloGraph floGraph, Tree tree) {
		this.tree = tree;
		floGraph.addObserver(this);
		setInitialContents(floGraph);
	}
	
	private void setInitialContents(FloGraph floGraph) {
		for (Module m : floGraph.getModules()) {
			TreeItem ti = new TreeItem(tree, SWT.NONE);
			ti.setText(m.getName());
			new BoxDefinitionContainerTreeListener(m, ti);
		}
	}
	
	@Override
	public void update(Observable o, Object arg) {
		FloGraph floGraph = (FloGraph) o;
		Object[] args = (Object []) arg;
		FloGraphChange change = (FloGraphChange) args[0];
		
		TreeItem ti;
		switch (change) {
		case ModuleAdded:
			Module module = (Module) args[1];
			ti = new TreeItem(tree, SWT.NONE);
			ti.setText(module.getName());
			new BoxDefinitionContainerTreeListener(module, ti);
			
			// Select the new module
			tree.select(ti);
			break;
			
		case ModuleRemoved:
			int index = (int) args[1];
			ti = tree.getItem(index);
			ti.removeAll();
			ti.dispose();
			
			// Select another module (if there is one)
			if (tree.getItemCount() > index) tree.select(tree.getItem(index));
			else if (tree.getItemCount() == index && index > 0) tree.select(tree.getItem(index-1));
			break;
			
		case BoxDefinitionSelected:
			if (!tree.isFocusControl()) {
				BoxDefinition bd = (BoxDefinition) args[1];
				ti = FloTree.findTreeItemFromBoxDefContainer(bd, tree, floGraph);
				tree.select(ti);
			}
			break;
			
		default:
			break;
		}
	}
}
