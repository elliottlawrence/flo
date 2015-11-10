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

	private Tree tree;
	
	public FloGraphTreeListener(FloGraph floGraph, Tree tree) {
		floGraph.addObserver(this);
		this.tree = tree;
		
		setInitialContents(floGraph);
	}
	
	private void setInitialContents(FloGraph floGraph) {
		for (Module m : floGraph.getModules()) {
			TreeItem ti = new TreeItem(tree, SWT.NONE);
			ti.setText(m.getName());
			
			for (BoxDefinition bd : m.getBoxDefinitions()) {
				TreeItem tiBd = new TreeItem(ti, SWT.NONE);
				tiBd.setText(bd.getBoxInterface().getName());
				
				addLocalDefinitions(bd, tiBd);
				
				tiBd.setExpanded(bd.getExpanded());
			}
			
			ti.setExpanded(m.getExpanded());
		}
	}
	
	private void addLocalDefinitions(BoxDefinition bd, TreeItem tiBd) {
		for (BoxDefinition ld : bd.getLocalDefinitions()) {
			TreeItem tiLd = new TreeItem(tiBd, SWT.NONE);
			tiLd.setText(ld.getBoxInterface().getName());
			
			addLocalDefinitions(ld, tiLd);
			
			tiLd.setExpanded(ld.getExpanded());
		}
	}
	
	@Override
	public void update(Observable o, Object arg) {
		tree.removeAll();
		setInitialContents((FloGraph) o);
	}
}
