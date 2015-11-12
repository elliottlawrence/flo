package flo;

import java.util.Observable;
import java.util.Observer;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.TreeItem;

public class BoxDefinitionContainerTreeListener implements Observer {

	private final TreeItem treeItem;
	
	public BoxDefinitionContainerTreeListener(BoxDefinitionContainer container, TreeItem treeItem) {
		container.addObserver(this);
		// If the container is a box definition we want to listen for changes to its interface as well
		if (container instanceof BoxDefinition)
			((BoxDefinition) container).getBoxInterface().addObserver(this);
		
		this.treeItem = treeItem;
		
		setInitialContents(container);
	}
	
	private void setInitialContents(BoxDefinitionContainer container) {
		for (BoxDefinition bd : container.getBoxDefinitions()) {
			TreeItem tiBd = new TreeItem(treeItem, SWT.NONE);
			tiBd.setText(bd.getBoxInterface().getName());
			new BoxDefinitionContainerTreeListener(bd, tiBd);
		}
	}
	
	@Override
	public void update(Observable o, Object arg) {
		Object[] args = (Object []) arg;
		FloGraphChange change = (FloGraphChange) args[0];
		
		TreeItem ti;
		switch (change) {
		case ModuleRenamed:
			Module m = (Module) o;
			treeItem.setText(m.getName());
			break;
			
		case BoxInterfaceRenamed:
			BoxInterface bi = (BoxInterface) o;
			treeItem.setText(bi.getName());
			break;
			
		case BoxDefinitionAdded:
			BoxDefinition bd = (BoxDefinition) args[1];
			ti = new TreeItem(treeItem, SWT.NONE);
			ti.setText(bd.getBoxInterface().getName());
			new BoxDefinitionContainerTreeListener(bd, ti);
			
			// Expand the parent
			treeItem.setExpanded(true);
			
			// Select the new box definition
			ti.getParent().select(ti);
			break;
			
		case BoxDefinitionRemoved:
			int index = (int) args[1];
			ti = treeItem.getItem(index);
			ti.removeAll();
			ti.dispose();
		
			// Select another box definition (if there is one)
			if (treeItem.getItemCount() > index)
				treeItem.getParent().select(treeItem.getItem(index));
			else if (treeItem.getItemCount() == index && index > 0)
				treeItem.getParent().select(treeItem.getItem(index-1));
			else treeItem.getParent().select(treeItem);
			break;
			
		default:
			break;
		}
	}
}
