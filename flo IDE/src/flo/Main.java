package flo;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.TreeEvent;
import org.eclipse.swt.events.TreeListener;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.wb.swt.SWTResourceManager;

import swing2swt.layout.BorderLayout;

public class Main {
	
	private static FloGraph currentFloGraph;

	protected Shell shell;
	private Tree tree;
	
	/**
	 * Launch the application.
	 * @param args
	 */
	public static void main(String[] args) {
		currentFloGraph = new FloGraph("Untitled");
		currentFloGraph.addModule("Main").addBoxDefinition("main");
		currentFloGraph.addModule("Utils").addBoxDefinition("helper");
		currentFloGraph.addModule("Library").addBoxDefinition("max");
		
		try {
			Main window = new Main();
			window.open();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Open the window.
	 */
	public void open() {
		Display.setAppName("flo");
		Display display = Display.getDefault();
		createContents();
		shell.open();
		shell.layout();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}
	}

	/**
	 * Create contents of the window.
	 */
	protected void createContents() {
		shell = new Shell();
		shell.setImage(SWTResourceManager.getImage(Main.class, "/Icons/flo.png"));
		shell.setSize(660, 434);
		shell.setText("flo");
		shell.setLayout(new BorderLayout(0, 0));
		
		Menu menu = new Menu(shell, SWT.BAR);
		shell.setMenuBar(menu);
		
		MenuItem miFile = new MenuItem(menu, SWT.CASCADE);
		miFile.setText("File");
		
		Menu mFile = new Menu(miFile);
		miFile.setMenu(mFile);
		
		MenuItem miNew = new MenuItem(mFile, SWT.NONE);
		miNew.setText("New");
		
		MenuItem miOpen = new MenuItem(mFile, SWT.NONE);
		miOpen.setText("Open");
		
		MenuItem miClose = new MenuItem(mFile, SWT.NONE);
		miClose.setText("Close");
		
		new MenuItem(mFile, SWT.SEPARATOR);
		
		MenuItem miSave = new MenuItem(mFile, SWT.NONE);
		miSave.setText("Save");
		
		MenuItem miSaveAs = new MenuItem(mFile, SWT.NONE);
		miSaveAs.setText("Save as");
		
		ToolBar toolBar = new ToolBar(shell, SWT.FLAT | SWT.RIGHT);
		toolBar.setLayoutData(BorderLayout.NORTH);
		
		ToolItem tiNewModule = new ToolItem(toolBar, SWT.NONE);
		tiNewModule.setImage(SWTResourceManager.getImage(Main.class, "/Icons/module.png"));
		tiNewModule.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				currentFloGraph.addModule("Module " + currentFloGraph.getModules().size());
			}
		});
		tiNewModule.setText("New Module");
		
		ToolItem tiNewBoxDefinition = new ToolItem(toolBar, SWT.NONE);
		tiNewBoxDefinition.setImage(SWTResourceManager.getImage(Main.class, "/Icons/box definition.png"));
		tiNewBoxDefinition.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				TreeItem[] selectedItems = tree.getSelection();
				if (selectedItems.length == 0) return;

				TreeItem selectedItem = selectedItems[0];
				Object selectedObject = findFloGraphObjectFromTreeItem(selectedItem, currentFloGraph);
				
				if (selectedObject instanceof Module) {
					Module selectedModule = (Module) selectedObject;
					selectedModule.addBoxDefinition("boxDefinition" + (selectedModule.getBoxDefinitions().size()+1));
				} else if (selectedObject instanceof BoxDefinition) {
					BoxDefinition selectedBoxDefinition = (BoxDefinition) selectedObject;
					selectedBoxDefinition.addLocalDefinition("boxDefinition" + (selectedBoxDefinition.getLocalDefinitions().size()+1));
				}
			}
		});
		tiNewBoxDefinition.setText("New Box Definition");
		
		ToolItem tiNewBox = new ToolItem(toolBar, SWT.NONE);
		tiNewBox.setImage(SWTResourceManager.getImage(Main.class, "/Icons/box.png"));
		tiNewBox.setText("New Box");
		
		new ToolItem(toolBar, SWT.SEPARATOR);
		
		ToolItem tiCompile = new ToolItem(toolBar, SWT.NONE);
		tiCompile.setImage(SWTResourceManager.getImage(Main.class, "/Icons/compile.png"));
		tiCompile.setText("Compile");
		
		ToolItem tiRun = new ToolItem(toolBar, SWT.NONE);
		tiRun.setImage(SWTResourceManager.getImage(Main.class, "/Icons/run.png"));
		tiRun.setText("Run");
		
		SashForm sashForm = new SashForm(shell, SWT.NONE);
		sashForm.setLayoutData(BorderLayout.CENTER);
		
		tree = new Tree(sashForm, SWT.BORDER);
		tree.addTreeListener(new TreeListener() {
			@Override
			public void treeCollapsed(TreeEvent e) {
				treeExpandedCollapsed(e, false);
			}

			@Override
			public void treeExpanded(TreeEvent e) {
				treeExpandedCollapsed(e, true);
			}
			
			private void treeExpandedCollapsed(TreeEvent e, boolean expanded) {
				TreeItem ti = (TreeItem) e.item;
				Object object = findFloGraphObjectFromTreeItem(ti, currentFloGraph);
				if (object instanceof Module) ((Module) object).setExpanded(expanded);
				else if (object instanceof BoxDefinition) ((BoxDefinition) object).setExpanded(expanded);
			}
		});
		new FloGraphTreeListener(currentFloGraph, tree);
		
		Canvas canvas = new Canvas(sashForm, SWT.NONE);
		sashForm.setWeights(new int[] {1, 3});
	}
	
	/**
	 * Given a TreeItem, find the corresponding object in the Flo Graph.
	 * @param ti
	 * @param floGraph
	 * @return The corresponding object
	 */
	private static Object findFloGraphObjectFromTreeItem(TreeItem ti, FloGraph floGraph) {
		TreeItem parent = ti.getParentItem();
		if (parent == null) {
			// Item is a module
			return floGraph.getModule(ti.getText());
		}
		
		// Item is a box definition
		Object parentObject = findFloGraphObjectFromTreeItem(parent, floGraph);
		if (parentObject instanceof Module) return ((Module) parentObject).getBoxDefinition(ti.getText());
		return ((BoxDefinition) parentObject).getLocalDefinition(ti.getText());
	}
}
