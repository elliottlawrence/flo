package flo;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Sash;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.wb.swt.SWTResourceManager;

import flo.floGraph.BoxDefinition;
import flo.floGraph.BoxDefinitionContainer;
import flo.floGraph.BoxInterface;
import flo.floGraph.FloGraph;

public class Main {

	protected Shell shell;

	private static FloGraph currentFloGraph;
	private static FloTree tree;
	private static FloCanvas floCanvas;

	private static final int MIN_TREE_WIDTH = 80;

	/**
	 * Launch the application.
	 *
	 * @param args
	 */
	public static void main(final String[] args) {
		currentFloGraph = new FloGraph("Untitled");
		currentFloGraph.addModule("Main").addBoxDefinition("main");
		currentFloGraph.addModule("Utils").addBoxDefinition("helper");
		currentFloGraph.addModule("Library").addBoxDefinition("max");

		try {
			final Main window = new Main();
			window.open();
		} catch (final Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Open the window.
	 */
	public void open() {
		Display.setAppName("flo");
		final Display display = Display.getDefault();
		createContents();
		shell.open();
		shell.layout();
		while (!shell.isDisposed())
			if (!display.readAndDispatch())
				display.sleep();
	}

	/**
	 * Create contents of the window.
	 */
	protected void createContents() {
		shell = new Shell();
		shell.setImage(SWTResourceManager.getImage(Main.class, "/Icons/flo.png"));
		shell.setSize(1000, 600);
		shell.setMinimumSize(300, 200);
		shell.setLocation(200, 50);
		shell.setText("flo");
		shell.setLayout(new FormLayout());

		final Menu menu = new Menu(shell, SWT.BAR);
		shell.setMenuBar(menu);

		final MenuItem miFile = new MenuItem(menu, SWT.CASCADE);
		miFile.setText("File");

		final Menu mFile = new Menu(miFile);
		miFile.setMenu(mFile);

		final MenuItem miNew = new MenuItem(mFile, SWT.NONE);
		miNew.setText("New");

		final MenuItem miOpen = new MenuItem(mFile, SWT.NONE);
		miOpen.setText("Open");

		final MenuItem miClose = new MenuItem(mFile, SWT.NONE);
		miClose.setText("Close");

		new MenuItem(mFile, SWT.SEPARATOR);

		final MenuItem miSave = new MenuItem(mFile, SWT.NONE);
		miSave.setText("Save");

		final MenuItem miSaveAs = new MenuItem(mFile, SWT.NONE);
		miSaveAs.setText("Save as");

		final ToolBar toolBar = new ToolBar(shell, SWT.FLAT | SWT.RIGHT);
		final FormData fd_toolBar = new FormData();
		fd_toolBar.left = new FormAttachment(0);
		fd_toolBar.right = new FormAttachment(0, 743);
		fd_toolBar.top = new FormAttachment(0);
		toolBar.setLayoutData(fd_toolBar);

		final ToolItem tiNewModule = new ToolItem(toolBar, SWT.NONE);
		tiNewModule.setText("New Module");
		tiNewModule.setImage(SWTResourceManager.getImage(Main.class, "/Icons/module.png"));
		tiNewModule.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				currentFloGraph.addModule("Module " + currentFloGraph.getModules().size());
			}
		});

		final ToolItem tiNewBoxDefinition = new ToolItem(toolBar, SWT.NONE);
		tiNewBoxDefinition.setText("New Box Definition");
		tiNewBoxDefinition.setImage(SWTResourceManager.getImage(Main.class, "/Icons/box definition.png"));
		tiNewBoxDefinition.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				final TreeItem[] selectedItems = tree.getTree().getSelection();
				if (selectedItems.length == 0)
					return;

				final TreeItem selectedItem = selectedItems[0];
				final BoxDefinitionContainer selectedContainer = FloTree.findBoxDefContainerFromTreeItem(selectedItem,
						currentFloGraph);
				final BoxDefinition newBoxDefinition = selectedContainer
						.addBoxDefinition("boxDefinition" + (selectedContainer.getBoxDefinitions().size() + 1));
				currentFloGraph.setCurrentBoxDefinition(newBoxDefinition);
			}
		});

		final ToolItem tiNewBox = new ToolItem(toolBar, SWT.NONE);
		tiNewBox.setText("New Box");
		tiNewBox.setImage(SWTResourceManager.getImage(Main.class, "/Icons/box.png"));
		tiNewBox.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				final BoxInterface bi = new BoxInterface("newBox");
				final BoxDefinition currentBoxDefinition = currentFloGraph.getCurrentBoxDefinition();
				if (currentBoxDefinition != null)
					currentBoxDefinition.addBox(bi);
			}
		});

		new ToolItem(toolBar, SWT.SEPARATOR);

		final ToolItem tiCompile = new ToolItem(toolBar, SWT.NONE);
		tiCompile.setImage(SWTResourceManager.getImage(Main.class, "/Icons/compile.png"));
		tiCompile.setText("Compile");

		final ToolItem tiRun = new ToolItem(toolBar, SWT.NONE);
		tiRun.setImage(SWTResourceManager.getImage(Main.class, "/Icons/run.png"));
		tiRun.setText("Run");

		new ToolItem(toolBar, SWT.SEPARATOR);

		final ToolItem tiZoomIn = new ToolItem(toolBar, SWT.NONE);
		tiZoomIn.setImage(SWTResourceManager.getImage(Main.class, "/Icons/zoomin.png"));
		tiZoomIn.setText("Zoom In");

		final ToolItem tiZoomOut = new ToolItem(toolBar, SWT.NONE);
		tiZoomOut.setImage(SWTResourceManager.getImage(Main.class, "/Icons/zoomout.png"));
		tiZoomOut.setText("Zoom Out");

		tree = new FloTree(shell, currentFloGraph);
		final Sash sash = new Sash(shell, SWT.VERTICAL);
		floCanvas = new FloCanvas(shell, currentFloGraph);

		// Set FormData properties
		final Tree treetree = tree.getTree();
		final FormData fd_tree = new FormData();
		fd_tree.top = new FormAttachment(toolBar);
		fd_tree.bottom = new FormAttachment(100);
		fd_tree.right = new FormAttachment(sash);
		fd_tree.left = new FormAttachment(0);
		treetree.setLayoutData(fd_tree);

		final FormData fd_sash = new FormData();
		fd_sash.top = new FormAttachment(toolBar);
		fd_sash.bottom = new FormAttachment(100);
		fd_sash.width = 4;
		fd_sash.left = new FormAttachment(0, 200);
		sash.setLayoutData(fd_sash);
		sash.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				final Rectangle sashRect = sash.getBounds();
				final Rectangle shellRect = shell.getClientArea();
				final int right = shellRect.width - sashRect.width - MIN_TREE_WIDTH;
				e.x = Math.max(Math.min(e.x, right), MIN_TREE_WIDTH);
				if (e.x != sashRect.x) {
					fd_sash.left = new FormAttachment(0, e.x);
					shell.layout();
				}
			}
		});

		final FormData fd_floCanvas = new FormData();
		fd_floCanvas.top = new FormAttachment(toolBar);
		fd_floCanvas.bottom = new FormAttachment(100);
		fd_floCanvas.right = new FormAttachment(100);
		fd_floCanvas.left = new FormAttachment(sash);
		floCanvas.setLayoutData(fd_floCanvas);
	}
}
