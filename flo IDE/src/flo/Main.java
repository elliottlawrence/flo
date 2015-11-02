package flo;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;

import swing2swt.layout.BorderLayout;

public class Main {

	protected Shell shell;

	/**
	 * Launch the application.
	 * @param args
	 */
	public static void main(String[] args) {
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
		shell.setSize(515, 353);
		shell.setText("flo");
		shell.setLayout(new BorderLayout(0, 0));
		
		Menu menu = new Menu(shell, SWT.BAR);
		shell.setMenuBar(menu);
		
		MenuItem fileMenuItem = new MenuItem(menu, SWT.CASCADE);
		fileMenuItem.setText("File");
		
		Menu fileMenu = new Menu(fileMenuItem);
		fileMenuItem.setMenu(fileMenu);
		
		MenuItem newMenuItem = new MenuItem(fileMenu, SWT.NONE);
		newMenuItem.setText("New");
		
		MenuItem openMenuItem = new MenuItem(fileMenu, SWT.NONE);
		openMenuItem.setText("Open");
		
		MenuItem closeMenuItem = new MenuItem(fileMenu, SWT.NONE);
		closeMenuItem.setText("Close");
		
		new MenuItem(fileMenu, SWT.SEPARATOR);
		
		MenuItem saveMenuItem = new MenuItem(fileMenu, SWT.NONE);
		saveMenuItem.setText("Save");
		
		MenuItem saveAsMenuItem = new MenuItem(fileMenu, SWT.NONE);
		saveAsMenuItem.setText("Save as");
		
		ToolBar toolBar = new ToolBar(shell, SWT.FLAT | SWT.RIGHT);
		toolBar.setLayoutData(BorderLayout.NORTH);
		
		ToolItem newBoxToolItem = new ToolItem(toolBar, SWT.NONE);
		newBoxToolItem.setText("New Box");
		
		Tree tree = new Tree(shell, SWT.BORDER);
		tree.setLayoutData(BorderLayout.WEST);
		
		Canvas canvas = new Canvas(shell, SWT.NONE);
		canvas.setLayoutData(BorderLayout.CENTER);

	}
}
