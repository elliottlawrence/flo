package flo;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Sash;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.wb.swt.SWTResourceManager;

import flo.Canvas.FloCanvas;
import flo.floGraph.BoxDefinition;
import flo.floGraph.BoxDefinitionContainer;
import flo.floGraph.BoxInterface;
import flo.floGraph.FloGraph;

public class Main {

    protected static Shell shell;

    private static FloGraph currentFloGraph;
    private static FloTree tree;
    private static FloCanvas floCanvas;

    private static final int MIN_TREE_WIDTH = 80;

    private static final double ZOOM_INCREMENT = 0.1;
    private static final double MIN_ZOOM = 0.3;
    private static final double MAX_ZOOM = 2.5;

    private static String savePath;
    private static String openPath;
    private static boolean closing = false;

    private static String[] extensions = { "*.flo" };

    /**
     * Launch the application.
     *
     * @param args
     */
    public static void main(final String[] args) {
        // Uncomment for WindowBuilder to work
        // currentFloGraph = new FloGraph();

        savePath = null;
        if (args.length > 0) {
            currentFloGraph = FloGraph.open(args[0]);
            savePath = args[0];
        } else
            currentFloGraph = createDefaultFloGraph();

        try {
            final Main window = new Main();
            window.open();
        } catch (final Exception e) {
            e.printStackTrace();
        }

        // Open another file if necessary
        if (openPath != null) {
            final String[] args1 = new String[] { openPath };
            openPath = null;
            main(args1);
        }
        // Start a new blank file if necessary
        else if (closing) {
            closing = false;
            main(new String[] {});
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
        shell.setImage(
            SWTResourceManager.getImage(Main.class, "/Icons/flo.png"));
        shell.setSize(1000, 600);
        shell.setMinimumSize(300, 200);
        shell.setLocation(50, 50);
        shell.setLayout(new FormLayout());
        updateShellText();

        tree = new FloTree(shell, currentFloGraph);
        final Sash sash = new Sash(shell, SWT.VERTICAL);
        floCanvas = new FloCanvas(shell, currentFloGraph);

        final Menu menu = new Menu(shell, SWT.BAR);
        shell.setMenuBar(menu);

        final MenuItem miFile = new MenuItem(menu, SWT.CASCADE);
        miFile.setText("File");

        final Menu mFile = new Menu(miFile);
        miFile.setMenu(mFile);

        final MenuItem miNew = new MenuItem(mFile, SWT.NONE);
        miNew.setText("New");
        miNew.setAccelerator(SWT.COMMAND | 'N');
        miNew.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                closing = true;
                shell.close();
            }
        });

        final MenuItem miOpen = new MenuItem(mFile, SWT.NONE);
        miOpen.setText("Open");
        miOpen.setAccelerator(SWT.COMMAND | 'O');
        miOpen.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                // Select a file to open
                final FileDialog dialog = new FileDialog(shell, SWT.OPEN);
                dialog.setFilterExtensions(extensions);
                openPath = dialog.open();
                if (openPath != null)
                    shell.close();
            }
        });

        final MenuItem miClose = new MenuItem(mFile, SWT.NONE);
        miClose.setText("Close");
        miClose.setAccelerator(SWT.COMMAND | 'W');
        miClose.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                closing = true;
                shell.close();
            }
        });

        new MenuItem(mFile, SWT.SEPARATOR);

        final MenuItem miSave = new MenuItem(mFile, SWT.NONE);
        miSave.setText("Save");
        miSave.setAccelerator(SWT.COMMAND | 'S');
        miSave.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                if (savePath == null)
                    saveAs();
                else
                    save();
            }
        });

        final MenuItem miSaveAs = new MenuItem(mFile, SWT.NONE);
        miSaveAs.setText("Save as");
        miSaveAs.setAccelerator(SWT.COMMAND | SWT.SHIFT | 'S');
        miSaveAs.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                saveAs();
            }
        });

        final ToolBar toolBar = new ToolBar(shell, SWT.FLAT | SWT.RIGHT);
        final FormData fd_toolBar = new FormData();
        fd_toolBar.right = new FormAttachment(floCanvas, 0, SWT.RIGHT);
        fd_toolBar.left = new FormAttachment(0);
        fd_toolBar.top = new FormAttachment(0);
        toolBar.setLayoutData(fd_toolBar);

        final ToolItem tiNewModule = new ToolItem(toolBar, SWT.NONE);
        tiNewModule.setText("New Module");
        tiNewModule.setImage(
            SWTResourceManager.getImage(Main.class, "/Icons/module.png"));
        tiNewModule.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                currentFloGraph
                    .addModule("Module " + currentFloGraph.getModules().size());
            }
        });

        final ToolItem tiNewBoxDefinition = new ToolItem(toolBar, SWT.NONE);
        tiNewBoxDefinition.setText("New Box Definition");
        tiNewBoxDefinition.setImage(
            SWTResourceManager
                .getImage(Main.class, "/Icons/box definition.png"));
        tiNewBoxDefinition.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                final TreeItem[] selectedItems = tree.getTree().getSelection();
                if (selectedItems.length == 0)
                    return;

                final TreeItem selectedItem = selectedItems[0];
                final BoxDefinitionContainer selectedContainer =
                    FloTree.findBoxDefContainerFromTreeItem(
                        selectedItem,
                        currentFloGraph);
                final BoxDefinition newBoxDefinition =
                    selectedContainer.addBoxDefinition(
                        "boxDefinition"
                            + (selectedContainer.getBoxDefinitions().size()
                                + 1));
                currentFloGraph.setCurrentBoxDefinition(newBoxDefinition);
            }
        });

        final ToolItem tiNewBox = new ToolItem(toolBar, SWT.NONE);
        tiNewBox.setText("New Box");
        tiNewBox.setImage(
            SWTResourceManager.getImage(Main.class, "/Icons/box.png"));
        tiNewBox.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                final BoxDefinition currentBoxDefinition =
                    currentFloGraph.getCurrentBoxDefinition();
                if (currentBoxDefinition != null) {
                    final BoxInterface bi = new BoxInterface(
                        "box" + (currentBoxDefinition.getBoxes().size() + 1));
                    final int ID = currentBoxDefinition
                        .addBox(bi, floCanvas.getDefaultBoxLocation());
                    floCanvas.setClickedBoxID(ID);
                }
            }
        });
        currentFloGraph.addCurrentBoxDefinitionObserver(
            e -> tiNewBox
                .setEnabled(currentFloGraph.getCurrentBoxDefinition() != null));

        new ToolItem(toolBar, SWT.SEPARATOR);

        final ToolItem tiCompile = new ToolItem(toolBar, SWT.NONE);
        tiCompile.setImage(
            SWTResourceManager.getImage(Main.class, "/Icons/compile.png"));
        tiCompile.setText("Compile");
        tiCompile.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                if (savePath == null) {
                    final MessageBox dialog =
                        new MessageBox(shell, SWT.ICON_WARNING);
                    dialog.setText("File not saved");
                    dialog.setMessage("Please save the file before compiling.");
                    dialog.open();
                    return;
                }

                try {
                    final ProcessBuilder pb =
                        new ProcessBuilder("../floBackend/dist/build/flo/./flo",
                            savePath /* , "-hask" */);
                    pb.inheritIO();
                    pb.start();
                } catch (final IOException e1) {
                    e1.printStackTrace();
                }
            }
        });

        final ToolItem tiRun = new ToolItem(toolBar, SWT.NONE);
        tiRun.setImage(
            SWTResourceManager.getImage(Main.class, "/Icons/run.png"));
        tiRun.setText("Run");
        tiRun.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                if (savePath == null) {
                    final MessageBox dialog =
                        new MessageBox(shell, SWT.ICON_WARNING);
                    dialog.setText("File not saved");
                    dialog.setMessage("Please save the file before running.");
                    dialog.open();
                    return;
                }

                try {
                    final File file = new File(savePath);
                    final String directory = file.getParent();
                    final String exeName =
                        FilenameUtils.removeExtension(file.getName());
                    final ProcessBuilder pb = new ProcessBuilder(
                        "/usr/bin/open", directory + "/./" + exeName);
                    pb.inheritIO();
                    pb.start();
                } catch (final IOException e1) {
                    e1.printStackTrace();
                }
            }
        });

        new ToolItem(toolBar, SWT.SEPARATOR).setWidth(30);

        final Scale scale = new Scale(toolBar, SWT.NONE);
        scale.setMinimum(0);
        scale.setMaximum(22);
        scale.setSelection(7); // MIN_ZOOM + ZOOM_INCREMENT * 7 = 1
        scale.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(final SelectionEvent e) {
                currentFloGraph
                    .setZoom(MIN_ZOOM + ZOOM_INCREMENT * scale.getSelection());
                floCanvas.redraw();
            }
        });
        final ToolItem scaleItem = new ToolItem(toolBar, SWT.SEPARATOR);
        scaleItem.setControl(scale);
        scaleItem.setWidth(100);

        // Zoom in or out when the user performs a pinch gesture
        floCanvas.addGestureListener(e -> {
            if (e.detail == SWT.GESTURE_MAGNIFY) {
                final double magnification = 1 + (e.magnification - 1) * 0.05;
                final double zoom = Math.min(
                    Math.max(
                        currentFloGraph.getZoom() * magnification,
                        MIN_ZOOM),
                    MAX_ZOOM);
                scale.setSelection((int) ((zoom - MIN_ZOOM) / ZOOM_INCREMENT));

                currentFloGraph.setZoom(zoom);
                floCanvas.redraw();
            }
        });

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
                final int right =
                    shellRect.width - sashRect.width - MIN_TREE_WIDTH;
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

    /**
     * Update the title of the window to reflect the current Flo Graph's name
     */
    private static void updateShellText() {
        final String title =
            savePath == null ? "Untitled" : FilenameUtils.getBaseName(savePath);
        shell.setText("flo - " + title);
    }

    /**
     * Create an empty, default Flo Graph
     *
     * @return
     */
    private static FloGraph createDefaultFloGraph() {
        FloGraph floGraph = FloGraph.open("../Sample Programs/Default.flo");
        if (floGraph == null) {
            floGraph = new FloGraph();
            floGraph.addModule("Main").addBoxDefinition("main");
        }
        return floGraph;
    }

    /**
     * Save the current file to a specified path
     */
    private static void saveAs() {
        final FileDialog dialog = new FileDialog(shell, SWT.SAVE);
        dialog.setFileName(FilenameUtils.getBaseName(savePath));
        dialog.setFilterExtensions(extensions);
        savePath = dialog.open();
        if (savePath == null)
            return;

        updateShellText();

        // Save it
        save();
    }

    /**
     * Save the current file
     */
    private static void save() {
        currentFloGraph.save(savePath);
    }
}