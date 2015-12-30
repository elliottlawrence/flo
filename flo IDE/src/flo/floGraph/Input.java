package flo.floGraph;

import javax.json.Json;
import javax.json.JsonObjectBuilder;

import flo.Util.Jsonable;

/**
 * An input to a function has a name.
 */
public class Input implements Jsonable {

    /**
     * The name of the input
     */
    private String name;

    /**
     * The box interface in which this input is defined
     */
    private final BoxInterface parent;

    /**
     * The cable attached to this input, if there is one
     */
    private Cable cable;

    /**
     * The output which is attached to this input
     */
    private final Output startOutput;

    /**
     * Create an input with the given name of the given box interface
     *
     * @param name
     * @param parent
     */
    public Input(final String name, final BoxInterface parent) {
        this.name = name;
        this.parent = parent;
        startOutput = new Output(parent, this);
    }

    // Methods related to name

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name;
    }

    // Methods related to parent

    public BoxInterface getParent() {
        return parent;
    }

    // Methods related to cable

    public Cable getCable() {
        return cable;
    }

    public void setCable(final Cable cable) {
        this.cable = cable;
    }

    public boolean hasCable() {
        return cable != null;
    }

    // Methods related to startOutput

    public Output getStartOutput() {
        return startOutput;
    }

    /**
     * Convert this input to JSON
     */
    @Override
    public JsonObjectBuilder toJsonObjectBuilder() {
        return Json.createObjectBuilder().add("parentID", parent.getID())
            .add("name", name);
    }
}
