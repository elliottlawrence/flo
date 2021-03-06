package flo.floGraph;

import java.util.ArrayList;
import java.util.List;

import javax.json.Json;
import javax.json.JsonObjectBuilder;

import flo.Util.Jsonable;

/**
 * Each box interface has an output
 */
public class Output implements Jsonable {

    /**
     * The cables stemming from this output
     */
    private final List<Cable> cables = new ArrayList<Cable>();

    /**
     * The box interface in which this output is defined
     */
    private final BoxInterface parent;

    /**
     * The input which is attached to this output
     */
    private final Input endInput;

    /**
     * Create the output for the given box interface
     *
     * @param parent
     */
    public Output(final BoxInterface parent, final Input endInput) {
        this.parent = parent;
        this.endInput = endInput;
    }

    // Methods related to cables

    public List<Cable> getCables() {
        return cables;
    }

    public boolean hasCable() {
        return cables.size() > 0;
    }

    public void addCable(final Cable cable) {
        cables.add(cable);
    }

    public void removeCable(final Cable cable) {
        cables.remove(cable);
    }

    // Methods related to endInput

    public Input getEndInput() {
        return endInput;
    }

    /**
     * Convert this output to JSON
     */
    @Override
    public JsonObjectBuilder toJsonObjectBuilder() {
        return Json.createObjectBuilder().add("parentID", parent.getID())
            .add("endInputName", endInput.getName());
    }
}
