package flo.floGraph;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;

import flo.Util.Jsonable;

/**
 * A cable connects the output of one box to the input of another.
 */
public class Cable implements Jsonable {

    /**
     * The output this cable is connected to
     */
    private final Output output;

    /**
     * The input this cable is connected to
     */
    private final Input input;

    /**
     * The Box Definition in which this cable is defined.
     */
    private final BoxDefinition parent;

    /**
     * Constructs a cable connecting the output to the input, defined in the
     * given box definition
     *
     * @param output
     * @param input
     * @param parent
     */
    public Cable(final Output output, final Input input,
        final BoxDefinition parent) {
        this.output = output;
        this.input = input;
        this.parent = parent;

        // If the input already has a cable attached to it, remove that first
        if (input.hasCable()) {
            final Cable otherCable = input.getCable();
            otherCable.getParent().removeCable(otherCable);
        }

        // Set connections
        this.input.setCable(this);
        this.output.addCable(this);
    }

    /**
     * Loads a cable from the given JSON object with the given parent
     *
     * @param jo
     * @param parent
     */
    public Cable(final JsonObject jo, final BoxDefinition parent) {
        this.parent = parent;

        final JsonObject inputObject = jo.getJsonObject("input");
        final int inputParentID = inputObject.getInt("parentID");
        final String inputName = inputObject.getString("name");
        input = inputParentID == -1
            ? parent.getBoxInterface().getOutput().getEndInput()
            : parent.getBoxes().get(inputParentID).x.getInput(inputName);

        final JsonObject joOutput = jo.getJsonObject("output");
        final int outputParentID = joOutput.getInt("parentID");
        if (outputParentID == -1) {
            final String endInputName = joOutput.getString("endInputName");
            output = parent.getBoxInterface().getInput(endInputName)
                .getStartOutput();
        } else
            output = parent.getBoxes().get(outputParentID).x.getOutput();

        // Set connections
        input.setCable(this);
        output.addCable(this);
    }

    // Methods related to output

    public Output getOutput() {
        return output;
    }

    // Methods related to input

    public Input getInput() {
        return input;
    }

    public void deleteConnections() {
        input.setCable(null);
        output.removeCable(this);
    }

    // Methods related to parent

    public BoxDefinition getParent() {
        return parent;
    }

    /**
     * Convert this cable to JSON
     */
    @Override
    public JsonObjectBuilder toJsonObjectBuilder() {
        return Json.createObjectBuilder()
            .add("input", input.toJsonObjectBuilder())
            .add("output", output.toJsonObjectBuilder());
    }
}
