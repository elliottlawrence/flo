package flo.floGraph;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;

import flo.Util.Jsonable;

/**
 * A cable connects the output of one box to the input of another.
 */
public class Cable implements Jsonable {

	private final Output output;
	private final Input input;

	/**
	 * The Box Definition in which this cable is defined.
	 */
	private final BoxDefinition parent;

	public Cable(final Output output, final Input input, final BoxDefinition parent) {
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

	public Cable(final JsonObject jo, final BoxDefinition parent) {
		this.parent = parent;

		final JsonObject inputObject = jo.getJsonObject("input");
		final int inputParentID = inputObject.getInt("parentID");
		final String inputName = inputObject.getString("name");
		input = inputParentID == -1 ? parent.getBoxInterface().getEndInput()
				: parent.getBoxes().get(inputParentID).x.getInput(inputName);

		final int outputParentID = jo.getJsonObject("output").getInt("parentID");
		output = parent.getBoxes().get(outputParentID).x.getOutput();

		// Set connections
		input.setCable(this);
		output.addCable(this);
	}

	public Input getInput() {
		return input;
	}

	public Output getOutput() {
		return output;
	}

	public void deleteConnections() {
		input.setCable(null);
		output.removeCable(this);
	}

	public BoxDefinition getParent() {
		return parent;
	}

	/**
	 * Convert this cable to JSON
	 */
	@Override
	public JsonObjectBuilder toJsonObjectBuilder() {
		return Json.createObjectBuilder().add("input", input.toJsonObjectBuilder()).add("output",
				output.toJsonObjectBuilder());
	}
}
