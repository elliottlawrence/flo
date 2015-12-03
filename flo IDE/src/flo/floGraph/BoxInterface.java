package flo.floGraph;

import java.util.ArrayList;
import java.util.List;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;

import flo.Observable.BoxInterfaceRenamedEvent;
import flo.Observable.CurrentBoxDefinitionEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;
import flo.Util.Jsonable;

/**
 * A box is a generic representation of functions, constructors, and literals.
 * When used in expressions, these appear as "black boxes", and only their
 * interface is visible. This consists of a name, a set of inputs, and an
 * output. (For symmetry, there is an end input connected to the output so that
 * the last box's output can be connected to an input.)
 */
public class BoxInterface implements Jsonable {

    /**
     * The name of the box
     */
    private String name;

    /**
     * The list of inputs this box accepts
     */
    private final List<Input> inputs;

    /**
     * The input which is attached to the output
     */
    private final Input endInput;

    /**
     * The box's output
     */
    private final Output output;

    /**
     * The ID of this box interface with respect to the box definition it is
     * defined in. In the box interface's own definition, this ID will remain
     * -1.
     */
    private int ID = -1;

    /**
     * Observables corresponding to the different events this object can emit
     */
    private final Observable<BoxInterfaceRenamedEvent> boxInterfaceRenamedObservable =
            new Observable<BoxInterfaceRenamedEvent>();
    private final Observable<CurrentBoxDefinitionEvent> currentBoxDefinitionObservable =
            new Observable<CurrentBoxDefinitionEvent>();

    /**
     * Create an empty box interface with the given name
     *
     * @param name
     */
    public BoxInterface(final String name) {
        this.name = name;
        inputs = new ArrayList<Input>();
        endInput = new Input("endInput", this);
        output = new Output(this);
    }

    /**
     * Load a box interface from the given JSON object
     *
     * @param jsonObject
     */
    public BoxInterface(final JsonObject jsonObject) {
        name = jsonObject.getString("name");

        inputs = new ArrayList<Input>();
        final List<JsonString> jsonInputs =
                jsonObject.getJsonArray("inputs").getValuesAs(JsonString.class);
        jsonInputs.forEach(jo -> inputs.add(new Input(jo.getString(), this)));

        endInput = new Input(name, this);

        output = new Output(this);
    }

    // Methods related to name

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name;

        boxInterfaceRenamedObservable
                .notifyObservers(new BoxInterfaceRenamedEvent());
        currentBoxDefinitionObservable
                .notifyObservers(new CurrentBoxDefinitionEvent());
    }

    // Methods related to inputs

    public List<Input> getInputs() {
        return inputs;
    }

    public Input getInput(final String name) {
        for (final Input i : inputs)
            if (i.getName().equals(name))
                return i;
        return null;
    }

    public boolean containsInput(final String name) {
        return getInput(name) != null;
    }

    public void addInput(final String name) {
        inputs.add(new Input(name, this));
    }

    public void removeInput(final Input input) {
        inputs.remove(input);

        // Remove cables attached to input
        final Cable cable = input.getCable();
        if (cable != null)
            cable.getParent().removeCable(cable);
    }

    public void removeInput(final String name) {
        inputs.remove(getInput(name));
    }

    // Methods related to endInput

    public Input getEndInput() {
        return endInput;
    }

    // Methods related to output

    public Output getOutput() {
        return output;
    }

    // Methods related to ID

    public int getID() {
        return ID;
    }

    public void setID(final int ID) {
        this.ID = ID;
    }

    // Methods related to observers

    public void addBoxInterfaceRenamedObserver(
            final Observer<BoxInterfaceRenamedEvent> o) {
        boxInterfaceRenamedObservable.addObserver(o);
    }

    public void addCurrentBoxDefinitionObserver(
            final Observer<CurrentBoxDefinitionEvent> o) {
        currentBoxDefinitionObservable.addObserver(o);
    }

    public void deleteCurrentBoxDefinitionObserver(
            final Observer<CurrentBoxDefinitionEvent> o) {
        currentBoxDefinitionObservable.deleteObserver(o);
    }

    public void deleteObservers() {
        boxInterfaceRenamedObservable.deleteObservers();
        currentBoxDefinitionObservable.deleteObservers();
    }

    /**
     * Convert this box interface to JSON
     */
    @Override
    public JsonObjectBuilder toJsonObjectBuilder() {
        final JsonArrayBuilder inputBuilder = Json.createArrayBuilder();
        inputs.forEach(i -> inputBuilder.add(i.getName()));

        return Json.createObjectBuilder().add("name", name)
                .add("boxFlavor", BoxFlavor.getBoxFlavor(name).toString())
                .add("inputs", inputBuilder);
    }
}