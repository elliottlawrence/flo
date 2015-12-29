package flo.floGraph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;

import flo.Observable.BoxAddedEvent;
import flo.Observable.CurrentBoxDefinitionEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;
import flo.Util.Jsonable;
import flo.Util.Pair;
import flo.Util.Pnt;

/**
 * A box definition consists of the box's interface augmented with a set of
 * boxes and cables connecting nodes on the boxes. In addition, a box may have
 * local box definitions, akin to a let/where clause in Haskell.
 */
public class BoxDefinition extends BoxDefinitionContainer implements Jsonable {

    /**
     * The box's interface
     */
    private final BoxInterface boxInterface;

    /**
     * The list of boxes contained in this box definition and their locations on
     * the canvas
     */
    private final Map<Integer, Pair<BoxInterface, Pnt>> boxes;

    /**
     * The list of cables connecting various boxes
     */
    private final List<Cable> cables;

    /**
     * Observables corresponding to the different events this object can emit
     */
    private final Observable<BoxAddedEvent> boxAddedObservable =
        new Observable<BoxAddedEvent>();
    private final Observable<CurrentBoxDefinitionEvent> currentBoxDefinitionObservable =
        new Observable<CurrentBoxDefinitionEvent>();

    /**
     * Create an empty box definition with the given name and parent
     *
     * @param name
     * @param parent
     */
    public BoxDefinition(final String name,
        final BoxDefinitionContainer parent) {
        super(parent);
        boxInterface = new BoxInterface(name);
        boxes = new HashMap<Integer, Pair<BoxInterface, Pnt>>();
        cables = new ArrayList<Cable>();
    }

    /**
     * Load a box defintition from the given JSON object with the given parent
     *
     * @param jsonObject
     * @param parent
     */
    public BoxDefinition(final JsonObject jsonObject,
        final BoxDefinitionContainer parent) {
        super(jsonObject, parent);

        boxInterface =
            new BoxInterface(jsonObject.getJsonObject("boxInterface"));

        boxes = new HashMap<Integer, Pair<BoxInterface, Pnt>>();
        final List<JsonObject> jsonBoxes =
            jsonObject.getJsonArray("boxes").getValuesAs(JsonObject.class);
        jsonBoxes.forEach(jo -> {
            final int ID = jo.getInt("ID");
            final BoxInterface bi =
                new BoxInterface(jo.getJsonObject("boxInterface"));
            bi.setID(ID);
            final Pnt point = new Pnt(jo.getInt("x"), jo.getInt("y"));
            boxes.put(ID, new Pair<BoxInterface, Pnt>(bi, point));
        });

        cables = new ArrayList<Cable>();
        final List<JsonObject> jsonCables =
            jsonObject.getJsonArray("cables").getValuesAs(JsonObject.class);
        jsonCables.forEach(jo -> cables.add(new Cable(jo, this)));
    }

    // Methods related to boxInterface

    public BoxInterface getBoxInterface() {
        return boxInterface;
    }

    // Methods related to boxes

    public Map<Integer, Pair<BoxInterface, Pnt>> getBoxes() {
        return boxes;
    }

    public int addBox(final BoxInterface bi, final Pnt p) {
        final int ID = getUniqueID();
        bi.setID(ID);
        boxes.put(ID, new Pair<BoxInterface, Pnt>(bi, p));

        boxAddedObservable.notifyObservers(new BoxAddedEvent());
        currentBoxDefinitionObservable
            .notifyObservers(new CurrentBoxDefinitionEvent());

        return ID;
    }

    private int getUniqueID() {
        for (int i = 0;; i++)
            if (!boxes.keySet().contains(i))
                return i;
    }

    public void removeBox(final int ID) {
        final BoxInterface bi = boxes.get(ID).x;

        // Remove any cables attached to box
        bi.getInputs().forEach(i -> {
            if (i.hasCable())
                removeCable(i.getCable());
        });

        final Output o = bi.getOutput();
        if (o.hasCable()) {
            final List<Cable> cablesToRemove =
                new ArrayList<Cable>(o.getCables());
            cablesToRemove.forEach(c -> removeCable(c));
        }

        boxes.remove(ID);
    }

    public void setBoxLocation(final Integer ID, final Pnt point) {
        if (ID == -1)
            return;
        final BoxInterface bi = boxes.get(ID).x;
        boxes.put(ID, new Pair<BoxInterface, Pnt>(bi, point));

        currentBoxDefinitionObservable
            .notifyObservers(new CurrentBoxDefinitionEvent());
    }

    // Methods related to cables

    public List<Cable> getCables() {
        return cables;
    }

    public void addCable(final Cable cable) {
        cables.add(cable);
    }

    public void removeCable(final Cable cable) {
        cable.deleteConnections();
        cables.remove(cable);
    }

    // Methods related to observers

    public void addBoxAddedObserver(final Observer<BoxAddedEvent> o) {
        boxAddedObservable.addObserver(o);
    }

    public void addCurrentBoxDefinitionObserver(
        final Observer<CurrentBoxDefinitionEvent> o) {
        currentBoxDefinitionObservable.addObserver(o);
    }

    public void deleteCurrentBoxDefinitionObserver(
        final Observer<CurrentBoxDefinitionEvent> o) {
        currentBoxDefinitionObservable.deleteObserver(o);
    }

    @Override
    public void deleteObservers() {
        super.deleteObservers();
        boxAddedObservable.deleteObservers();
        currentBoxDefinitionObservable.deleteObservers();
    }

    /**
     * Convert this module to JSON
     */
    @Override
    public JsonObjectBuilder toJsonObjectBuilder() {
        final JsonArrayBuilder boxBuilder = Json.createArrayBuilder();
        boxes.keySet().forEach(i -> {
            final Pair<BoxInterface, Pnt> pair = boxes.get(i);
            final JsonObjectBuilder objectBuilder = Json.createObjectBuilder()
                .add("ID", i).add("boxInterface", pair.x.toJsonObjectBuilder())
                .add("x", pair.y.x).add("y", pair.y.y);
            boxBuilder.add(objectBuilder);
        });

        final JsonArrayBuilder cableBuilder = Json.createArrayBuilder();
        cables.forEach(c -> cableBuilder.add(c.toJsonObjectBuilder()));

        return super.toJsonObjectBuilder()
            .add("boxInterface", boxInterface.toJsonObjectBuilder())
            .add("boxes", boxBuilder).add("cables", cableBuilder);
    }
}
