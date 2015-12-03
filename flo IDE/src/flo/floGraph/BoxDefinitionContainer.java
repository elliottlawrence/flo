package flo.floGraph;

import java.util.ArrayList;
import java.util.List;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;

import flo.Observable.BoxDefinitionAddedEvent;
import flo.Observable.BoxDefinitionRemovedEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;
import flo.Util.Jsonable;

/**
 * A common superclass for Modules and Box Definitions, both of which can
 * contain other Box Definitions.
 */
public abstract class BoxDefinitionContainer implements Jsonable {

    /**
     * The box definitions/local definitions that are defined in this box
     * container
     */
    private final List<BoxDefinition> boxDefinitions;

    /**
     * The box definition container in which this is defined. For modules, this
     * field is null.
     */
    private final BoxDefinitionContainer parent;

    /**
     * Observables corresponding to the different events this object can emit
     */
    private final Observable<BoxDefinitionAddedEvent> boxDefinitionAddedObservable =
            new Observable<BoxDefinitionAddedEvent>();
    private final Observable<BoxDefinitionRemovedEvent> boxDefinitionRemovedObservable =
            new Observable<BoxDefinitionRemovedEvent>();

    /**
     * Create an empty box definition container which is a child of the given
     * box definition container
     *
     * @param parent
     */
    public BoxDefinitionContainer(final BoxDefinitionContainer parent) {
        this.parent = parent;
        boxDefinitions = new ArrayList<BoxDefinition>();
    }

    /**
     * Load a box definition container from the given JSON object with the given
     * parent
     *
     * @param jsonObject
     * @param parent
     */
    public BoxDefinitionContainer(final JsonObject jsonObject,
            final BoxDefinitionContainer parent) {
        this.parent = parent;

        boxDefinitions = new ArrayList<BoxDefinition>();
        final List<JsonObject> jsonBoxDefinitions = jsonObject
                .getJsonArray("boxDefinitions").getValuesAs(JsonObject.class);
        jsonBoxDefinitions
                .forEach(jo -> boxDefinitions.add(new BoxDefinition(jo, this)));
    }

    // Methods related to boxDefinitions

    public List<BoxDefinition> getBoxDefinitions() {
        return boxDefinitions;
    }

    public BoxDefinition getBoxDefinition(final String name) {
        for (final BoxDefinition bd : boxDefinitions)
            if (bd.getBoxInterface().getName().equals(name))
                return bd;
        return null;
    }

    public BoxDefinition addBoxDefinition(final String name) {
        final BoxDefinition bd = new BoxDefinition(name, this);
        boxDefinitions.add(bd);

        boxDefinitionAddedObservable
                .notifyObservers(new BoxDefinitionAddedEvent(bd));
        return bd;
    }

    public void removeBoxDefinition(final String name) {
        removeBoxDefinition(getBoxDefinition(name));
    }

    public void removeBoxDefinition(final BoxDefinition bd) {
        final int index = boxDefinitions.indexOf(bd);
        bd.deleteObservers();
        boxDefinitions.remove(bd);

        boxDefinitionRemovedObservable
                .notifyObservers(new BoxDefinitionRemovedEvent(index));
    }

    /**
     * Returns true if this contains a certain box definition, at any level
     *
     * @param boxDefinition
     * @return
     */
    public boolean contains(final BoxDefinition boxDefinition) {
        for (final BoxDefinition bd : boxDefinitions)
            if (bd == boxDefinition || bd.contains(boxDefinition))
                return true;
        return false;
    }

    // Methods related to parent

    public BoxDefinitionContainer getParent() {
        return parent;
    }

    // Methods related to observers

    public void addBoxDefinitionAddedObserver(
            final Observer<BoxDefinitionAddedEvent> o) {
        boxDefinitionAddedObservable.addObserver(o);
    }

    public void addBoxDefinitionRemovedObserver(
            final Observer<BoxDefinitionRemovedEvent> o) {
        boxDefinitionRemovedObservable.addObserver(o);
    }

    public void deleteObservers() {
        boxDefinitionAddedObservable.deleteObservers();
        boxDefinitionRemovedObservable.deleteObservers();
    }

    /**
     * Convert this box definition container to JSON
     */
    @Override
    public JsonObjectBuilder toJsonObjectBuilder() {
        final JsonArrayBuilder boxDefinitionsBuilder =
                Json.createArrayBuilder();
        boxDefinitions.forEach(
                bd -> boxDefinitionsBuilder.add(bd.toJsonObjectBuilder()));

        return Json.createObjectBuilder().add("boxDefinitions",
                boxDefinitionsBuilder);
    }
}
