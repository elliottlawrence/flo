package flo.floGraph;

import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;

import flo.Observable.ModuleRenamedEvent;
import flo.Observable.Observable;
import flo.Observable.Observer;
import flo.Util.Jsonable;

/**
 * A module consists of a list of definitions.
 */
public class Module extends BoxDefinitionContainer implements Jsonable {

    /**
     * The name of the module
     */
    private String name;

    /**
     * Observables corresponding to the different events this object can emit
     */
    private final Observable<ModuleRenamedEvent> moduleRenamedObservable =
            new Observable<ModuleRenamedEvent>();

    /**
     * Create an empty module with the given name
     *
     * @param name
     */
    public Module(final String name) {
        super(null);
        this.name = name;
    }

    /**
     * Load a module from the given JSON object
     *
     * @param jo
     */
    public Module(final JsonObject jo) {
        super(jo, null);

        name = jo.getString("name");
    }

    // Methods related to name

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name;

        moduleRenamedObservable.notifyObservers(new ModuleRenamedEvent());
    }

    // Methods related to observers

    public void addModuleRenamedObserver(final Observer<ModuleRenamedEvent> o) {
        moduleRenamedObservable.addObserver(o);
    }

    @Override
    public void deleteObservers() {
        super.deleteObservers();
        moduleRenamedObservable.deleteObservers();
    }

    /**
     * Convert this module to JSON
     */
    @Override
    public JsonObjectBuilder toJsonObjectBuilder() {
        return super.toJsonObjectBuilder().add("name", name);
    }
}
