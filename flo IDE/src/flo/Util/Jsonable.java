package flo.Util;

import javax.json.JsonObjectBuilder;

public interface Jsonable {

    /**
     * Converts this object to a JSON value
     *
     * @return
     */
    public JsonObjectBuilder toJsonObjectBuilder();
}
