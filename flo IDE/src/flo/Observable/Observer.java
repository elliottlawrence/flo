package flo.Observable;

/**
 * A type-safe implementation of the Observer pattern because the original
 * designers of Java were morons.
 */
public interface Observer<T> {
    public void update(T e);
}
