package flo.Observable;

import java.util.ArrayList;
import java.util.List;

/**
 * A type-safe implementation of the Observable pattern because the original
 * designers of Java were morons.
 */
public class Observable<T> {
    private final List<Observer<T>> observers = new ArrayList<Observer<T>>();

    public void addObserver(final Observer<T> observer) {
        observers.add(observer);
    }

    public void deleteObserver(final Observer<T> observer) {
        observers.remove(observer);
    }

    public void deleteObservers() {
        observers.clear();
    }

    public void notifyObservers(final T e) {
        observers.forEach(observer -> observer.update(e));
    }
}
