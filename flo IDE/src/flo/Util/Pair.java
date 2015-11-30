package flo.Util;

/**
 * Since Java is stupid and doesn't have a built in tuple type
 */
public class Pair<T1, T2> {
	public final T1 x;
	public final T2 y;

	public Pair(final T1 x, final T2 y) {
		this.x = x;
		this.y = y;
	}
}
