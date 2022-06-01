package pins.data.typ;

/**
 * Type {@code void}.
 */
public class SemVoid extends SemType {

	@Override
	public long size() {
		return 0;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[31mVoid\033[0m");
	}

}
