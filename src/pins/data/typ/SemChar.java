package pins.data.typ;

/**
 * Type {@code char}.
 */
public class SemChar extends SemType {

	@Override
	public long size() {
		return 8;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[31mChar\033[0m");
	}

}
