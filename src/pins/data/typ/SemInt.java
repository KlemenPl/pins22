package pins.data.typ;

/**
 * Type {@code integer}.
 */
public class SemInt extends SemType {

	@Override
	public long size() {
		return 8;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[31mInt\033[0m");
	}
	
}
