package pins.data.imc.code.expr;

import pins.data.imc.visitor.*;

/**
 * Constant.
 * 
 * Returns the value of a constant.
 */
public class ImcCONST extends ImcExpr {

	/** The value. */
	public final long value;

	/**
	 * Constructs a new constant.
	 * 
	 * @param value The value.
	 */
	public ImcCONST(long value) {
		this.value = value;
	}

	@Override
	public <Result, Arg> Result accept(ImcVisitor<Result, Arg> visitor, Arg accArg) {
		return visitor.visit(this, accArg);
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "CONST(" + value + ")");
	}

	@Override
	public String toString() {
		return "CONST(" + value + ")";
	}

}
