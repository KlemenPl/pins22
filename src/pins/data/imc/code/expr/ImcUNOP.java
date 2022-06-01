package pins.data.imc.code.expr;

import pins.data.imc.visitor.*;

/**
 * Unary operation (logical, arithmetic).
 * 
 * Evaluates the value of the operand, performs the selected unary operation and
 * return its result.
 */
public class ImcUNOP extends ImcExpr {

	public enum Oper {
		NOT, NEG,
	}

	/** The operator. */
	public final Oper oper;

	/** The operand. */
	public final ImcExpr subExpr;

	/**
	 * Constructs a unary operation.
	 * 
	 * @param oper    The operator.
	 * @param subExpr The operand.
	 */
	public ImcUNOP(Oper oper, ImcExpr subExpr) {
		this.oper = oper;
		this.subExpr = subExpr;
	}

	@Override
	public <Result, Arg> Result accept(ImcVisitor<Result, Arg> visitor, Arg accArg) {
		return visitor.visit(this, accArg);
	}
	
	@Override
	public void log(String pfx) {
		System.out.println(pfx + "UNOP(" + oper + ")");
		subExpr.log(pfx + "  ");
	}

	@Override
	public String toString() {
		return "UNOP(" + oper + "," + subExpr.toString() + ")";
	}

}
