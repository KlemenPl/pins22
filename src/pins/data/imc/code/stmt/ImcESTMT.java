package pins.data.imc.code.stmt;

import pins.data.imc.code.expr.*;
import pins.data.imc.visitor.*;

/**
 * Expression statement.
 * 
 * Evaluates expression and throws the result away.
 */
public class ImcESTMT extends ImcStmt {

	/** The expression. */
	public final ImcExpr expr;

	/**
	 * Constructs an expression statement.
	 * 
	 * @param expr The expression.
	 */
	public ImcESTMT(ImcExpr expr) {
		this.expr = expr;
	}

	@Override
	public <Result, Arg> Result accept(ImcVisitor<Result, Arg> visitor, Arg accArg) {
		return visitor.visit(this, accArg);
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "ESTMT");
		expr.log(pfx + "  ");
	}

	@Override
	public String toString() {
		return "ESTMT(" + expr.toString() + ")";
	}

}
