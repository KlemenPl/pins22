package pins.data.imc.code.expr;

import pins.data.imc.code.stmt.*;
import pins.data.imc.visitor.*;

/**
 * Statement expression.
 * 
 * Executes the statement, evaluates the expression and returns its value.
 */
public class ImcSEXPR extends ImcExpr {

	/** The statement. */
	public final ImcStmt stmt;

	/** The expression. */
	public final ImcExpr expr;

	/**
	 * Constructs a statement expression.
	 * 
	 * @param stmt The statement.
	 * @param expr The expression.
	 */
	public ImcSEXPR(ImcStmt stmt, ImcExpr expr) {
		this.stmt = stmt;
		this.expr = expr;
	}

	@Override
	public <Result, Arg> Result accept(ImcVisitor<Result, Arg> visitor, Arg accArg) {
		return visitor.visit(this, accArg);
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "SEXPR");
		stmt.log(pfx + "  ");
		expr.log(pfx + "  ");
	}

	@Override
	public String toString() {
		return "SEXPR(" + stmt.toString() + "," + expr.toString() + ")";
	}

}
