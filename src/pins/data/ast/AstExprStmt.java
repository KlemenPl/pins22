package pins.data.ast;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/**
 * An expression statement.
 */
public class AstExprStmt extends AstStmt {
	
	public final AstExpr expr;

	public AstExprStmt(Location location, AstExpr expr) {
		super(location);
		this.expr = expr;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[1mAstExprStmt\033[0m @(" + location + ")");
		logAttributes(pfx);
		expr.log(pfx + "  ");
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
