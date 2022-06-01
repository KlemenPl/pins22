package pins.data.ast;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/**
 * A conditional statement.
 */
public class AstIfStmt extends AstStmt {

	public final AstExpr condExpr;

	public final AstStmt thenBodyStmt;

	public final AstStmt elseBodyStmt;

	public AstIfStmt(Location location, AstExpr condExpr, AstStmt thenBodyStmt) {
		super(location);
		this.condExpr = condExpr;
		this.thenBodyStmt = thenBodyStmt;
		this.elseBodyStmt = null;
	}

	public AstIfStmt(Location location, AstExpr condExpr, AstStmt thenBodyStmt, AstStmt elseBodyStmt) {
		super(location);
		this.condExpr = condExpr;
		this.thenBodyStmt = thenBodyStmt;
		this.elseBodyStmt = elseBodyStmt;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[1mAstIfStmt\033[0m @(" + location + ")");
		logAttributes(pfx);
		condExpr.log(pfx + "  ");
		thenBodyStmt.log(pfx + "  ");
		if (elseBodyStmt != null) elseBodyStmt.log(pfx + "  ");
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
