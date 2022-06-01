package pins.data.ast;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/**
 * A prefix expression.
 */
public class AstPstExpr extends AstExpr {

	public enum Oper {
		PTR
	};
	
	public final Oper oper;
	
	public final AstExpr subExpr;
	
	public AstPstExpr(Location location, Oper oper, AstExpr subExpr) {
		super(location);
		this.oper = oper;
		this.subExpr = subExpr;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[1mAstPstExpr(" + oper + ")\033[0m @(" + location + ")");
		logAttributes(pfx);
		subExpr.log(pfx + "  ");
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
