package pins.data.ast;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/*
 * A parameter declaration.
 */
public class AstParDecl extends AstDecl {

	public AstParDecl(Location location, String name, AstType type) {
		super(location, name, type);
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[1mAstParDecl(" + name + ")\033[0m @(" + location + ")");
		logAttributes(pfx);
		type.log(pfx + "  ");
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
