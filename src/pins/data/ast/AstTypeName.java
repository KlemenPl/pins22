package pins.data.ast;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/**
 * A type name.
 */
public class AstTypeName extends AstType implements AstName {

	public final String name;

	public AstTypeName(Location location, String name) {
		super(location);
		this.name = name;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + "\033[1mAstTypeName(" + name + ")\033[0m @(" + location + ")");
		logAttributes(pfx);
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
