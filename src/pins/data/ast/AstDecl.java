package pins.data.ast;

import pins.common.report.*;

/**
 * A declaration.
 */
public abstract class AstDecl extends AST {

	public final String name;
	
	public final AstType type;
	
	public AstDecl(Location location, String name, AstType type) {
		super(location);
		this.name = name;
		this.type = type;
	}

}
