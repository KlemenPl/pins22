package pins.data.ast;

import java.util.*;

import pins.common.report.*;
import pins.data.ast.visitor.AstVisitor;

/**
 * A sequence of ASTs.
 */
public class ASTs<AstKind extends AST> extends AST {
	
	private Vector<AstKind> asts;
	
	public ASTs(Location location, Collection<AstKind> asts) {
		super(location);
		this.asts = new Vector<AstKind>(asts);
	}
	
	public Vector<AstKind> asts() {
		return new Vector<AstKind>(asts);
	}
	
	@Override
	public void log(String pfx) {
		//System.out.println(pfx + "ASTs" + " @" + location.toString());
		for (AstKind ast : asts) {
			ast.log(pfx + "  ");
		}
	}

	@Override
	public <Result, Arg> Result accept(AstVisitor<Result, Arg> visitor, Arg arg) {
		return visitor.visit(this, arg);
	}

}
