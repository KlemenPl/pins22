package pins.phase.seman;

import java.util.*;
import pins.data.ast.*;
import pins.data.typ.*;

public class SemAn implements AutoCloseable {

	/** Maps names to declarations. */
	public static final HashMap<AstName, AstDecl> declaredAt = new HashMap<AstName, AstDecl>();

	/** Maps type declarations to semantic representations of types. */
	public static final HashMap<AstTypDecl, SemName> declaresType = new HashMap<AstTypDecl, SemName>(0);

	/** Maps syntax types to semantic representations of types. */
	public static final HashMap<AstType, SemType> describesType = new HashMap<AstType, SemType>(0);

	/** Maps expressions to semantic representations of types. */
	public static final HashMap<AstExpr, SemType> exprOfType = new HashMap<AstExpr, SemType>(0);

	/** Maps statements to semantic representations of types. */
	public static final HashMap<AstStmt, SemType> stmtOfType = new HashMap<AstStmt, SemType>(0);
	
	public SemAn() {		
	}
	
	public void close() {
	}

}
