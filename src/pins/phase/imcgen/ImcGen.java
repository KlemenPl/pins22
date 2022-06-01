package pins.phase.imcgen;

import java.util.*;
import pins.data.ast.*;
import pins.data.imc.code.expr.*;
import pins.data.imc.code.stmt.*;

/**
 * Intermediate code generation.
 */
public class ImcGen implements AutoCloseable {

	/** Maps statements to intermediate code. */
	public static final HashMap<AstStmt, ImcStmt> stmtImc = new HashMap<AstStmt, ImcStmt>(0);

	/** Maps expressions to intermediate code. */
	public static final HashMap<AstExpr, ImcExpr> exprImc = new HashMap<AstExpr, ImcExpr>(0);

	/**
	 * Constructs a new phase for intermediate code generation.
	 */
	public ImcGen() {
	}

	public void close() {
	}

}
