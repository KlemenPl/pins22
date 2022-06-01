package pins.data.symbol;

import pins.common.report.*;
import pins.common.logger.*;

/**
 * Lexical symbol.
 */
public class Symbol implements Locatable, Loggable {

	public final Token token;

	public final String lexeme;

	public final Location location;

	public Symbol(Token token, String lexeme, Location location) {
		this.token = token;
		this.lexeme = lexeme;
		this.location = location;
	}

	@Override
	public String toString() {
		return "(" + token.toString() + "," + lexeme + "," + (location == null ? "" : location.toString()) + ")";
	}

	@Override
	public Location location() {
		return location;
	}

	@Override
	public void log(String pfx) {
		System.out.println(pfx + toString());
	}

}
