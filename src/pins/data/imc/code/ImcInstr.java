package pins.data.imc.code;

import pins.common.logger.*;
import pins.data.imc.visitor.*;

/**
 * Intermediate code instruction.
 */
public abstract class ImcInstr implements Loggable {

	public abstract <Result, Arg> Result accept(ImcVisitor<Result, Arg> visitor, Arg accArg);

}
