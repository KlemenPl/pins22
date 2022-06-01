package pins.phase.memory;

import java.util.*;

import pins.data.ast.*;
import pins.data.mem.*;

/**
 * Computing memory layout: frames and accesses.
 */
public class Memory implements AutoCloseable {

    /**
     * Maps function declarations to frames.
     */
    public static final HashMap<AstFunDecl, MemFrame> frames = new HashMap<AstFunDecl, MemFrame>(0);

    /**
     * Maps variable declarations to accesses.
     */
    public static final HashMap<AstVarDecl, MemAccess> varAccesses = new HashMap<AstVarDecl, MemAccess>(0);

    /**
     * Maps parameter declarations to accesses.
     */
    public static final HashMap<AstParDecl, MemRelAccess> parAccesses = new HashMap<AstParDecl, MemRelAccess>(0);

    /**
     * Constructs a new phase for computing layout.
     */
    public Memory() {
    }

    public void close() {
    }

}
