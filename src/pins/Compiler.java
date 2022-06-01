package pins;

import java.util.*;
import pins.common.report.*;
import pins.data.ast.*;
import pins.phase.imclin.ChunkGenerator;
import pins.phase.imclin.ImcLin;
import pins.phase.imclin.Interpreter;
import pins.phase.lexan.*;
import pins.phase.synan.*;
import pins.phase.seman.*;
import pins.phase.memory.*;
import pins.phase.imcgen.*;

/**
 * The PINS'22 compiler.
 */
public class Compiler {

	/** All phases of the compiler. */
	private static final String phases = "none|lexan|synan|abstr|seman|memory|imcgen|imclin";

	/** Values of command line arguments. */
	private static HashMap<String, String> cmdLine = new HashMap<String, String>();

	public static void main(String[] args) {

		try {
			Report.info("This is the PINS'22 compiler:");

			// Scan the command line.
			for (int argc = 0; argc < args.length; argc++) {
				if (args[argc].startsWith("--")) {
					// Command-line switch.
					if (args[argc].matches("--src-file-name=.*")) {
						if (cmdLine.get("--src-file-name") == null) {
							cmdLine.put("--src-file-name", args[argc]);
							continue;
						}
					}
					if (args[argc].matches("--dst-file-name=.*")) {
						if (cmdLine.get("--dst-file-name") == null) {
							cmdLine.put("--dst-file-name", args[argc]);
							continue;
						}
					}
					if (args[argc].matches("--target-phase=(" + phases + "|all)")) {
						if (cmdLine.get("--target-phase") == null) {
							cmdLine.put("--target-phase", args[argc].replaceFirst("^[^=]*=", ""));
							continue;
						}
					}
					if (args[argc].matches("--logged-phase=(" + phases + "|all)")) {
						if (cmdLine.get("--logged-phase") == null) {
							cmdLine.put("--logged-phase", args[argc].replaceFirst("^[^=]*=", ""));
							continue;
						}
					}
					Report.warning("Command line argument '" + args[argc] + "' ignored.");
				} else {
					// Source file name.
					if (cmdLine.get("--src-file-name") == null) {
						cmdLine.put("--src-file-name", args[argc]);
					} else {
						Report.warning("Source file '" + args[argc] + "' ignored.");
					}
				}
			}
			if (cmdLine.get("--src-file-name") == null) {
				throw new Report.Error("Source file not specified.");
			}
			if (cmdLine.get("--dst-file-name") == null) {
				cmdLine.put("--dst-file-name", cmdLine.get("--src-file-name").replaceFirst("\\.[^./]*$", "") + ".mms");
			}
			if ((cmdLine.get("--target-phase") == null) || (cmdLine.get("--target-phase").equals("all"))) {
				cmdLine.put("--target-phase", phases.replaceFirst("^.*\\|", ""));
			}

			// The compilation process carried out phase by phase.
			while (true) {

				// Lexical analysis.
				if (cmdLine.get("--target-phase").equals("lexan")) {
					try (LexAn lexan = new LexAn(cmdLine.get("--src-file-name"))) {
						pins.data.symbol.Symbol symbol;
						do {
							symbol = lexan.lexer();
							symbol.log("");
						} while (symbol.token != pins.data.symbol.Token.EOF);
					}
					break;
				}

				// Syntax analysis.
				AST ast = null;
				try (LexAn lexan = new LexAn(cmdLine.get("--src-file-name")); SynAn synan = new SynAn(lexan)) {
					ast = synan.parser();
				}
				if (cmdLine.get("--target-phase").equals("synan"))
					break;

				// Abstract syntax.
				if (cmdLine.get("--target-phase").equals("abstr")) {
					ast.log("");
					break;
				}

				// Semantic analysis.
				try (SemAn seman = new SemAn()) {
					NameResolver nameResolver = new NameResolver();
					ast.accept(nameResolver, NameResolver.Mode.INSERT_NAME);
					ast.accept(nameResolver, NameResolver.Mode.RESOLVE_NAME);

					TypeChecker typeChecker = new TypeChecker();
					ast.accept(typeChecker, TypeChecker.Mode.RESOLVE_TYPE);
					ast.accept(typeChecker, TypeChecker.Mode.CHECK_TYPE);
				}
				if (cmdLine.get("--target-phase").equals("seman")) {
					ast.log("");
					break;
				}

				// Memory: accesses and stack frames.
				try (Memory memory = new Memory()) {
					ast.accept(new MemEvaluator(), null);
				}
				if (cmdLine.get("--target-phase").equals("memory")) {
					ast.log("");
					break;
				}
				
				// Intermediate code generation.
				try (ImcGen imcgen = new ImcGen()) {
					ast.accept(new CodeGenerator(), null);
				}
				if (cmdLine.get("--target-phase").equals("imcgen")) {
					ast.log("");
					break;
				}

				// Linearization of intermediate code.
				try (ImcLin imclin = new ImcLin()) {
					ast.accept(new ChunkGenerator(), null);
					//ast.log(" ");
					Interpreter interpreter = new Interpreter(ImcLin.dataChunks(), ImcLin.codeChunks());
					System.out.println("EXIT CODE: " + interpreter.run("_main"));
				}
				if (cmdLine.get("--target-phase").equals("imclin"))
					break;

				break;

			}

			Report.info("Done.");
		} catch (

		Report.Error __) {
			System.exit(1);
		}
	}

}
