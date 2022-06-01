package pins.phase.synan;

import pins.common.report.Location;
import pins.common.report.Report;
import pins.data.ast.*;
import pins.data.symbol.Symbol;
import pins.data.symbol.Token;
import pins.phase.lexan.*;

import java.util.ArrayList;
import java.util.List;

public class SynAn implements AutoCloseable {

    private final LexAn lexan;

    private Symbol prev;
    private Symbol curr;

    final boolean logDerivations = false;

    public SynAn(LexAn lexan) {
        this.lexan = lexan;
        curr = lexan.lexer();
    }

    public void close() {
        lexan.close();
    }

    public AST parser() {
        return parsePrg();
    }

    private ASTs<AstDecl> parsePrg() {
        if (logDerivations) System.out.println("prg -> decls");
        List<AstDecl> decls = new ArrayList<>();
        Location start = curr.location;
        parseDecls(decls);
        Location end = curr.location;
        return new ASTs<>(new Location(start, end), decls);
    }

    private void parseDecls(List<AstDecl> decls) {
        if (logDerivations) System.out.println("decls -> decl decls'");
        parseDecl(decls);
        parseDecls_(decls);
    }

    private void parseDecls_(List<AstDecl> decls) {
        if (match(Token.EOF)) {
            if (logDerivations) System.out.println("decls' -> EOF");
            return;
        } else if (curr.token == Token.RIGHT_PAREN) {
            return;
        }
        if (logDerivations) System.out.println("decls' -> decls");
        parseDecls(decls);
    }

    private void parseDecl(List<AstDecl> decls) {
        if (match(Token.TYP)) decls.add(parseTypDecl());
        else if (match(Token.VAR)) decls.add(parseVarDecl());
        else if (match(Token.FUN)) decls.add(parseFunDecl());
        else throw new Report.Error(curr, "Syntax error: Invalid declaration.");
    }

    private AstTypDecl parseTypDecl() {
        if (logDerivations) System.out.println("decl -> typ identifier = type ;");
        Location start = prev.location;
        String name = curr.lexeme;
        consume(Token.IDENTIFIER, "identifier", "Invalid typ declaration (typ 'identifier' = type ;)");
        consume(Token.EQUAL, "assignment operator '='", "Invalid typ declaration (typ identifier '=' type ;)");
        AstType type = parseType();
        Location end = curr.location;
        consume(Token.SEMICOLON, "semicolon ';'", "Invalid typ declaration, missing semicolon (typ identifier = type ';')");
        return new AstTypDecl(new Location(start, end), name, type);
    }
    private AstVarDecl parseVarDecl() {
        if (logDerivations) System.out.println("decl -> var identifier : type ;");
        Location start = prev.location;
        String name = curr.lexeme;
        consume(Token.IDENTIFIER, "identifier", "Invalid var declaration (var 'identifier' : type ;)");
        consume(Token.COLON, "colon ':'", "Invalid var declaration (var identifier ':' type ;)");
        AstType type = parseType();
        Location end = curr.location;
        consume(Token.SEMICOLON, "semicolon ';'", "Invalid var declaration (var identifier : type ';')");
        return new AstVarDecl(new Location(start, end), name, type);
    }
    private AstFunDecl parseFunDecl() {
        if (logDerivations) System.out.println("decl -> fun identifier ( fun_params ) : type = expr ;");
        Location start = prev.location;
        String name = curr.lexeme;
        consume(Token.IDENTIFIER, "identifier", "Invalid fun declaration (fun 'identifier' (fun_args) : type = expr ;)");
        consume(Token.LEFT_PAREN, "left parenthesis '('", "Invalid fun declaration (fun identifier '(' fun_args) : type = expr ;)");
        List<AstParDecl> pars = new ArrayList<>();
        parseFunParams(pars);
        consume(Token.RIGHT_PAREN, "right parenthesis ')'", "Invalid fun declaration (fun identifier (fun_args ')' : type = expr ;)");
        consume(Token.COLON, "colon ':'", "Invalid fun declaration (fun identifier (fun_args) ':' type = expr ;)");
        AstType ret = parseType();
        consume(Token.EQUAL, "assignment operator '='", "Invalid fun declaration (fun identifier (fun_args) : type '=' expr ;)");
        AstExpr expr = parseExpr();
        consume(Token.SEMICOLON, "semicolon ';'", "Invalid fun declaration (fun identifier (fun_args) : type = expr ';')");
        Location end = prev.location;
        return new AstFunDecl(new Location(start, end), name, new ASTs<>(null, pars), ret, expr);
    }

    private void parseFunParams(List<AstParDecl> parDecls) {
        if (match(Token.IDENTIFIER)) {
            Location start = prev.location;
            String name = prev.lexeme;
            if (logDerivations) System.out.println("fun_params -> identifier : type fun_params'");
            consume(Token.COLON, "colon ':'", "Invalid fun_args (identifier ':' type)");
            AstType type = parseType();
            Location end = curr.location;
            parDecls.add(new AstParDecl(new Location(start, end), name, type));
            parseFunParams_(parDecls);
        } else {
            if (logDerivations) System.out.println("fun_params -> .");
        }
    }
    private void parseFunParams_(List<AstParDecl> parDecls) {
        if (match(Token.COMMA)) {
            if (logDerivations) System.out.println("fun_params' -> , identifier : type fun_params'");
            String name = curr.lexeme;
            Location start = curr.location;
            consume(Token.IDENTIFIER, "identifier", "Invalid fun_args (..., 'identifier' : type)");
            consume(Token.COLON, "colon ':'", "Invalid fun_args (..., identifier ':' type)");
            AstType type = parseType();
            Location end = curr.location;
            parDecls.add(new AstParDecl(new Location(start, end), name, type));
            parseFunParams_(parDecls);
        } else {
            if (logDerivations) System.out.println("fun_params' -> .");
        }
    }

    private AstType parseType() {
        if (match(Token.LEFT_BRACKET)) {
            if (logDerivations) System.out.println("type -> [ expr ] type");
            Location start = prev.location;
            AstExpr size = parseExpr();
            consume(Token.RIGHT_BRACKET, "closing right brackets ']'", "missing closing bracket");
            AstType type = parseType();
            Location end = curr.location;
            return new AstArrType(new Location(start, end), type, size);
        } else if (match(Token.XOR)) {
            if (logDerivations) System.out.println("type -> ^ type");
            Location start = prev.location;
            AstType type = parseType();
            Location end = curr.location;
            return new AstPtrType(new Location(start, end), type);
        } else if (match(Token.LEFT_PAREN)) {
            if (logDerivations) System.out.println("type -> ( type )");
            AstType type = parseType();
            consume(Token.RIGHT_PAREN, "closing right parenthesis ')'", "missing closing parenthesis");
            return type;
        } else if (match(Token.IDENTIFIER)) {
            if (logDerivations) System.out.println("type -> identifier");
            return new AstTypeName(prev.location, prev.lexeme);
        } else if (match(Token.VOID)){
            if (logDerivations) System.out.println("type -> void");
            return new AstAtomType(prev.location, AstAtomType.Kind.VOID);
        } else if (match(Token.CHAR)){
            if (logDerivations) System.out.println("type -> char");
            return new AstAtomType(prev.location, AstAtomType.Kind.CHAR);
        } else if (match(Token.INT)){
            if (logDerivations) System.out.println("type -> int");
            return new AstAtomType(prev.location, AstAtomType.Kind.INT);
        }
        throw error("type", "Was expecting a type");
    }

    private void parseStmts(List<AstStmt> stmts) {
        if (logDerivations) System.out.println("stmts -> stmt stmts'");
        stmts.add(parseStmt());
        parseStmts_(stmts);
    }
    private void parseStmts_(List<AstStmt> stmts) {
        if (match(Token.SEMICOLON)) {
            if (check(Token.RIGHT_BRACE)) {
                if (logDerivations) System.out.println("stmts' -> }");
                return;
            }
            if (logDerivations) System.out.println("stmts' -> ; stmts");
            parseStmts(stmts);
        } else {
            consume(Token.SEMICOLON, "semicolon");
            if (logDerivations) System.out.println("stmts' -> .");
        }
    }

    private void parseStmtsEnd(List<AstStmt> stmts) {
        if (logDerivations) System.out.println("stmtsEnd -> stmt ; stmtsEnd'");
        stmts.add(parseStmt());
        consume(Token.SEMICOLON, "semicolon ';'", "missing semicolon");
        parseStmtsEnd_(stmts);
    }

    private void parseStmtsEnd_(List<AstStmt> stmts) {
        if (check(Token.END)){
            if (logDerivations) System.out.println("stmtsEnd' -> end");
            return;
        }
        if (logDerivations) System.out.println("stmtsEnd' -> stmtsEnd");
        parseStmtsEnd(stmts);
    }

    private void parseStmtsIf(List<AstStmt> stmts) {
        if (logDerivations) System.out.println("stmtsIf -> stmt ; stmtsIf'");
        stmts.add(parseStmt());
        consume(Token.SEMICOLON, "semicolon ';'", "missing semicolon");
        parseStmtsIf_(stmts);
    }
    private void parseStmtsIf_(List<AstStmt> stmts) {
        if (check(Token.ELSE)) {
            if (logDerivations) System.out.println("stmtsIf' -> else");
        } else if (check(Token.END)) {
            if (logDerivations) System.out.println("stmtsIf' -> end");
        } else {
            if (logDerivations) System.out.println("stmtsIf' -> stmtsIf");
            parseStmtsIf(stmts);
        }
    }

    private AstStmt parseStmt() {
        if (match(Token.IF)) {
            if (logDerivations) System.out.println("stmt -> if stmtIf");
            return parseStmtIf();
        } else if (match(Token.WHILE)) {
            if (logDerivations) System.out.println("stmt -> while stmtWhile");
            return parseStmtWhile();
        } else {
            if (logDerivations) System.out.println("stmt -> expr stmt'");
            AstExpr expr = parseExpr();
            return parseStmt_(expr);
        }
    }
    private AstStmt parseStmt_(AstExpr expr) {
        if (match(Token.EQUAL)) {
            // Assignment statement
            if (logDerivations) System.out.println("stmt' -> = expr");
            Location start = expr.location;
            AstExpr lvalue = expr;
            AstExpr rvalue = parseExpr();
            Location end = prev.location;
            return new AstAssignStmt(new Location(start, end), lvalue, rvalue);
        } else {
            if (logDerivations) System.out.println("stmt' -> .");
            return new AstExprStmt(expr.location, expr);
        }
    }
    private AstIfStmt parseStmtIf() {
        if (logDerivations) System.out.println("stmtIf -> expr then stmts stmtIf' ;");
        if (check(Token.THEN)) {
            throw error("expr", "Invalid if syntax (if 'expr' then ...)");
        }
        Location start = prev.location;

        AstExpr cond = parseExpr();
        consume(Token.THEN, "then", "Invalid if syntax (if expr 'then' ...)");

        List<AstStmt> thenStmts = new ArrayList<>();
        Location thenStart = curr.location;
        parseStmtsIf(thenStmts);
        Location thenEnd = prev.location;

        Location elseStart = curr.location;
        List<AstStmt> elseStmts = new ArrayList<>();
        parseStmtIf_(elseStmts);
        Location elseEnd = prev.location;

        AstStmtExpr thenStmt = new AstStmtExpr(new Location(thenStart, thenEnd), new ASTs<>(new Location(thenStart, thenEnd), thenStmts));
        AstStmtExpr elseStmt = new AstStmtExpr(new Location(elseStart, elseEnd), new ASTs<>(new Location(elseStart, elseEnd), elseStmts));

        Location end = prev.location;
        Location loc = new Location(start, end);
        return new AstIfStmt(loc, cond, new AstExprStmt(loc, thenStmt), new AstExprStmt(loc, elseStmt));
    }
    private void parseStmtIf_(List<AstStmt> stmts) {
        if (match(Token.END)) {
            if (logDerivations) System.out.println("stmtIf' -> end");
        } else if (match(Token.ELSE)){
            if (logDerivations) System.out.println("stmtIf' -> else stmts end");
            parseStmtsIf(stmts);
            consume(Token.END, "end", "Invalid if syntax (if expr then stmts [else stmts] 'end';)");
        } else {
            // Error
            consume(Token.END, "end", "Invalid if syntax (if expr then stmts [else stmts] 'end';)");
        }
    }
    private AstWhileStmt parseStmtWhile() {
        if (logDerivations) System.out.println("stmtsWhile -> while expr do stmts end ;");
        Location start = prev.location;
        if (check(Token.DO)) {
            throw error("expr", "Invalid while syntax (while 'expr' do stmts end;)");
        }
        AstExpr cond = parseExpr();
        consume(Token.DO, "do", "Invalid while syntax (while expr 'do' stmts end;)");

        List<AstStmt> stmts = new ArrayList<>();
        Location bodyStart = curr.location;
        parseStmtsEnd(stmts);
        Location bodyEnd = prev.location;

        consume(Token.END, "end", "Invalid while syntax (while expr do stmts 'end';)");
        //consume(Token.SEMICOLON, "missing semicolon ';'", "Invalid while syntax (while expr do stmts 'end ';)");
        Location end = prev.location;

        Location bodyLoc = new Location(bodyStart, bodyEnd);
        AstStmtExpr bodyStmt = new AstStmtExpr(bodyLoc, new ASTs<>(bodyLoc, stmts));

        return new AstWhileStmt(new Location(start, end), cond, new AstExprStmt(bodyLoc, bodyStmt));
    }

    private AstExpr parseExpr() {
         if (logDerivations) System.out.println("expr -> exprDis");
         return parseExprDis();
    }

    private AstExpr parseExprDis() {
        if (logDerivations) System.out.println("exprDis -> exprCon exprDis'");
        AstExpr e1 = parseExprCon();
        return parseExprDis_(e1);
    }
    private AstExpr parseExprDis_(AstExpr e1) {
        if (match(Token.OR)) {
            if (logDerivations) System.out.println("exprDis' -> OR exprCon exprDis'");
            AstExpr e2 = parseExprCon();
            e2 = parseExprDis_(e2);
            return new AstBinExpr(new Location(e1.location, e2.location), AstBinExpr.Oper.OR, e1, e2);
        } else {
            if (logDerivations) System.out.println("exprDis' -> .");
            return e1;
        }
    }

    private AstExpr parseExprCon() {
        if (logDerivations) System.out.println("exprCon -> exprRel exprCon'");
        AstExpr e1 = parseExprRel();
        return parseExprCon_(e1);
    }
    private AstExpr parseExprCon_(AstExpr e1) {
        if (match(Token.AND)) {
            if (logDerivations) System.out.println("exprCon' -> AND exprRel exprCon'");
            AstExpr e2 = parseExprRel();
            e2 = parseExprCon_(e2);
            return new AstBinExpr(new Location(e1.location, e2.location), AstBinExpr.Oper.AND, e1, e2);
        } else {
            if (logDerivations) System.out.println("exprCon' -> .");
            return e1;
        }
    }

    private AstExpr parseExprRel() {
        if (logDerivations) System.out.println("exprRel -> exprAdd'");
        AstExpr e1 = parseExprAdd();
        return parseExprRel_(e1);
    }
    private AstExpr parseExprRel_(AstExpr e1) {
        if (match(Token.EQUAL_EQUAL)) {
            if (logDerivations) System.out.println("exprRel' -> EQ exprAdd");
            AstExpr e2 = parseExprAdd();
            return new AstBinExpr(new Location(e1.location, e2.location), AstBinExpr.Oper.EQU, e1, e2);
        } else if (match(Token.NOT_EQUAL)) {
            if (logDerivations) System.out.println("exprRel' -> NE exprAdd");
            AstExpr e2 = parseExprAdd();
            return new AstBinExpr(new Location(e1.location, e2.location), AstBinExpr.Oper.NEQ, e1, e2);
        } else if (match(Token.LESS)) {
            if (logDerivations) System.out.println("exprRel' -> LT exprAdd");
            AstExpr e2 = parseExprAdd();
            return new AstBinExpr(new Location(e1.location, e2.location), AstBinExpr.Oper.LTH, e1, e2);
        } else if (match(Token.GREAT)) {
            if (logDerivations) System.out.println("exprRel' -> GT exprAdd");
            AstExpr e2 = parseExprAdd();
            return new AstBinExpr(new Location(e1.location, e2.location), AstBinExpr.Oper.GTH, e1, e2);
        } else if (match(Token.LESS_EQUAL)) {
            if (logDerivations) System.out.println("exprRel' -> LE exprAdd'");
            AstExpr e2 = parseExprAdd();
            return new AstBinExpr(new Location(e1.location, e2.location), AstBinExpr.Oper.LEQ, e1, e2);
        } else if (match(Token.GREATER_EQUAL)) {
            if (logDerivations) System.out.println("exprRel' -> GE exprAdd");
            AstExpr e2 = parseExprAdd();
            return new AstBinExpr(new Location(e1.location, e2.location), AstBinExpr.Oper.GEQ, e1, e2);
        } else {
            if (logDerivations) System.out.println("exprRel' -> .");
            return e1;
        }
    }

    private AstExpr parseExprAdd() {
        if (logDerivations) System.out.println("exprAdd -> exprMul exprAdd'");
        AstExpr e1 = parseExprMul();
        return parseExprAdd_(e1);
    }
    private AstExpr parseExprAdd_(AstExpr e1) {
        if (match(Token.PLUS)) {
            if (logDerivations) System.out.println("exprAdd' -> + exprMul exprAdd'");
            AstExpr e2 = parseExprMul();
            return parseExprAdd_(new AstBinExpr(new Location(e1.location, e2.location), AstBinExpr.Oper.ADD, e1, e2));
        } else if (match(Token.MINUS)) {
            if (logDerivations) System.out.println("exprAdd' -> - exprMul exprAdd'");
            AstExpr e2 = parseExprMul();
            return parseExprAdd_(new AstBinExpr(new Location(e1.location, e2.location), AstBinExpr.Oper.SUB, e1, e2));
        } else {
            if (logDerivations) System.out.println("exprAdd' -> .");
            return e1;
        }
    }

    private AstExpr parseExprMul() {
        if (logDerivations) System.out.println("exprMul -> exprPrefix exprMul'");
        AstExpr e1 = parseExprPrefix();
        return parseExprMul_(e1);
    }
    private AstExpr parseExprMul_(AstExpr e1) {
        if (match(Token.STAR)) {
            if (logDerivations) System.out.println("exprMul' -> * exprPrefix exprMul'");
            AstExpr e2 = parseExprPrefix();
            return parseExprMul_(new AstBinExpr(new Location(e1.location, e2.location), AstBinExpr.Oper.MUL, e1, e2));
        } else if (match(Token.SLASH)) {
            if (logDerivations) System.out.println("exprMul' -> / exprPrefix exprMul'");
            AstExpr e2 = parseExprPrefix();
            return parseExprMul_(new AstBinExpr(new Location(e1.location, e2.location), AstBinExpr.Oper.DIV, e1, e2));
        } else if (match(Token.MODULO)) {
            if (logDerivations) System.out.println("exprMul' -> % exprPrefix exprMul'");
            AstExpr e2 = parseExprPrefix();
            return parseExprMul_(new AstBinExpr(new Location(e1.location, e2.location), AstBinExpr.Oper.MOD, e1, e2));
        } else {
            if (logDerivations) System.out.println("exprMul' -> .");
            return e1;
        }
    }

    private AstExpr parseExprPrefix() {
        Location start = curr.location;
        if (match(Token.NOT)) {
            if (logDerivations) System.out.println("exprPrefix -> ! exprPrefix");
            AstExpr e = parseExprPrefix();
            return new AstPreExpr(new Location(start, e.location), AstPreExpr.Oper.NOT, e);
        } else if (match(Token.PLUS)) {
            if (logDerivations) System.out.println("exprPrefix -> + exprPrefix");
            AstExpr e = parseExprPrefix();
            return new AstPreExpr(new Location(start, e.location), AstPreExpr.Oper.ADD, e);
        } else if (match(Token.MINUS)) {
            if (logDerivations) System.out.println("exprPrefix -> - exprPrefix");
            AstExpr e = parseExprPrefix();
            return new AstPreExpr(new Location(start, e.location), AstPreExpr.Oper.SUB, e);
        } else if (match(Token.XOR)) {
            if (logDerivations) System.out.println("exprPrefix -> ^ exprPrefix");
            AstExpr e = parseExprPrefix();
            return new AstPreExpr(new Location(start, e.location), AstPreExpr.Oper.PTR, e);
        } else if (match(Token.NEW)) {
            if (logDerivations) System.out.println("exprPrefix -> new exprPrefix");
            AstExpr e = parseExprPrefix();
            return new AstPreExpr(new Location(start, e.location), AstPreExpr.Oper.NEW, e);
        } else if (match(Token.DEL)) {
            if (logDerivations) System.out.println("exprPrefix -> delete exprPrefix");
            AstExpr e = parseExprPrefix();
            return new AstPreExpr(new Location(start, e.location), AstPreExpr.Oper.DEL, e);
        } else {
            if (logDerivations) System.out.println("exprPrefix -> exprPostfix");
            return parseExprPostfix();
        }
    }

    private AstExpr parseExprPostfix() {
        if (logDerivations) System.out.println("exprPostfix -> exprAtom exprPostfix'");
        AstExpr e1 = parseExprAtom();
        return parseExprPostfix_(e1);
    }
    private AstExpr parseExprPostfix_(AstExpr e1) {
        Location start = e1.location;
        if (match(Token.LEFT_BRACKET)) {
            if (logDerivations) System.out.println("exprPostfix' -> [ expr ] exprPostfix'");
            AstExpr expr = parseExpr();
            consume(Token.RIGHT_BRACKET, "closing right brackets ']'", "missing closing brackets");
            return parseExprPostfix_(new AstBinExpr(new Location(start, expr.location), AstBinExpr.Oper.ARR, e1, expr));
        } else if (match(Token.XOR)) {
            if (logDerivations) System.out.println("exprPostfix' -> ^ exprPostfix'");
            return parseExprPostfix_(new AstPstExpr(new Location(start, e1.location), AstPstExpr.Oper.PTR, e1));
        }else {
            if (logDerivations) System.out.println("exprPostfix' -> .");
            return e1;
        }
    }

    private AstExpr parseExprAtom() {
        if (match(Token.CONST_CHAR)) {
            if (logDerivations) System.out.println("exprAtom -> constChar");
            return new AstConstExpr(prev.location, AstConstExpr.Kind.CHAR, prev.lexeme);
        } else if (match(Token.CONST_INT)) {
            if (logDerivations) System.out.println("exprAtom -> constInt");
            return new AstConstExpr(prev.location, AstConstExpr.Kind.INT, prev.lexeme);
        } else if (match(Token.CONST_PTR)) {
            if (logDerivations) System.out.println("exprAtom -> constPtr");
            return new AstConstExpr(prev.location, AstConstExpr.Kind.PTR, prev.lexeme);
        } else if (match(Token.CONST_VOID)) {
            if (logDerivations) System.out.println("exprAtom -> constVoid");
            return new AstConstExpr(prev.location, AstConstExpr.Kind.VOID, prev.lexeme);
        } else if (match(Token.IDENTIFIER)) {
            if (logDerivations) System.out.println("exprAtom -> identifier exprAtom'");
            return parseExprAtom_(prev);
        } else if (match(Token.LEFT_BRACE)) {
            // Compound statement
            if (logDerivations) System.out.println("exprAtom -> { stmts }");
            List<AstStmt> stmts = new ArrayList<>();
            Location start = prev.location;
            Location innerStart = curr.location;
            parseStmts(stmts);
            Location innerEnd = prev.location;
            consume(Token.RIGHT_BRACE, "closing right braces '}'", "missing closing braces");
            Location end = prev.location;

            ASTs<AstStmt> stmtASTs = new ASTs<>(new Location(innerStart, innerEnd), stmts);
            return new AstStmtExpr(new Location(start, end), stmtASTs);
        } else if (match(Token.LEFT_PAREN)) {
            if (logDerivations) System.out.println("exprAtom -> ( expr exprNested )");
            AstExpr e = parseExpr();
            e =  parseExprNested(e);
            consume(Token.RIGHT_PAREN, "closing right parenthesis ')'", "missing closing parenthesis");
            return e;
        } else {
            throw error("expression atom", null);
        }
    }
    private AstExpr parseExprAtom_(Symbol identifier) {
        if (match(Token.LEFT_PAREN)) {
            if (logDerivations) System.out.println("exprAtom' -> ( exprFunCall )");
            List<AstExpr> args = new ArrayList<>();
            Location start = curr.location;
            parseExprFunCall(args);
            Location end = curr.location;
            consume(Token.RIGHT_PAREN, "right parenthesis ')'", "missing closing parenthesis");
            return new AstCallExpr(new Location(identifier.location, prev.location), identifier.lexeme,
                    new ASTs<>(new Location(start, end), args));
        } else {
            if (logDerivations) System.out.println("exprAtom' -> .");
            return new AstNameExpr(identifier.location, identifier.lexeme);
        }
    }
    private void parseExprFunCall(List<AstExpr> args) {
        if (logDerivations) System.out.println("exprFunCall -> expr exprFunCall'");
        if (!check(Token.RIGHT_PAREN)) {
            AstExpr e = parseExpr();
            args.add(e);
        }
        parseExprFunCall_(args);
    }
    private void parseExprFunCall_(List<AstExpr> args) {
        if (match(Token.COMMA)){
            if (logDerivations) System.out.println("exprFunCall' -> , exprFunCall");
            parseExprFunCall(args);
        } else {
            if (logDerivations) System.out.println("exprFunCall' -> .");
        }
    }
    private AstExpr parseExprNested(AstExpr e) {
        if (match(Token.COLON)) {
            // Type cast expression
            if (logDerivations) System.out.println("exprNested -> : type exprNested'");
            AstType type = parseType();
            return new AstCastExpr(new Location(e.location, type.location), e, type);
        } else if (match(Token.WHERE)) {
            // Where expression
            if (logDerivations) System.out.println("exprNested -> where decls'");
            List<AstDecl> decls = new ArrayList<>();
            Location start = curr.location;
            parseDecls(decls);
            Location end = curr.location;
            ASTs<AstDecl> declsAST = new ASTs<>(new Location(start, end), decls);
            return new AstWhereExpr(new Location(e.location, prev.location), declsAST, e);
        } else {
            // Enclosed expression
            if (logDerivations) System.out.println("exprNested -> .");
            return e;
        }
    }

    private Report.Error error(String expected, String error) {
        String errStr = String.format("Expected: %s, got: \"%s\".", expected, curr.lexeme);
        if (error != null)
            errStr = String.format("%s Error: %s.", errStr, error);
        return new Report.Error(curr, errStr);
    }
    private Symbol consume(Token token, String expected, String error) {
        if (check(token)) return advance();

        throw error(expected, error);
    }
    private Symbol consume(Token token, String expected) {
        return consume(token, expected, null);
    }

    private Symbol advance() {
        prev = curr;
        if (curr.token != Token.EOF) curr = lexan.lexer();
        return prev;
    }

    private boolean check(Token token) {
        return curr.token == token;
    }

    private boolean match(Token... tokens) {
        for (Token type : tokens) {
            if (check(type)) {
                advance();
                return true;
            }
        }
        return false;
    }
}
