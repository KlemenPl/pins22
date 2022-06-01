package pins.phase.seman;

import pins.common.report.Report;
import pins.data.ast.*;
import pins.data.ast.visitor.AstFullVisitor;

public class NameResolver extends AstFullVisitor<Void, NameResolver.Mode> {

    public enum Mode {
        RESOLVE_NAME,
        INSERT_NAME,
    }

    private final SymbTable symbTable = new SymbTable();

    @Override
    public Void visit(AstVarDecl varDecl, Mode mode) {
        if (mode == Mode.RESOLVE_NAME) {
            varDecl.type.accept(this, mode);
        } else {
            try {
                symbTable.ins(varDecl.name, varDecl);
            } catch (SymbTable.CannotInsNameException e) {
                throw errorAlreadyDefined(varDecl, varDecl.name, "variable");
            }
        }
        return null;
    }

    @Override
    public Void visit(AstTypDecl typDecl, Mode mode) {
        if (mode == Mode.RESOLVE_NAME) {
            typDecl.type.accept(this, mode);
        } else {
            try {
                symbTable.ins(typDecl.name, typDecl);
            } catch (SymbTable.CannotInsNameException e) {
                throw errorAlreadyDefined(typDecl, typDecl.name, "type");
            }
        }
        return null;
    }

    @Override
    public Void visit(AstFunDecl funDecl, Mode mode) {
        if (mode == Mode.RESOLVE_NAME) {
            funDecl.pars.accept(this, mode);
            funDecl.type.accept(this, mode);
            symbTable.newScope();
            funDecl.pars.accept(this, Mode.INSERT_NAME);
            funDecl.expr.accept(this, mode);
            symbTable.oldScope();
        } else {
            try {
                symbTable.ins(funDecl.name, funDecl);
            } catch (SymbTable.CannotInsNameException e) {
                throw errorAlreadyDefined(funDecl, funDecl.name, "function");
            }
        }
        return null;
    }

    @Override
    public Void visit(AstParDecl parDecl, Mode mode) {
        if (mode == Mode.RESOLVE_NAME) {
            parDecl.type.accept(this, mode);
        } else {
            try {
                symbTable.ins(parDecl.name, parDecl);
            } catch (SymbTable.CannotInsNameException e) {
                throw errorAlreadyDefined(parDecl, parDecl.name, "parameter");
            }
        }
        return null;
    }

    @Override
    public Void visit(AstTypeName typeName, Mode mode) {
        if (mode == Mode.RESOLVE_NAME) {
            try {
                AstDecl decl = symbTable.fnd(typeName.name);
                SemAn.declaredAt.put(typeName, decl);
            } catch (SymbTable.CannotFndNameException e) {
                throw errorNotDefined(typeName, typeName.name, "type");
            }
        }
        return null;
    }

    @Override
    public Void visit(AstNameExpr nameExpr, Mode mode) {
        if (mode == Mode.RESOLVE_NAME) {
            try {
                AstDecl decl = symbTable.fnd(nameExpr.name);
                SemAn.declaredAt.put(nameExpr, decl);
            } catch (SymbTable.CannotFndNameException e) {
                throw errorNotDefined(nameExpr, nameExpr.name, "variable");
            }
        }
        return null;
    }

    @Override
    public Void visit(AstCallExpr callExpr, Mode mode) {
        if (mode == Mode.RESOLVE_NAME) {
            try {
                AstDecl decl = symbTable.fnd(callExpr.name);
                SemAn.declaredAt.put(callExpr, decl);
                callExpr.args.accept(this, mode);
            } catch (SymbTable.CannotFndNameException e) {
                throw errorNotDefined(callExpr, callExpr.name, "function");
            }
        }
        return null;
    }

    @Override
    public Void visit(AstWhereExpr whereExpr, Mode mode) {
        if (mode == Mode.RESOLVE_NAME) {
            symbTable.newScope();
            whereExpr.decls.accept(this, Mode.INSERT_NAME);
            whereExpr.decls.accept(this, mode);
            whereExpr.subExpr.accept(this, mode);
            symbTable.oldScope();
        }
        return null;
    }

    private Report.Error errorAlreadyDefined(AST node, String name, String type) {
        String error = "Semantic error: " + type + " name '" + name + "' (" + node.location + ") already defined";
        try {
            AST defined = symbTable.fnd(name);
            return new Report.Error(error + " (previously defined at " + defined.location + ").");
        } catch (SymbTable.CannotFndNameException e) {
            return new Report.Error(error + ".");
        }
    }

    private Report.Error errorNotDefined(AST node, String name, String type) {
        return new Report.Error("Semantic error: " + type + " '" + name + "' (" + node.location + ") not defined.");
    }


}
