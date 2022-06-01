package pins.phase.lexan;

import java.io.*;

import pins.common.report.*;
import pins.data.symbol.*;

/**
 * Lexical analyzer.
 */
public class LexAn implements AutoCloseable {

    private final String         srcFileName;
    private final BufferedReader reader;

    private int line;
    private int column;

    public LexAn(String srcFileName) {
        this.srcFileName = srcFileName;
        this.line = 1;
        this.column = 1;
        try {
            reader = new BufferedReader(new FileReader(srcFileName));
        } catch (FileNotFoundException __) {
            throw new Report.Error("Cannot open source file '" + srcFileName + "'.");
        }
    }

    public void close() {
        try {
            reader.close();
        } catch (IOException __) {
            throw new Report.Error("Cannot close source file '" + srcFileName + "'.");
        }
    }

    public Symbol lexer() {
        try {
            skipInsignificant();

            int c = advance();
            if (c == -1) return new Symbol(Token.EOF, "EOF", null);

            Location oneTokenLoc = new Location(line, column - 1, line, column);
            Location twoTokenLoc    = new Location(line, column - 1, line, column + 1);

            switch (c) {
                case '(': return new Symbol(Token.LEFT_PAREN, "(", oneTokenLoc);
                case ')': return new Symbol(Token.RIGHT_PAREN, ")", oneTokenLoc);
                case '{': return new Symbol(Token.LEFT_BRACE, "{", oneTokenLoc);
                case '}': return new Symbol(Token.RIGHT_BRACE, "}", oneTokenLoc);
                case '[': return new Symbol(Token.LEFT_BRACKET, "[", oneTokenLoc);
                case ']': return new Symbol(Token.RIGHT_BRACKET, "]", oneTokenLoc);
                case ',': return new Symbol(Token.COMMA, ",", oneTokenLoc);
                case ':': return new Symbol(Token.COLON, ":", oneTokenLoc);
                case ';': return new Symbol(Token.SEMICOLON, ";", oneTokenLoc);
                case '&': return new Symbol(Token.AND, "&", oneTokenLoc);
                case '|': return new Symbol(Token.OR, "|", oneTokenLoc);
                case '*': return new Symbol(Token.STAR, "*", oneTokenLoc);
                case '/': return new Symbol(Token.SLASH, "/", oneTokenLoc);
                case '%': return new Symbol(Token.MODULO, "%", oneTokenLoc);
                case '+': return new Symbol(Token.PLUS, "+", oneTokenLoc);
                case '-': return new Symbol(Token.MINUS, "-", oneTokenLoc);
                case '^': return new Symbol(Token.XOR, "^", oneTokenLoc);
                case '!':
                    if (peek() == '=') {
                        advance();
                        return new Symbol(Token.NOT_EQUAL, "!=", twoTokenLoc);
                    } else return new Symbol(Token.NOT, "!", oneTokenLoc);
                case '=':
                    if (peek() == '=') {
                        advance();
                        return new Symbol(Token.EQUAL_EQUAL, "==", twoTokenLoc);
                    } else return new Symbol(Token.EQUAL, "=", oneTokenLoc);
                case '<':
                    if (peek() == '=') {
                        advance();
                        return new Symbol(Token.LESS_EQUAL, "<=", twoTokenLoc);
                    } else return new Symbol(Token.LESS, "<", oneTokenLoc);
                case '>':
                    if (peek() == '=') {
                        advance();
                        return new Symbol(Token.GREATER_EQUAL, ">=", twoTokenLoc);
                    } else return new Symbol(Token.GREAT, ">", oneTokenLoc);
                default: {
                    if (isAlpha(c)) return name(c);
                    else if (isDigit(c)) return numberConst(c);
                    else if (c == '\'') return charConst();
                    else throw new Report.Error(oneTokenLoc, String.format("Unrecognised symbol: '%c'", (char) c));
                }
            }


        } catch (IOException __) {
            throw new Report.Error("An error occurred while reading file.");
        }
    }

    private Symbol name(int start) throws IOException{
        StringBuilder sb = new StringBuilder();
        int startColumn = column - 1;
        sb.append((char) start);

        while (isAlphaNum(peek())) {
            sb.append((char) advance());
        }

        String name = sb.toString();

        Location location = new Location(line, startColumn, line, column);

        // Todo: Use trie
        switch (name) {
            case "char":  return new Symbol(Token.CHAR, name, location);
            case "del":   return new Symbol(Token.DEL, name, location);
            case "do":    return new Symbol(Token.DO, name, location);
            case "else":  return new Symbol(Token.ELSE, name, location);
            case "end":   return new Symbol(Token.END, name, location);
            case "fun":   return new Symbol(Token.FUN, name, location);
            case "if":    return new Symbol(Token.IF, name, location);
            case "int":   return new Symbol(Token.INT, name, location);
            case "new":   return new Symbol(Token.NEW, name, location);
            case "then":  return new Symbol(Token.THEN, name, location);
            case "typ":   return new Symbol(Token.TYP, name, location);
            case "var":   return new Symbol(Token.VAR, name, location);
            case "void":  return new Symbol(Token.VOID, name, location);
            case "where": return new Symbol(Token.WHERE, name, location);
            case "while": return new Symbol(Token.WHILE, name, location);
            // constants
            case "none":  return new Symbol(Token.CONST_VOID, name, location);
            case "nil":   return new Symbol(Token.CONST_PTR, name, location);
            // Default, identifier
            default:      return new Symbol(Token.IDENTIFIER, name, location);
        }


    }

    private Symbol numberConst(int start) throws IOException {
        StringBuilder sb = new StringBuilder();
        int startColumn = column - 1;
        sb.append((char) start);

        while (isDigit(peek())) {
            sb.append((char) advance());
        }
        // if (isAlpha(current()))
        //     throw new Report.Error(new Location(line, startColumn),
        //             "Illegal identifier name (can not start with a digit).");
        return new Symbol(Token.CONST_INT, sb.toString(), new Location(line, startColumn, line, column));
    }

    private Symbol charConst() throws IOException {
        int startColumn = column - 1; // Starting ' is already consumed

        int maxLen = peek() == '\\' ? 2 : 1; // Have we escaped?
        char []charSequence = new char[maxLen];

        for (int i = 0; i < maxLen; ++i) {
            int val = advance();
            if (val < 32 || val > 126) {
                throw new Report.Error(new Location(line, column),
                        String.format("Illegal ASCII value (%d).", val));
            }
            charSequence[i] = (char)val;
        }

        if (charSequence[0] == '\\' &&
            !(charSequence[1] == '\\' || charSequence[1] == '\''))
            throw new Report.Error(new Location(line, startColumn + 1),
                    String.format("Illegal character escape sequence: '\\%c'.", charSequence[1]));
        if (charSequence[0] == '\'')
            throw new Report.Error(new Location(line, startColumn, line, column),
                    "Empty character constant.");

        if (peek() != '\'') throw new Report.Error(new Location(line, startColumn, line, column),
                "Unterminated character constant.");
        // Consume closing '
        advance();

        return new Symbol(Token.CONST_CHAR, "'" + String.valueOf(charSequence) + "'",
                new Location(line, startColumn, line, column));
    }

    private boolean isAlpha(int c) throws IOException {
        return ('a' <= c && c <= 'z') ||
                ('A' <= c && c <= 'Z') || c == '_';
    }

    private boolean isAlphaNum(int c) throws IOException {
        return isAlpha(c) || isDigit(c);
    }

    private boolean isDigit(int c) throws IOException {
        return c >= '0' && c <= '9';
    }

    /**
     * Skips unimportant characters (whitespaces and comments).
     */
    private void skipInsignificant() throws IOException {
        while (true) {
            switch (peek()) {
                case '\t': // HR
                    // Align column to nearest multiple of 8.
                    if (column % 8 != 0) column += 8 - column % 8;
                    advance();
                    break;
                case ' ':  // Space
                case '\n': // LF
                case '\r': // CR
                    advance();
                    break;
                case '#':
                    if (peekNext() == '{') {
                        int nestCount = 1;
                        // Consume #
                        advance();

                        while (nestCount > 0) {
                            advance();
                            if (peek() == '#' && peekNext() == '{') nestCount++;
                            else if (peek() == '}' && peekNext() == '#') nestCount--;
                            else if (peek() == -1) {
                                Report.warning(String.format("Unterminated comment at the end of the file (nest depth: %d)", nestCount));
                                break;
                            }
                        }
                        // Consume closing }#
                        advance(); advance();
                        break;
                    }
                default: return;
            }
        }
    }

    private int advance() throws IOException {
        column++;
        int read = reader.read();
        if (read == '\n' || // Unix
            read == '\r') { // Apple
            int curr = peek();
            if (read == '\r' && curr == '\n') // Windows
                reader.read();
            line++;
            column = 1;
        }
        return read;
    }

    private int peek() throws IOException {
        reader.mark(1);
        int read = reader.read();
        reader.reset();
        return read;
    }

    private int peekNext() throws IOException {
        reader.mark(2);
        reader.read();
        int read = reader.read();
        reader.reset();
        return read;
    }

}
