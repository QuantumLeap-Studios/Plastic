using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Plastic
{
    // === Tokenizer ===
    public enum TokenType
    {
        EOF, Identifier, Number, String,
        // Keywords
        Fn, Let, If, Else, While, For, Return, Interface,
        // Symbols
        Plus, Minus, Star, Slash, Percent,
        Equal, EqualEqual, Bang, BangEqual,
        Greater, GreaterEqual, Less, LessEqual,
        LParen, RParen, LBrace, RBrace, Comma, Semicolon, Arrow,
        Colon, True, False, In
    }

    public class Token
    {
        public TokenType Type { get; }
        public string Lexeme { get; }
        public object? Literal { get; }
        public int Line { get; }
        public Token(TokenType type, string lexeme, object? literal, int line)
        {
            Type = type; Lexeme = lexeme; Literal = literal; Line = line;
        }
        public override string ToString() => $"{Type} {Lexeme} {Literal}";
    }

    public class Lexer
    {
        private readonly string _src;
        private readonly List<Token> _tokens = new();
        private int _start = 0, _current = 0, _line = 1;
        private static readonly Dictionary<string, TokenType> _keywords = new() {
            {"fn", TokenType.Fn}, {"let", TokenType.Let}, {"if", TokenType.If}, {"else", TokenType.Else},
            {"while", TokenType.While}, {"for", TokenType.For}, {"return", TokenType.Return},
            {"interface", TokenType.Interface}, {"true", TokenType.True}, {"false", TokenType.False},
            {"in", TokenType.In} 
        };
        public Lexer(string src) { _src = src; }
        public List<Token> ScanTokens()
        {
            while (!IsAtEnd())
            {
                _start = _current;
                ScanToken();
            }
            _tokens.Add(new Token(TokenType.EOF, "", null, _line));
            return _tokens;
        }
        private void ScanToken()
        {
            char c = Advance();
            switch (c)
            {
                case '+': AddToken(TokenType.Plus); break;
                case '-': AddToken(Match('>') ? TokenType.Arrow : TokenType.Minus); break;
                case '*': AddToken(TokenType.Star); break;
                case '/':
                    if (Match('/'))
                    {
                        // Comment goes until end of line
                        while (Peek() != '\n' && !IsAtEnd()) Advance();
                    }
                    else
                    {
                        AddToken(TokenType.Slash);
                    }
                    break;
                case '%': AddToken(TokenType.Percent); break;
                case '(': AddToken(TokenType.LParen); break;
                case ')': AddToken(TokenType.RParen); break;
                case '{': AddToken(TokenType.LBrace); break;
                case '}': AddToken(TokenType.RBrace); break;
                case ',': AddToken(TokenType.Comma); break;
                case ';': AddToken(TokenType.Semicolon); break;
                case ':': AddToken(TokenType.Colon); break;
                case '!': AddToken(Match('=') ? TokenType.BangEqual : TokenType.Bang); break;
                case '=': AddToken(Match('=') ? TokenType.EqualEqual : TokenType.Equal); break;
                case '<': AddToken(Match('=') ? TokenType.LessEqual : TokenType.Less); break;
                case '>': AddToken(Match('=') ? TokenType.GreaterEqual : TokenType.Greater); break;
                case ' ':
                case '\r':
                case '\t': break;
                case '\n': _line++; break;
                case '"': String(); break;
                default:
                    if (IsDigit(c)) { Number(); }
                    else if (IsAlpha(c)) { Identifier(); }
                    else
                    {
                        throw new Exception($"Unexpected char: {c} at line {_line}");
                    }
                    break;
            }
        }
        private void Identifier()
        {
            while (IsAlphaNumeric(Peek())) Advance();
            var text = _src.Substring(_start, _current - _start);
            var type = _keywords.ContainsKey(text) ? _keywords[text] : TokenType.Identifier;
            AddToken(type);
        }
        private void Number()
        {
            while (IsDigit(Peek())) Advance();
            if (Peek() == '.' && IsDigit(PeekNext()))
            {
                Advance();
                while (IsDigit(Peek())) Advance();
            }
            var text = _src.Substring(_start, _current - _start);
            if (!double.TryParse(text, out var value))
                throw new Exception($"Invalid number literal: {text} at line {_line}");
            AddToken(TokenType.Number, value);
        }

        private void String()
        {
            while (Peek() != '"' && !IsAtEnd())
            {
                if (Peek() == '\n') _line++;
                Advance();
            }
            if (IsAtEnd()) throw new Exception("Unterminated string.");
            Advance();
            var str = _src.Substring(_start + 1, _current - _start - 2);
            AddToken(TokenType.String, str);
        }
        private bool Match(char expected)
        {
            if (IsAtEnd() || _src[_current] != expected) return false;
            _current++; return true;
        }
        private char Peek() => IsAtEnd() ? '\0' : _src[_current];
        private char PeekNext() => _current + 1 >= _src.Length ? '\0' : _src[_current + 1];
        private bool IsAlpha(char c) => char.IsLetter(c) || c == '_';
        private bool IsAlphaNumeric(char c) => IsAlpha(c) || IsDigit(c);
        private bool IsDigit(char c) => char.IsDigit(c);
        private char Advance() => _src[_current++];
        private bool IsAtEnd() => _current >= _src.Length;
        private void AddToken(TokenType type, object? literal = null)
        {
            var text = _src.Substring(_start, _current - _start);
            _tokens.Add(new Token(type, text, literal, _line));
        }
    }

    // === AST Nodes ===
    public abstract class AstNode { }
    public class ProgramNode : AstNode
    {
        public List<Stmt> Statements { get; } = new();
    }
    public abstract class Stmt : AstNode { }
    public abstract class Expr : AstNode { }

    public class FnDecl : Stmt
    {
        public required string Name;
        public required List<(string, string)> Params;
        public required string ReturnType;
        public required List<Stmt> Body;
    }
    public class VarDecl : Stmt
    {
        public required string Name;
        public required string Type;
        public required Expr Initializer;
    }
    public class IfStmt : Stmt
    {
        public required Expr Condition;
        public required List<Stmt> ThenBranch;
        public List<Stmt>? ElseBranch;
    }
    public class WhileStmt : Stmt
    {
        public required Expr Condition;
        public required List<Stmt> Body;
    }
    public class ForStmt : Stmt
    {
        public required string Var;
        public required Expr Start;
        public required Expr End;
        public required List<Stmt> Body;
    }
    public class ReturnStmt : Stmt
    {
        public Expr? Value;
    }
    public class ExprStmt : Stmt
    {
        public required Expr Expression;
    }
    public class BlockStmt : Stmt
    {
        public required List<Stmt> Statements;
    }

    public class BinaryExpr : Expr
    {
        public required Expr Left;
        public required Token Op;
        public required Expr Right;
    }
    public class UnaryExpr : Expr
    {
        public required Token Op;
        public required Expr Right;
    }
    public class LiteralExpr : Expr
    {
        public required object Value;
    }
    public class GroupingExpr : Expr
    {
        public required Expr Expression;
    }
    public class VariableExpr : Expr
    {
        public required string Name;
    }
    public class CallExpr : Expr
    {
        public required Expr Callee;
        public required List<Expr> Arguments;
    }

    // === Parser ===
    public class Parser
    {
        private readonly List<Token> _tokens; private int _current = 0;
        public Parser(List<Token> tokens) { _tokens = tokens; }
        public ProgramNode Parse()
        {
            var program = new ProgramNode();
            while (!IsAtEnd()) program.Statements.Add(ParseDeclaration());
            return program;
        }
        private Stmt ParseDeclaration()
        {
            if (Match(TokenType.Fn)) return ParseFunction("function");
            if (Match(TokenType.Let)) return ParseVarDecl();
            return ParseStatement();
        }
        private FnDecl ParseFunction(string kind)
        {
            var fn = new FnDecl
            {
                Name = Consume(TokenType.Identifier, $"Expect {kind} name.").Lexeme,
                Params = new(),
                ReturnType = string.Empty, // Initialize ReturnType to avoid CS9035  
                Body = new()
            };

            Consume(TokenType.LParen, "Expect '('.");

            if (!Check(TokenType.RParen))
            {
                do
                {
                    var name = Consume(TokenType.Identifier, "Expect param name.").Lexeme;
                    Consume(TokenType.Colon, "Expect ':'.");
                    var type = Consume(TokenType.Identifier, "Expect type name.").Lexeme;
                    fn.Params.Add((name, type));
                } while (Match(TokenType.Comma));
            }
            Consume(TokenType.RParen, "Expect ')'.");
            Consume(TokenType.Arrow, "Expect '->'.");
            fn.ReturnType = Consume(TokenType.Identifier, "Expect return type.").Lexeme;
            Consume(TokenType.LBrace, "Expect '{' before body.");
            fn.Body = ParseBlock();
            return fn;
        }
        private List<Stmt> ParseBlock()
        {
            var stmts = new List<Stmt>();
            while (!Check(TokenType.RBrace) && !IsAtEnd()) stmts.Add(ParseStatement());
            Consume(TokenType.RBrace, "Expect '}' after block.");
            return stmts;
        }
        private Stmt ParseVarDecl()
        {
            var name = Consume(TokenType.Identifier, "Expect variable name.").Lexeme;
            Consume(TokenType.Colon, "Expect ':'.");
            var type = Consume(TokenType.Identifier, "Expect type.").Lexeme;
            Consume(TokenType.Equal, "Expect '='.");
            var init = ParseExpression();
            Consume(TokenType.Semicolon, "Expect ';' after var declaration.");
            return new VarDecl { Name = name, Type = type, Initializer = init };
        }
        private Stmt ParseStatement()
        {
            if (Match(TokenType.If)) return ParseIf();
            if (Match(TokenType.While)) return ParseWhile();
            if (Match(TokenType.For)) return ParseFor();
            if (Match(TokenType.Return)) return ParseReturn();
            if (Match(TokenType.LBrace)) return new BlockStmt { Statements = ParseBlock() };
            return ParseExprStmt();
        }
        private IfStmt ParseIf()
        {
            Consume(TokenType.LParen, "Expect '('.");
            var cond = ParseExpression();
            Consume(TokenType.RParen, "Expect ')'.");

            List<Stmt> thenBranch;
            if (Match(TokenType.LBrace))
            {
                thenBranch = ParseBlock();
            }
            else
            {
                thenBranch = new List<Stmt> { ParseStatement() };
            }

            List<Stmt>? elseBranch = null;
            if (Match(TokenType.Else))
            {
                if (Match(TokenType.LBrace))
                {
                    elseBranch = ParseBlock();
                }
                else
                {
                    elseBranch = new List<Stmt> { ParseStatement() };
                }
            }

            return new IfStmt { Condition = cond, ThenBranch = thenBranch, ElseBranch = elseBranch };
        }
        private WhileStmt ParseWhile()
        {
            Consume(TokenType.LParen, "Expect '('.");
            var cond = ParseExpression();
            Consume(TokenType.RParen, "Expect ')'.");

            List<Stmt> body;
            if (Match(TokenType.LBrace))
            {
                body = ParseBlock();
            }
            else
            {
                body = new List<Stmt> { ParseStatement() };
            }

            return new WhileStmt { Condition = cond, Body = body };
        }
        private ForStmt ParseFor()
        {
            Consume(TokenType.LParen, "Expect '(' after 'for'.");
            var varName = Consume(TokenType.Identifier, "Expect variable name.").Lexeme;
            Consume(TokenType.In, "Expect 'in' after variable name.");
            var start = ParseExpression();
            Consume(TokenType.Comma, "Expect ',' after start expression.");
            var end = ParseExpression();
            Consume(TokenType.RParen, "Expect ')' after clauses.");

            var body = new List<Stmt>();
            if (Match(TokenType.LBrace))
            {
                body = ParseBlock();
            }
            else
            {
                body.Add(ParseStatement());
            }

            return new ForStmt { Var = varName, Start = start, End = end, Body = body };
        }
        private ReturnStmt ParseReturn()
        {
            var value = !Check(TokenType.Semicolon) ? ParseExpression() : null;
            Consume(TokenType.Semicolon, "Expect ';' after return.");
            return new ReturnStmt { Value = value };
        }
        private ExprStmt ParseExprStmt()
        {
            var expr = ParseExpression();
            Consume(TokenType.Semicolon, "Expect ';' after expression.");
            return new ExprStmt { Expression = expr };
        }
        private Expr ParseExpression() => ParseAssignment();

        private Expr ParseAssignment()
        {
            var expr = ParseEquality();

            if (Match(TokenType.Equal))
            {
                var value = ParseAssignment();

                if (expr is VariableExpr ve)
                {
                    var op = Previous();
                    Console.WriteLine($"DEBUG: Assignment operator token: {op.Type} {op.Lexeme}");
                    return new BinaryExpr
                    {
                        Left = expr,
                        Op = op,
                        Right = value
                    };
                }

                throw new Exception("Invalid assignment target.");
            }

            return expr;
        }

        private Expr ParseEquality()
        {
            var expr = ParseComparison();
            while (Match(TokenType.BangEqual, TokenType.EqualEqual))
            {
                var op = Previous(); var right = ParseComparison();
                expr = new BinaryExpr { Left = expr, Op = op, Right = right };
            }
            return expr;
        }
        private Expr ParseComparison()
        {
            var expr = ParseTerm();
            while (Match(TokenType.Greater, TokenType.GreaterEqual, TokenType.Less, TokenType.LessEqual))
            {
                var op = Previous(); var right = ParseTerm();
                expr = new BinaryExpr { Left = expr, Op = op, Right = right };
            }
            return expr;
        }
        private Expr ParseTerm()
        {
            var expr = ParseFactor();
            while (Match(TokenType.Plus, TokenType.Minus))
            {
                var op = Previous(); var right = ParseFactor();
                expr = new BinaryExpr { Left = expr, Op = op, Right = right };
            }
            return expr;
        }
        private Expr ParseFactor()
        {
            var expr = ParseUnary();
            while (Match(TokenType.Star, TokenType.Slash, TokenType.Percent))
            {
                var op = Previous(); var right = ParseUnary();
                expr = new BinaryExpr { Left = expr, Op = op, Right = right };
            }
            return expr;
        }
        private Expr ParseUnary()
        {
            if (Match(TokenType.Bang, TokenType.Minus))
            {
                var op = Previous(); var right = ParseUnary();
                return new UnaryExpr { Op = op, Right = right };
            }
            return ParseCall();
        }
        private Expr ParseCall()
        {
            var expr = ParsePrimary();
            while (true)
            {
                if (Match(TokenType.LParen))
                {
                    expr = FinishCall(expr);
                }
                else break;
            }
            return expr;
        }
        private Expr FinishCall(Expr callee)
        {
            var args = new List<Expr>();
            if (!Check(TokenType.RParen))
            {
                do { args.Add(ParseExpression()); } while (Match(TokenType.Comma));
            }
            var paren = Consume(TokenType.RParen, "Expect ')' after arguments.");
            return new CallExpr { Callee = callee, Arguments = args };
        }
        private Expr ParsePrimary()
        {
            if (Match(TokenType.False)) return new LiteralExpr { Value = false };
            if (Match(TokenType.True)) return new LiteralExpr { Value = true };
            if (Match(TokenType.Number)) return new LiteralExpr { Value = Previous().Literal! };
            if (Match(TokenType.String)) return new LiteralExpr { Value = Previous().Literal! };
            if (Match(TokenType.Identifier)) return new VariableExpr { Name = Previous().Lexeme };
            if (Match(TokenType.LParen))
            {
                var expr = ParseExpression();
                Consume(TokenType.RParen, "Expect ')'.");
                return new GroupingExpr { Expression = expr };
            }
            throw new Exception("Expect expression.");
        }
        // Utility parsing methods
        private bool Match(params TokenType[] types)
        {
            foreach (var t in types) if (Check(t)) { Advance(); return true; }
            return false;
        }
        private Token Consume(TokenType type, string message)
        {
            if (Check(type)) return Advance();
            throw new Exception(message);
        }
        private bool Check(TokenType type) => !IsAtEnd() && Peek().Type == type;
        private Token Advance() { if (!IsAtEnd()) _current++; return Previous(); }
        private bool IsAtEnd() => Peek().Type == TokenType.EOF;
        private Token Peek() => _tokens[_current];
        private Token Previous() => _tokens[_current - 1];
    }

    // === TypeChecker ===
    public class TypeChecker
    {
        private Dictionary<string, string> _variables = new();
        private Dictionary<string, (List<(string, string)> parameters, string returnType)> _functions = new();

        public void Check(ProgramNode program)
        {
            // First pass: collect all function signatures
            foreach (var stmt in program.Statements)
            {
                if (stmt is FnDecl fn)
                {
                    _functions[fn.Name] = (fn.Params, fn.ReturnType);
                }
            }

            // Second pass: check all statements and expressions
            foreach (var stmt in program.Statements)
            {
                CheckStatement(stmt);
            }
        }

        private void CheckStatement(Stmt stmt)
        {
            switch (stmt)
            {
                case FnDecl fn:
                    CheckFunction(fn);
                    break;
                case VarDecl vd:
                    var exprType = GetExpressionType(vd.Initializer);
                    if (exprType != vd.Type)
                    {
                        // Allow numeric coercion between i32/f64/etc
                        if (!IsNumericType(vd.Type) || !IsNumericType(exprType))
                        {
                            throw new Exception($"Type mismatch in variable declaration: expected {vd.Type}, got {exprType}");
                        }
                    }
                    _variables[vd.Name] = vd.Type;
                    break;
                case IfStmt ifStmt:
                    var condType = GetExpressionType(ifStmt.Condition);
                    if (condType != "bool")
                    {
                        throw new Exception("Condition must be a boolean expression");
                    }
                    foreach (var s in ifStmt.ThenBranch) CheckStatement(s);
                    if (ifStmt.ElseBranch != null)
                    {
                        foreach (var s in ifStmt.ElseBranch) CheckStatement(s);
                    }
                    break;
                case WhileStmt ws:
                    var whileCondType = GetExpressionType(ws.Condition);
                    if (whileCondType != "bool")
                    {
                        throw new Exception("Condition must be a boolean expression");
                    }
                    foreach (var s in ws.Body) CheckStatement(s);
                    break;
                case ForStmt fs:
                    var startType = GetExpressionType(fs.Start);
                    var endType = GetExpressionType(fs.End);
                    if (!IsNumericType(startType) || !IsNumericType(endType))
                    {
                        throw new Exception("For loop bounds must be numeric");
                    }
                    // Set the range variable type temporarily
                    _variables[fs.Var] = "i32";
                    foreach (var s in fs.Body) CheckStatement(s);
                    _variables.Remove(fs.Var);
                    break;
                case ReturnStmt rs:
                    // In a full implementation, we would track the current function context
                    break;
                case ExprStmt es:
                    GetExpressionType(es.Expression);
                    break;
                case BlockStmt bs:
                    // Create a new scope for the block
                    var outerVars = new Dictionary<string, string>(_variables);
                    foreach (var s in bs.Statements)
                    {
                        CheckStatement(s);
                    }
                    // Restore outer variables scope
                    _variables = outerVars;
                    break;
            }
        }

        private void CheckFunction(FnDecl fn)
        {
            // Save outer scope
            var outerVars = new Dictionary<string, string>(_variables);

            // Add parameters to local scope
            foreach (var (name, type) in fn.Params)
            {
                _variables[name] = type;
            }

            // Check function body
            foreach (var stmt in fn.Body)
            {
                CheckStatement(stmt);
            }

            // Restore outer scope
            _variables = outerVars;
        }

        private string GetExpressionType(Expr expr)
        {
            switch (expr)
            {
                case LiteralExpr lit:
                    return lit.Value switch
                    {
                        double => "f64",
                        bool => "bool",
                        string => "string",
                        _ => "any"
                    };

                case VariableExpr ve:
                    if (_variables.TryGetValue(ve.Name, out var type))
                    {
                        return type;
                    }
                    throw new Exception($"Undefined variable: {ve.Name}");

                case BinaryExpr be:
                    var leftType = GetExpressionType(be.Left);
                    var rightType = GetExpressionType(be.Right);

                    switch (be.Op.Type)
                    {
                        case TokenType.Plus:
                        case TokenType.Minus:
                        case TokenType.Star:
                        case TokenType.Slash:
                        case TokenType.Percent:
                            if (IsNumericType(leftType) && IsNumericType(rightType))
                            {
                                return leftType; // Simplification - return the left type
                            }
                            throw new Exception($"Cannot perform arithmetic on types {leftType} and {rightType}");

                        case TokenType.EqualEqual:
                        case TokenType.BangEqual:
                        case TokenType.Greater:
                        case TokenType.GreaterEqual:
                        case TokenType.Less:
                        case TokenType.LessEqual:
                            return "bool";

                        case TokenType.Equal:
                            // Assignment - check that types are compatible
                            if (leftType != rightType && !(IsNumericType(leftType) && IsNumericType(rightType)))
                            {
                                throw new Exception($"Cannot assign {rightType} to {leftType}");
                            }
                            return leftType;

                        default:
                            throw new Exception($"Unsupported binary operator: {be.Op.Lexeme}");
                    }

                case UnaryExpr ue:
                    rightType = GetExpressionType(ue.Right);

                    switch (ue.Op.Type)
                    {
                        case TokenType.Minus:
                            if (IsNumericType(rightType))
                            {
                                return rightType;
                            }
                            throw new Exception($"Cannot negate non-numeric type: {rightType}");

                        case TokenType.Bang:
                            if (rightType == "bool")
                            {
                                return "bool";
                            }
                            throw new Exception($"Cannot logically negate non-boolean type: {rightType}");

                        default:
                            throw new Exception($"Unsupported unary operator: {ue.Op.Lexeme}");
                    }

                case CallExpr ce:
                    if (ce.Callee is VariableExpr callee)
                    {
                        if (_functions.TryGetValue(callee.Name, out var fnInfo))
                        {
                            // Check argument count
                            if (ce.Arguments.Count != fnInfo.parameters.Count)
                            {
                                throw new Exception($"Function {callee.Name} expects {fnInfo.parameters.Count} arguments but got {ce.Arguments.Count}");
                            }

                            // Check argument types
                            for (int i = 0; i < ce.Arguments.Count; i++)
                            {
                                var argType = GetExpressionType(ce.Arguments[i]);
                                var paramType = fnInfo.parameters[i].Item2;

                                if (argType != paramType && !(IsNumericType(argType) && IsNumericType(paramType)))
                                {
                                    throw new Exception($"Argument {i + 1} type mismatch: expected {paramType}, got {argType}");
                                }
                            }

                            return fnInfo.returnType;
                        }

                        // Check for built-in functions
                        if (callee.Name == "print") return "void";
                        if (callee.Name == "len") return "i32";
                        if (callee.Name == "range") return "range";

                        throw new Exception($"Undefined function: {callee.Name}");
                    }
                    throw new Exception("Call expression must have a variable as callee");

                case GroupingExpr ge:
                    return GetExpressionType(ge.Expression);

                default:
                    throw new Exception($"Unsupported expression type: {expr.GetType().Name}");
            }
        }

        private bool IsNumericType(string type)
        {
            return type == "i32" || type == "f64" || type == "i64" || type == "f32";
        }
    }

    // === Environment ===
    public class Environment
    {
        private readonly Dictionary<string, object?> _values = new();
        private readonly Environment? _enclosing;

        public Environment() { _enclosing = null; }
        public Environment(Environment enclosing) { _enclosing = enclosing; }

        public void Define(string name, object? value)
        {
            _values[name] = value;
        }

        public object? Get(string name)
        {
            if (_values.TryGetValue(name, out var value))
            {
                return value;
            }

            if (_enclosing != null)
            {
                return _enclosing.Get(name);
            }

            throw new Exception($"Undefined variable '{name}'");
        }

        public void Assign(string name, object? value)
        {
            if (_values.ContainsKey(name))
            {
                _values[name] = value;
                return;
            }

            if (_enclosing != null)
            {
                _enclosing.Assign(name, value);
                return;
            }

            throw new Exception($"Undefined variable '{name}'");
        }
    }

    // === Interpreter ===
    public class Interpreter
    {
        private readonly Environment _globals = new();
        private Environment _environment;

        public Interpreter(Dictionary<string, object> globals)
        {
            _environment = _globals;

            // Add globals to environment
            foreach (var (name, value) in globals)
            {
                _globals.Define(name, value);
            }
        }

        public object? Execute(ProgramNode program)
        {
            try
            {
                object? result = null;
                foreach (var stmt in program.Statements)
                {
                    result = ExecuteStmt(stmt);
                }

                // If there's a main function, call it
                try
                {
                    var mainFn = _globals.Get("main");
                    if (mainFn is FnDecl)
                    {
                        result = CallFunction(mainFn as FnDecl, new List<object?>());
                    }
                }
                catch (Exception)
                {
                    // No main function, continue with result
                }

                return result;
            }
            catch (ReturnException re)
            {
                return re.Value;
            }
        }

        private object? ExecuteStmt(Stmt stmt)
        {
            switch (stmt)
            {
                case ExprStmt es:
                    return Evaluate(es.Expression);

                case VarDecl vd:
                    var value = Evaluate(vd.Initializer);
                    _environment.Define(vd.Name, value);
                    return null;

                case FnDecl fn:
                    _environment.Define(fn.Name, fn);
                    return null;

                case ReturnStmt rs:
                    object? val = null;
                    if (rs.Value != null)
                    {
                        val = Evaluate(rs.Value);
                    }
                    throw new ReturnException(val);

                case IfStmt ifStmt:
                    if (IsTruthy(Evaluate(ifStmt.Condition)))
                    {
                        return ExecuteBlock(ifStmt.ThenBranch, _environment);
                    }
                    else if (ifStmt.ElseBranch != null)
                    {
                        return ExecuteBlock(ifStmt.ElseBranch, _environment);
                    }
                    return null;

                case WhileStmt ws:
                    while (IsTruthy(Evaluate(ws.Condition)))
                    {
                        ExecuteBlock(ws.Body, _environment);
                    }
                    return null;

                case ForStmt fs:
                    var start = Convert.ToInt32(Evaluate(fs.Start));
                    var end = Convert.ToInt32(Evaluate(fs.End));

                    // Create a new environment for the loop
                    var previous = _environment;
                    try
                    {
                        _environment = new Environment(_environment);

                        for (int i = start; i <= end; i++)
                        {
                            _environment.Define(fs.Var, i);
                            ExecuteBlock(fs.Body, _environment);
                        }
                    }
                    finally
                    {
                        _environment = previous;
                    }
                    return null;

                case BlockStmt bs:
                    return ExecuteBlock(bs.Statements, new Environment(_environment));

                default:
                    throw new Exception($"Unsupported statement type: {stmt.GetType().Name}");
            }
        }

        private object? ExecuteBlock(List<Stmt> statements, Environment environment)
        {
            var previous = _environment;
            try
            {
                _environment = environment;

                object? last = null;
                foreach (var statement in statements)
                {
                    last = ExecuteStmt(statement);
                }
                return last;
            }
            catch (ReturnException re)
            {
                throw;
            }
            finally
            {
                _environment = previous;
            }
        }

        private object? Evaluate(Expr expr)
        {
            switch (expr)
            {
                case LiteralExpr lit:
                    return lit.Value;

                case VariableExpr ve:
                    return _environment.Get(ve.Name);

                case BinaryExpr be:
                    var left = Evaluate(be.Left);
                    var right = Evaluate(be.Right);

                    if (be.Op.Type == TokenType.Equal && be.Left is VariableExpr variable)
                    {
                        _environment.Assign(variable.Name, right);
                        return right;
                    }

                    return ApplyBinary(be.Op, left, right);

                case UnaryExpr ue:
                    var operand = Evaluate(ue.Right);

                    switch (ue.Op.Type)
                    {
                        case TokenType.Minus:
                            CheckNumberOperand(ue.Op, operand);
                            return -(double)operand;

                        case TokenType.Bang:
                            return !IsTruthy(operand);

                        default:
                            throw new Exception($"Unsupported unary operator: {ue.Op.Lexeme}");
                    }

                case CallExpr ce:
                    var callee = Evaluate(ce.Callee);

                    var arguments = new List<object?>();
                    foreach (var argument in ce.Arguments)
                    {
                        arguments.Add(Evaluate(argument));
                    }

                    if (callee is Delegate del)
                    {
                        return del.DynamicInvoke(arguments.ToArray());
                    }
                    else if (callee is FnDecl fn)
                    {
                        return CallFunction(fn, arguments);
                    }

                    throw new Exception("Can only call functions and classes.");

                case GroupingExpr ge:
                    return Evaluate(ge.Expression);

                default:
                    throw new Exception($"Unsupported expression type: {expr.GetType().Name}");
            }
        }
        private object? ApplyBinary(Token op, object? left, object? right)
        {
            return op.Type switch
            {
                TokenType.Plus => (left, right) switch
                {
                    (double l, double r) => l + r,
                    (string l, string r) => l + r,
                    (string l, _) => l + right?.ToString(),
                    (_, string r) => left?.ToString() + r,
                    _ => throw new Exception("Operands must be two numbers or two strings.")
                },
                TokenType.Minus => (double)left - (double)right,
                TokenType.Star => (double)left * (double)right,
                TokenType.Slash => (double)right == 0
                    ? throw new Exception("Division by zero.")
                    : (double)left / (double)right,
                TokenType.Percent => (double)right == 0
                    ? throw new Exception("Modulo by zero.")
                    : (double)left % (double)right,
                TokenType.Greater => (double)left > (double)right,
                TokenType.GreaterEqual => (double)left >= (double)right,
                TokenType.Less => (double)left < (double)right,
                TokenType.LessEqual => (double)left <= (double)right,
                TokenType.EqualEqual => IsEqual(left, right),
                TokenType.BangEqual => !IsEqual(left, right),
                _ => throw new Exception($"Unsupported binary operator: {op.Lexeme}")
            };
        }

        private object? CallFunction(FnDecl function, List<object?> arguments)
        {
            var environment = new Environment(_globals);

            // Bind parameters to arguments
            for (int i = 0; i < function.Params.Count; i++)
            {
                environment.Define(function.Params[i].Item1, arguments[i]);
            }

            try
            {
                ExecuteBlock(function.Body, environment);
            }
            catch (ReturnException re)
            {
                return re.Value;
            }

            return null;
        }

        private bool IsTruthy(object? obj)
        {
            if (obj == null) return false;
            if (obj is bool b) return b;
            if (obj is double d) return d != 0;
            return true;
        }

        private bool IsEqual(object? a, object? b)
        {
            if (a == null && b == null) return true;
            if (a == null) return false;
            return a.Equals(b);
        }

        private void CheckNumberOperand(Token op, object? operand)
        {
            if (operand is double) return;
            throw new Exception($"Operand must be a number for operator '{op.Lexeme}'.");
        }

        private void CheckNumberOperands(Token op, object? left, object? right)
        {
            if (left is double && right is double) return;
            throw new Exception($"Operands must be numbers for operator '{op.Lexeme}'.");
        }
    }

    public class ReturnException : Exception
    {
        public object? Value { get; }
        public ReturnException(object? value) { Value = value; }
    }

    // === Engine & Builtins ===
    public class PlasticEngine
    {
        private readonly Dictionary<string, object> _globals = new();

        public PlasticEngine()
        {
            RegisterBuiltins();
        }

        public void RegisterGlobal(string name, object value)
        {
            ArgumentNullException.ThrowIfNull(name);
            ArgumentNullException.ThrowIfNull(value);
            _globals[name] = value;
        }

        private void RegisterBuiltins()
        {
            _globals["print"] = new Action<object>(o => Console.WriteLine(o));
            _globals["len"] = new Func<string, int>(s => s.Length);
            _globals["range"] = new Func<double, double, object>((start, end) => new { Start = start, End = end });
            _globals["input"] = new Func<string?>(() => Console.ReadLine());
            _globals["parseInt"] = new Func<string, double>(s => {
                if (double.TryParse(s, out var result)) return result;
                throw new Exception($"Cannot parse '{s}' as number.");
            });
            _globals["toString"] = new Func<object, string>(o => o.ToString());
        }

        public object? Evaluate(string source)
        {
            try
            {
                var lexer = new Lexer(source);
                var tokens = lexer.ScanTokens();

                var parser = new Parser(tokens);
                var program = parser.Parse();

                var checker = new TypeChecker();
                checker.Check(program);

                var interpreter = new Interpreter(_globals);
                return interpreter.Execute(program);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
                return null;
            }
        }
    }

    // === Sample Usage ===
    public static class PlasticDemo
    {
        public static void Main()
        {
            var engine = new PlasticEngine();
            var source = @"
print(""Hello, World!"");
return 0;
            ";

            var result = engine.Evaluate(source);
            if (result != null && result.ToString().Length != 0)
            {
                Console.WriteLine($"Result: {result}");
            }
            else
            {
                Console.WriteLine("No result.");
            }
        }
    }

    /// <summary>
    /// Contains features such as compiling, interpreting, and executing Plastic code.
    /// </summary>
    public static class EngineFeatures
    {
        /// <summary>
        /// Interprets and executes the Plastic code.
        /// </summary>
        public static object? Interpret(string source, bool debugReturn = false)
        {
            var engine = new PlasticEngine();
            if (!debugReturn)
            {
                return engine.Evaluate(source);
            }
            else
            {
                var result = engine.Evaluate(source);
                if (result != null && result.ToString().Length != 0)
                {
                    Console.WriteLine($"Result: {result}");
                }
                else
                {
                    Console.WriteLine("No result.");
                }
                return result;
            }
        }
    }
}
