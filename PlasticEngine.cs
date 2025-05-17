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
        Colon, True, False, In,
        Struct, Enum, Trait, Impl, Mut, Pub, Self, Match, As, Dot,
        AtSymbol, Import, Reference, Using
    }
    public class ImportStmt : Stmt
    {
        public required string PackageName;
        public required ImportType Type;

        public enum ImportType
        {
            Import,   // @import - Import all symbols from a package
            Reference, // @reference - Reference another package
            Using      // @using - Import specific symbols from a package
        }
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
            {"in", TokenType.In},
            {"trait", TokenType.Trait}, {"impl", TokenType.Impl}, {"mut", TokenType.Mut},
            {"pub", TokenType.Pub}, {"self", TokenType.Self}, {"match", TokenType.Match},
            {"as", TokenType.As},
            {"import", TokenType.Import}, {"reference", TokenType.Reference}, {"using", TokenType.Using}
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
        private void ScanDirective()
        {
            while (char.IsWhiteSpace(Peek()) && !IsAtEnd()) Advance();

            _start = _current;

            while (IsAlpha(Peek())) Advance();

            var directive = _src.Substring(_start, _current - _start);

            if (directive == "import" || directive == "reference" || directive == "using")
            {
                // We manually added the token earlier, so we don't add it here
                // This is just to consume the directive name
            }
            else
            {
                throw new Exception($"Unknown directive '@{directive}' at line {_line}");
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
        public bool IsPublic { get; set; } = false;  // Add this property
        public bool alwaysTrue = true; // For testing purposes
    }
    // Update VarDecl class (around line 164)
    public class VarDecl : Stmt
    {
        public required string Name;
        public required string Type;
        public required Expr Initializer;
        public bool IsMutable { get; set; } = false;
        public bool IsPublic { get; set; } = false;
        public bool alwaysTrue { get; set; } = true; // New property for reference
    }

    // Add after BlockStmt class (around line 178)
    public class StructDecl : Stmt
    {
        public required string Name;
        public required List<(string, string, bool)> Fields; // (name, type, is_public)
    }

    public class EnumDecl : Stmt
    {
        public required string Name;
        public required List<(string, List<(string, string)>)> Variants; // (variant_name, [(field_name, field_type)])
    }

    public class TraitDecl : Stmt
    {
        public required string Name;
        public required List<FnDecl> Methods;
    }

    public class ImplDecl : Stmt
    {
        public required string TypeName;
        public required string? TraitName; // null for inherent implementations
        public required List<FnDecl> Methods;
    }

    public class MatchExpr : Expr
    {
        public required Expr Value;
        public required List<(Expr, Expr)> Arms; // (pattern, result)
    }

    public class FieldAccessExpr : Expr
    {
        public required Expr Object;
        public required string FieldName;
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
        public class OwnershipTracker
        {
            private Dictionary<string, string> _owners = new();

            public void RegisterOwnership(string varName, string owner)
            {
                _owners[varName] = owner;
            }

            public void TransferOwnership(string fromVar, string toVar)
            {
                if (_owners.TryGetValue(fromVar, out var owner))
                {
                    _owners[toVar] = owner;
                    _owners.Remove(fromVar);
                }
            }

            public bool CheckOwnership(string varName, string expectedOwner)
            {
                return _owners.TryGetValue(varName, out var owner) && owner == expectedOwner;
            }
        }

        private OwnershipTracker _ownership = new();

        public ProgramNode Parse()
        {
            var program = new ProgramNode();
            while (!IsAtEnd()) program.Statements.Add(ParseDeclaration());
            return program;
        }
        // Update ParseDeclaration method (around line 206)
        private Stmt ParseDeclaration()
        {
            if (Match(TokenType.Import, TokenType.Reference, TokenType.Using))
            {
                return ParseImportStatement();
            }

            if (Match(TokenType.Fn)) return ParseFunction("function");
            if (Match(TokenType.Let)) return ParseVarDecl();
            if (Match(TokenType.Struct)) return ParseStructDecl();
            if (Match(TokenType.Enum)) return ParseEnumDecl();
            if (Match(TokenType.Trait)) return ParseTraitDecl();
            if (Match(TokenType.Impl)) return ParseImplDecl();
            return ParseStatement();
        }

        private Stmt ParseImportStatement()
        {
            // Determine which type of import this is
            ImportStmt.ImportType importType;
            TokenType directiveType = Previous().Type;

            if (directiveType == TokenType.Import)
                importType = ImportStmt.ImportType.Import;
            else if (directiveType == TokenType.Reference)
                importType = ImportStmt.ImportType.Reference;
            else if (directiveType == TokenType.Using)
                importType = ImportStmt.ImportType.Using;
            else
                throw new Exception("Invalid import directive type");

            // Get the package name (can be string or identifier)
            string packageName;
            if (Match(TokenType.String))
            {
                packageName = (string)Previous().Literal;
            }
            else if (Match(TokenType.Identifier))
            {
                packageName = Previous().Lexeme;

                // Handle dotted package paths (e.g., "core.math")
                while (Match(TokenType.Dot))
                {
                    packageName += "." + Consume(TokenType.Identifier, "Expect identifier after '.'").Lexeme;
                }
            }
            else
            {
                throw new Exception("Expect package name as string or identifier");
            }

            Consume(TokenType.Semicolon, "Expect ';' after import statement");

            return new ImportStmt { PackageName = packageName, Type = importType };
        }

        // Add after ParseCall (around line 335)
        private Expr ParseFieldAccess()
        {
            var expr = ParseCall();

            while (Match(TokenType.Dot))
            {
                var name = Consume(TokenType.Identifier, "Expect field name after '.'.").Lexeme;
                expr = new FieldAccessExpr { Object = expr, FieldName = name };
            }

            return expr;
        }

        // Add after ParseReturn (around line 302)
        private Expr ParseMatch()
        {
            var value = ParseExpression();
            Consume(TokenType.LBrace, "Expect '{' after match value.");

            var arms = new List<(Expr, Expr)>();
            while (!Check(TokenType.RBrace) && !IsAtEnd())
            {
                var pattern = ParseExpression();
                Consume(TokenType.Arrow, "Expect '=>' after match pattern.");
                var result = ParseExpression();
                Consume(TokenType.Comma, "Expect ',' after match arm.");
                arms.Add((pattern, result));
            }

            Consume(TokenType.RBrace, "Expect '}' after match arms.");
            return new MatchExpr { Value = value, Arms = arms };
        }

        // Add new methods after ParseVarDecl (around line 251)
        private Stmt ParseStructDecl()
        {
            var name = Consume(TokenType.Identifier, "Expect struct name.").Lexeme;
            Consume(TokenType.LBrace, "Expect '{' after struct name.");

            var fields = new List<(string, string, bool)>();
            while (!Check(TokenType.RBrace) && !IsAtEnd())
            {
                bool isPublic = Match(TokenType.Pub);
                var fieldName = Consume(TokenType.Identifier, "Expect field name.").Lexeme;
                Consume(TokenType.Colon, "Expect ':' after field name.");
                var fieldType = Consume(TokenType.Identifier, "Expect field type.").Lexeme;
                Consume(TokenType.Semicolon, "Expect ';' after field declaration.");
                fields.Add((fieldName, fieldType, isPublic));
            }

            Consume(TokenType.RBrace, "Expect '}' after struct fields.");
            return new StructDecl { Name = name, Fields = fields };
        }

        private Stmt ParseEnumDecl()
        {
            var name = Consume(TokenType.Identifier, "Expect enum name.").Lexeme;
            Consume(TokenType.LBrace, "Expect '{' after enum name.");

            var variants = new List<(string, List<(string, string)>)>();
            while (!Check(TokenType.RBrace) && !IsAtEnd())
            {
                var variantName = Consume(TokenType.Identifier, "Expect variant name.").Lexeme;
                List<(string, string)> fields = new();

                if (Match(TokenType.LParen))
                {
                    if (!Check(TokenType.RParen))
                    {
                        do
                        {
                            var fieldName = Consume(TokenType.Identifier, "Expect field name.").Lexeme;
                            Consume(TokenType.Colon, "Expect ':' after field name.");
                            var fieldType = Consume(TokenType.Identifier, "Expect field type.").Lexeme;
                            fields.Add((fieldName, fieldType));
                        } while (Match(TokenType.Comma));
                    }
                    Consume(TokenType.RParen, "Expect ')' after variant fields.");
                }

                Consume(TokenType.Comma, "Expect ',' after variant.");
                variants.Add((variantName, fields));
            }

            Consume(TokenType.RBrace, "Expect '}' after enum variants.");
            return new EnumDecl { Name = name, Variants = variants };
        }

        private Stmt ParseTraitDecl()
        {
            var name = Consume(TokenType.Identifier, "Expect trait name.").Lexeme;
            Consume(TokenType.LBrace, "Expect '{' after trait name.");

            var methods = new List<FnDecl>();
            while (!Check(TokenType.RBrace) && !IsAtEnd())
            {
                methods.Add(ParseFunction("method"));
            }

            Consume(TokenType.RBrace, "Expect '}' after trait methods.");
            return new TraitDecl { Name = name, Methods = methods };
        }

        private Stmt ParseImplDecl()
        {
            var typeName = Consume(TokenType.Identifier, "Expect type name.").Lexeme;

            string? traitName = null;
            if (Match(TokenType.For))
            {
                traitName = Consume(TokenType.Identifier, "Expect trait name after 'for'.").Lexeme;
            }

            Consume(TokenType.LBrace, "Expect '{' for impl block.");

            var methods = new List<FnDecl>();
            while (!Check(TokenType.RBrace) && !IsAtEnd())
            {
                methods.Add(ParseFunction("method"));
            }

            Consume(TokenType.RBrace, "Expect '}' after impl methods.");
            return new ImplDecl { TypeName = typeName, TraitName = traitName, Methods = methods };
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
        // Modify ParseVarDecl (around line 244)
        private Stmt ParseVarDecl()
        {
            bool isPublic = Match(TokenType.Pub);
            bool isMutable = Match(TokenType.Mut);

            var name = Consume(TokenType.Identifier, "Expect variable name.").Lexeme;
            Consume(TokenType.Colon, "Expect ':'.");
            var type = Consume(TokenType.Identifier, "Expect type.").Lexeme;
            Consume(TokenType.Equal, "Expect '='.");
            var init = ParseExpression();
            Consume(TokenType.Semicolon, "Expect ';' after var declaration.");

            return new VarDecl
            {
                Name = name,
                Type = type,
                Initializer = init,
                IsMutable = isMutable,
                IsPublic = isPublic
            };
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
            throw new Exception(message + $" Line {_current}");
        }
        private bool Check(TokenType type) => !IsAtEnd() && Peek().Type == type;
        private Token Advance() { if (!IsAtEnd()) _current++; return Previous(); }
        private bool IsAtEnd() => Peek().Type == TokenType.EOF;
        private Token Peek() => _tokens[_current];
        private Token Previous() => _tokens[_current - 1];
    }

    // === TypeChecker ===
    // === TypeChecker ===
    public class TypeChecker
    {
        // Enhanced type representation with mutability and ownership info
        private record TypeInfo(
            string Type,
            bool IsMutable,
            string? Owner,
            bool IsBorrowed,
            bool IsReference,
            int Lifetime);

        // Primary storage for variables and their type information
        private Dictionary<string, TypeInfo> _variables = new();

        // Function signatures with full type information including generics
        private Dictionary<string, (List<(string name, TypeInfo type)> parameters, TypeInfo returnType)> _functions = new();

        // Custom type definitions (structs, enums)
        private Dictionary<string, (string kind, Dictionary<string, TypeInfo> members)> _userTypes = new();

        // Trait definitions and implementations
        private Dictionary<string, List<(string name, TypeInfo returnType, List<(string, TypeInfo)> parameters)>> _traits = new();
        private Dictionary<string, Dictionary<string, List<(string name, TypeInfo returnType, List<(string, TypeInfo)> parameters)>>> _implementations = new();

        // Stack of current ownership contexts for checking move semantics
        private Stack<HashSet<string>> _movedVariables = new();

        // Cache for type compatibility checks to improve performance
        private Dictionary<(string, string), bool> _typeCompatibilityCache = new();

        // Lifetimes counter for reference checking
        private int _currentLifetime = 0;

        // Current function context for return type checking
        private string _currentFunction = string.Empty;
        private TypeInfo? _currentReturnType = null;

        // Performance optimization: avoid string concatenation in hot paths
        private static readonly TypeInfo _boolType = new("bool", false, null, false, false, 0);
        private static readonly TypeInfo _voidType = new("void", false, null, false, false, 0);
        private static readonly TypeInfo _i32Type = new("i32", false, null, false, false, 0);
        private static readonly TypeInfo _f64Type = new("f64", false, null, false, false, 0);
        private static readonly TypeInfo _stringType = new("string", false, null, false, false, 0);
        private static readonly TypeInfo _anyType = new("any", false, null, false, false, 0);

        public void Check(ProgramNode program)
        {
            // Reset state for fresh analysis
            _variables.Clear();
            _functions.Clear();
            _userTypes.Clear();
            _traits.Clear();
            _implementations.Clear();
            _movedVariables.Clear();
            _typeCompatibilityCache.Clear();
            _currentLifetime = 0;

            try
            {
                // Multi-pass analysis for interdependent declarations

                // First pass: collect user-defined types (structs, enums)
                CollectUserTypes(program);

                // Second pass: collect traits and trait bounds
                CollectTraits(program);

                // Third pass: collect function signatures and implementations
                CollectFunctionSignatures(program);

                // Fourth pass: verify implementations match trait definitions
                VerifyTraitImplementations();

                // Fifth pass: perform detailed type checking of all statements and expressions
                foreach (var stmt in program.Statements)
                {
                    CheckStatement(stmt, new HashSet<string>());
                }

                // Final pass: verify all required trait implementations exist
                VerifyRequiredImplementations();
            }
            catch (Exception)
            {
                // Clean up resources
                _typeCompatibilityCache.Clear();
                _movedVariables.Clear();
                throw;
            }
        }

        private void CollectUserTypes(ProgramNode program)
        {
            foreach (var stmt in program.Statements)
            {
                switch (stmt)
                {
                    case StructDecl sd:
                        var members = new Dictionary<string, TypeInfo>();
                        foreach (var (name, type, isPublic) in sd.Fields)
                        {
                            // Validate field type
                            if (!IsValidType(type) && !_userTypes.ContainsKey(type))
                            {
                                throw new Exception($"Unknown type '{type}' for field '{name}' in struct '{sd.Name}'");
                            }

                            members[name] = new TypeInfo(type, false, null, false, false, 0);
                        }
                        _userTypes[sd.Name] = ("struct", members);
                        break;

                    case EnumDecl ed:
                        var enumMembers = new Dictionary<string, TypeInfo>();
                        foreach (var (variantName, fields) in ed.Variants)
                        {
                            // Create composite type for variant
                            var variantType = $"{ed.Name}::{variantName}";

                            // For each field in the variant
                            int fieldIndex = 0;
                            foreach (var (fieldName, fieldType) in fields)
                            {
                                if (!IsValidType(fieldType) && !_userTypes.ContainsKey(fieldType))
                                {
                                    throw new Exception($"Unknown type '{fieldType}' for field '{fieldName}' in enum variant '{variantName}'");
                                }

                                // Use index-based field names for unnamed fields
                                var effectiveFieldName = string.IsNullOrEmpty(fieldName)
                                    ? $"{fieldIndex++}"
                                    : fieldName;

                                enumMembers[$"{variantName}.{effectiveFieldName}"] = new TypeInfo(fieldType, false, null, false, false, 0);
                            }
                        }
                        _userTypes[ed.Name] = ("enum", enumMembers);
                        break;
                }
            }

            // Second pass to resolve any interdependent types
            ValidateNoCircularTypeDefinitions();
        }

        private void ValidateNoCircularTypeDefinitions()
        {
            var visited = new HashSet<string>();
            var visiting = new HashSet<string>();

            foreach (var typeName in _userTypes.Keys)
            {
                if (!visited.Contains(typeName))
                {
                    CheckTypeCircularity(typeName, visited, visiting);
                }
            }
        }

        private void CheckTypeCircularity(string typeName, HashSet<string> visited, HashSet<string> visiting)
        {
            if (visiting.Contains(typeName))
            {
                throw new Exception($"Circular type dependency detected involving '{typeName}'");
            }

            if (visited.Contains(typeName))
            {
                return;
            }

            visiting.Add(typeName);

            if (_userTypes.TryGetValue(typeName, out var typeInfo))
            {
                foreach (var fieldInfo in typeInfo.members.Values)
                {
                    if (_userTypes.ContainsKey(fieldInfo.Type))
                    {
                        CheckTypeCircularity(fieldInfo.Type, visited, visiting);
                    }
                }
            }

            visiting.Remove(typeName);
            visited.Add(typeName);
        }

        private void CollectTraits(ProgramNode program)
        {
            foreach (var stmt in program.Statements)
            {
                if (stmt is TraitDecl td)
                {
                    var methods = new List<(string name, TypeInfo returnType, List<(string, TypeInfo)> parameters)>();

                    foreach (var method in td.Methods)
                    {
                        var parameters = new List<(string, TypeInfo)>();
                        foreach (var (paramName, paramType) in method.Params)
                        {
                            if (!IsValidType(paramType) && !_userTypes.ContainsKey(paramType))
                            {
                                throw new Exception($"Unknown parameter type '{paramType}' in trait method '{method.Name}'");
                            }

                            parameters.Add((paramName, new TypeInfo(paramType, false, null, false, false, 0)));
                        }

                        if (!IsValidType(method.ReturnType) && !_userTypes.ContainsKey(method.ReturnType))
                        {
                            throw new Exception($"Unknown return type '{method.ReturnType}' in trait method '{method.Name}'");
                        }

                        methods.Add((method.Name, new TypeInfo(method.ReturnType, false, null, false, false, 0), parameters));
                    }

                    _traits[td.Name] = methods;
                }
            }
        }

        private void CollectFunctionSignatures(ProgramNode program)
        {
            foreach (var stmt in program.Statements)
            {
                switch (stmt)
                {
                    case FnDecl fn:
                        var parameters = new List<(string name, TypeInfo type)>();
                        foreach (var (paramName, paramType) in fn.Params)
                        {
                            var isMutable = false; // In a full impl, we'd check for '&mut' prefix
                            var isReference = false; // Check for '&' prefix

                            var effectiveType = paramType;
                            if (paramType.StartsWith("&mut "))
                            {
                                isMutable = true;
                                isReference = true;
                                effectiveType = paramType.Substring(5);
                            }
                            else if (paramType.StartsWith("&"))
                            {
                                isReference = true;
                                effectiveType = paramType.Substring(1);
                            }

                            if (!IsValidType(effectiveType) && !_userTypes.ContainsKey(effectiveType))
                            {
                                throw new Exception($"Unknown parameter type '{effectiveType}' in function '{fn.Name}'");
                            }

                            parameters.Add((paramName, new TypeInfo(effectiveType, isMutable, null, false, isReference, 0)));
                        }

                        if (!IsValidType(fn.ReturnType) && !_userTypes.ContainsKey(fn.ReturnType))
                        {
                            throw new Exception($"Unknown return type '{fn.ReturnType}' in function '{fn.Name}'");
                        }

                        _functions[fn.Name] = (parameters, new TypeInfo(fn.ReturnType, false, null, false, false, 0));
                        break;

                    case ImplDecl impl:
                        // Handle implementing traits
                        if (impl.TraitName != null)
                        {
                            if (!_traits.ContainsKey(impl.TraitName))
                            {
                                throw new Exception($"Implementing unknown trait '{impl.TraitName}'");
                            }

                            if (!_userTypes.ContainsKey(impl.TypeName))
                            {
                                throw new Exception($"Implementing trait for unknown type '{impl.TypeName}'");
                            }

                            var methods = new List<(string name, TypeInfo returnType, List<(string, TypeInfo)> parameters)>();
                            foreach (var method in impl.Methods)
                            {
                                parameters = new List<(string, TypeInfo)>();
                                foreach (var (paramName, paramType) in method.Params)
                                {
                                    if (!IsValidType(paramType) && !_userTypes.ContainsKey(paramType))
                                    {
                                        throw new Exception($"Unknown parameter type '{paramType}' in implementation method '{method.Name}'");
                                    }

                                    parameters.Add((paramName, new TypeInfo(paramType, false, null, false, false, 0)));
                                }

                                if (!IsValidType(method.ReturnType) && !_userTypes.ContainsKey(method.ReturnType))
                                {
                                    throw new Exception($"Unknown return type '{method.ReturnType}' in implementation method '{method.Name}'");
                                }

                                methods.Add((method.Name, new TypeInfo(method.ReturnType, false, null, false, false, 0), parameters));

                                // Also register as a function with 'self' parameter
                                var fullParams = new List<(string name, TypeInfo type)>();
                                fullParams.Add(("self", new TypeInfo(impl.TypeName, false, null, false, true, 0)));
                                foreach (var (name, type) in parameters)
                                {
                                    fullParams.Add((name, type));
                                }

                                _functions[$"{impl.TypeName}::{method.Name}"] = (fullParams, new TypeInfo(method.ReturnType, false, null, false, false, 0));
                            }

                            if (!_implementations.ContainsKey(impl.TraitName))
                            {
                                _implementations[impl.TraitName] = new Dictionary<string, List<(string name, TypeInfo returnType, List<(string, TypeInfo)> parameters)>>();
                            }

                            _implementations[impl.TraitName][impl.TypeName] = methods;
                        }
                        // Handle inherent implementations
                        else
                        {
                            if (!_userTypes.ContainsKey(impl.TypeName))
                            {
                                throw new Exception($"Implementing methods for unknown type '{impl.TypeName}'");
                            }

                            foreach (var method in impl.Methods)
                            {
                                parameters = new List<(string name, TypeInfo type)>();

                                // Add implicit self parameter
                                parameters.Add(("self", new TypeInfo(impl.TypeName, false, null, false, true, 0)));

                                foreach (var (paramName, paramType) in method.Params)
                                {
                                    if (!IsValidType(paramType) && !_userTypes.ContainsKey(paramType))
                                    {
                                        throw new Exception($"Unknown parameter type '{paramType}' in method '{method.Name}'");
                                    }

                                    parameters.Add((paramName, new TypeInfo(paramType, false, null, false, false, 0)));
                                }

                                if (!IsValidType(method.ReturnType) && !_userTypes.ContainsKey(method.ReturnType))
                                {
                                    throw new Exception($"Unknown return type '{method.ReturnType}' in method '{method.Name}'");
                                }

                                _functions[$"{impl.TypeName}::{method.Name}"] = (parameters, new TypeInfo(method.ReturnType, false, null, false, false, 0));
                            }
                        }
                        break;
                }
            }
        }

        private void VerifyTraitImplementations()
        {
            foreach (var (traitName, impls) in _implementations)
            {
                var traitMethods = _traits[traitName];

                foreach (var (typeName, methods) in impls)
                {
                    // Check that all required trait methods are implemented
                    var implementedMethods = methods.Select(m => m.name).ToHashSet();
                    foreach (var traitMethod in traitMethods)
                    {
                        if (!implementedMethods.Contains(traitMethod.name))
                        {
                            throw new Exception($"Type '{typeName}' does not implement required trait method '{traitMethod.name}' from trait '{traitName}'");
                        }

                        // Find the implementation
                        var impl = methods.First(m => m.name == traitMethod.name);

                        // Check return type
                        if (!AreTypesCompatible(impl.returnType.Type, traitMethod.returnType.Type))
                        {
                            throw new Exception($"Return type mismatch in implementation of '{traitMethod.name}' for trait '{traitName}': expected '{traitMethod.returnType.Type}', got '{impl.returnType.Type}'");
                        }

                        // Check parameter count
                        if (impl.parameters.Count != traitMethod.parameters.Count)
                        {
                            throw new Exception($"Parameter count mismatch in implementation of '{traitMethod.name}' for trait '{traitName}': expected {traitMethod.parameters.Count}, got {impl.parameters.Count}");
                        }

                        // Check parameter types
                        for (int i = 0; i < impl.parameters.Count; i++)
                        {
                            if (!AreTypesCompatible(impl.parameters[i].Item2.Type, traitMethod.parameters[i].Item2.Type))
                            {
                                throw new Exception($"Parameter type mismatch in implementation of '{traitMethod.name}' for trait '{traitName}': parameter {i + 1} expected '{traitMethod.parameters[i].Item2.Type}', got '{impl.parameters[i].Item2.Type}'");
                            }
                        }
                    }
                }
            }
        }

        private void VerifyRequiredImplementations()
        {
            // This would check that any usage of trait bounds has corresponding implementations
            // Simplified for now
        }

        private void CheckStatement(Stmt stmt, HashSet<string> movedVars)
        {
            switch (stmt)
            {
                case FnDecl fn:
                    CheckFunction(fn);
                    break;

                case VarDecl vd:
                    var exprType = GetExpressionType(vd.Initializer, movedVars);

                    // Allow numeric coercion between i32/f64/etc.
                    if (exprType.Type != vd.Type && !(IsNumericType(vd.Type) && IsNumericType(exprType.Type)))
                    {
                        throw new Exception($"Type mismatch in variable declaration: expected {vd.Type}, got {exprType.Type}");
                    }

                    // Transfer ownership if the value is moved
                    string? owner = null;
                    if (vd.Initializer is VariableExpr ve && _variables.TryGetValue(ve.Name, out var sourceVar) && !sourceVar.IsReference)
                    {
                        // Transferring ownership from source variable to this one
                        owner = $"var:{vd.Name}";
                        movedVars.Add(ve.Name);
                    }
                    else
                    {
                        // New ownership
                        owner = $"var:{vd.Name}";
                    }

                    _variables[vd.Name] = new TypeInfo(vd.Type, vd.IsMutable, owner, false, false, _currentLifetime);
                    break;

                case IfStmt ifStmt:
                    var condType = GetExpressionType(ifStmt.Condition, movedVars);
                    if (condType.Type != "bool")
                    {
                        throw new Exception("Condition must be a boolean expression");
                    }

                    // Track moved variables in each branch
                    var thenMoved = new HashSet<string>(movedVars);
                    var elseMoved = new HashSet<string>(movedVars);

                    foreach (var s in ifStmt.ThenBranch)
                    {
                        CheckStatement(s, thenMoved);
                    }

                    if (ifStmt.ElseBranch != null)
                    {
                        foreach (var s in ifStmt.ElseBranch)
                        {
                            CheckStatement(s, elseMoved);
                        }

                        // Variables moved in both branches are definitely moved
                        foreach (var moved in thenMoved.Intersect(elseMoved))
                        {
                            movedVars.Add(moved);
                        }
                    }
                    // Variables moved in the then branch may or may not be moved overall
                    else
                    {
                        // In a full implementation, we'd track potentially moved vars
                    }
                    break;

                case WhileStmt ws:
                    var whileCondType = GetExpressionType(ws.Condition, movedVars);
                    if (whileCondType.Type != "bool")
                    {
                        throw new Exception("Condition must be a boolean expression");
                    }

                    // Conservative approach: don't allow moves inside loops
                    // because we can't statically know how many times the loop executes
                    _movedVariables.Push(new HashSet<string>());

                    foreach (var s in ws.Body)
                    {
                        CheckStatement(s, _movedVariables.Peek());
                    }

                    // Check that no variables were moved inside the loop
                    if (_movedVariables.Pop().Count > 0)
                    {
                        throw new Exception("Cannot move variables inside loops with unknown iteration count");
                    }
                    break;

                case ForStmt fs:
                    var startType = GetExpressionType(fs.Start, movedVars);
                    var endType = GetExpressionType(fs.End, movedVars);

                    if (!IsNumericType(startType.Type) || !IsNumericType(endType.Type))
                    {
                        throw new Exception("For loop bounds must be numeric");
                    }

                    // Set up the range variable with a new scope
                    _variables[fs.Var] = new TypeInfo("i32", false, null, false, false, _currentLifetime);

                    // Create a new move tracking scope for the loop
                    var loopMoved = new HashSet<string>();
                    foreach (var s in fs.Body)
                    {
                        CheckStatement(s, loopMoved);
                    }

                    // Apply moves from loop (conservatively - could be more nuanced)
                    foreach (var moved in loopMoved)
                    {
                        movedVars.Add(moved);
                    }

                    // Clean up the range variable
                    _variables.Remove(fs.Var);
                    break;

                case ReturnStmt rs:
                    if (rs.Value != null)
                    {
                        var returnType = GetExpressionType(rs.Value, movedVars);

                        if (_currentReturnType != null && !AreTypesCompatible(returnType.Type, _currentReturnType.Type))
                        {
                            throw new Exception($"Return type mismatch: function returns {_currentReturnType.Type}, but returning {returnType.Type}");
                        }
                    }
                    else if (_currentReturnType != null && _currentReturnType.Type != "void")
                    {
                        throw new Exception($"Function must return a value of type {_currentReturnType.Type}");
                    }
                    break;

                case ExprStmt es:
                    GetExpressionType(es.Expression, movedVars);
                    break;

                case BlockStmt bs:
                    // Create a new scope with increased lifetime
                    _currentLifetime++;
                    var outerLifetime = _currentLifetime;

                    // Track ownership changes locally first
                    var localMovedVars = new HashSet<string>(movedVars);

                    foreach (var s in bs.Statements)
                    {
                        CheckStatement(s, localMovedVars);
                    }

                    // Propagate ownership changes up
                    foreach (var moved in localMovedVars)
                    {
                        movedVars.Add(moved);
                    }

                    // Validate that no references with this lifetime escape
                    ValidateNoEscapingReferences(outerLifetime);
                    _currentLifetime--;
                    break;

                case StructDecl _:
                case EnumDecl _:
                case TraitDecl _:
                case ImplDecl _:
                    // These were handled in the collection phases
                    break;

                default:
                    throw new Exception($"Unsupported statement type: {stmt.GetType().Name}");
            }
        }

        private void ValidateNoEscapingReferences(int lifetime)
        {
            // Check that no references with this lifetime are stored in longer-lived variables
            foreach (var (name, info) in _variables)
            {
                if (info.IsReference && info.Lifetime == lifetime)
                {
                    // Don't allow storing references in longer-lived variables
                    // In a full implementation, we'd check specific storage locations

                    // For simplicity, just warn about all references with this lifetime
                    Console.WriteLine($"Warning: Reference in '{name}' might outlive its scope.");
                }
            }
        }

        private void CheckFunction(FnDecl fn)
        {
            // Save outer context
            var oldFunction = _currentFunction;
            var oldReturnType = _currentReturnType;
            var oldLifetime = _currentLifetime;

            // Set up function context
            _currentFunction = fn.Name;
            _currentReturnType = new TypeInfo(fn.ReturnType, false, null, false, false, 0);
            _currentLifetime++;

            // Clear moved variables for this function
            var movedVars = new HashSet<string>();

            // Create a new scope for parameters
            var outerVariables = new Dictionary<string, TypeInfo>(_variables);
            _variables.Clear();

            // Add parameters to local scope
            foreach (var (name, type) in fn.Params)
            {
                var isMutable = false;
                var isReference = false;

                var effectiveType = type;
                if (type.StartsWith("&mut "))
                {
                    isMutable = true;
                    isReference = true;
                    effectiveType = type.Substring(5);
                }
                else if (type.StartsWith("&"))
                {
                    isReference = true;
                    effectiveType = type.Substring(1);
                }

                _variables[name] = new TypeInfo(effectiveType, isMutable, $"param:{name}", false, isReference, _currentLifetime);
            }

            // Check function body
            foreach (var stmt in fn.Body)
            {
                CheckStatement(stmt, movedVars);
            }

            // Validate return paths (simplified)
            if (fn.ReturnType != "void" && !HasReturnOnAllPaths(fn.Body))
            {
                throw new Exception($"Function '{fn.Name}' might not return a value on all code paths");
            }

            // Restore outer context
            _variables = outerVariables;
            _currentFunction = oldFunction;
            _currentReturnType = oldReturnType;
            _currentLifetime = oldLifetime;
        }

        private bool HasReturnOnAllPaths(List<Stmt> statements)
        {
            // Simplified check for returns on all paths
            // In a real implementation, this would be more sophisticated

            if (statements.Count == 0)
            {
                return false;
            }

            // If the last statement is a return, the path returns
            if (statements[statements.Count - 1] is ReturnStmt)
            {
                return true;
            }

            // Check if the last statement is an if with returns in both branches
            if (statements[statements.Count - 1] is IfStmt ifStmt)
            {
                bool thenReturns = HasReturnOnAllPaths(ifStmt.ThenBranch);
                bool elseReturns = ifStmt.ElseBranch != null && HasReturnOnAllPaths(ifStmt.ElseBranch);

                return thenReturns && elseReturns;
            }

            // Check if the last statement is a block that returns
            if (statements[statements.Count - 1] is BlockStmt blockStmt)
            {
                return HasReturnOnAllPaths(blockStmt.Statements);
            }

            return false;
        }

        private TypeInfo GetExpressionType(Expr expr, HashSet<string> movedVars)
        {
            switch (expr)
            {
                case LiteralExpr lit:
                    return lit.Value switch
                    {
                        double => _f64Type,
                        bool => _boolType,
                        string => _stringType,
                        _ => _anyType
                    };

                case VariableExpr ve:
                    if (_variables.TryGetValue(ve.Name, out var varInfo))
                    {
                        // Check for use after move
                        if (movedVars.Contains(ve.Name))
                        {
                            throw new Exception($"Use of moved variable '{ve.Name}'");
                        }

                        return varInfo;
                    }
                    throw new Exception($"Undefined variable: {ve.Name}");

                case BinaryExpr be:
                    var leftType = GetExpressionType(be.Left, movedVars);

                    // For assignments, we need to check mutability before evaluating the right side
                    if (be.Op.Type == TokenType.Equal)
                    {
                        if (be.Left is VariableExpr varExpr)
                        {
                            if (!_variables.TryGetValue(varExpr.Name, out var varInfo3) || !varInfo3.IsMutable)
                            {
                                throw new Exception($"Cannot assign to immutable variable '{varExpr.Name}'");
                            }
                        }
                        else if (be.Left is FieldAccessExpr fieldAccess)
                        {
                            // Would check field mutability in a full implementation
                        }
                        else
                        {
                            throw new Exception("Left side of assignment must be a variable or field");
                        }
                    }

                    var rightType = GetExpressionType(be.Right, movedVars);

                    switch (be.Op.Type)
                    {
                        case TokenType.Plus:
                            if (leftType.Type == "string" || rightType.Type == "string")
                            {
                                return _stringType;
                            }
                            else if (IsNumericType(leftType.Type) && IsNumericType(rightType.Type))
                            {
                                return CombineNumericTypes(leftType.Type, rightType.Type);
                            }
                            throw new Exception($"Cannot apply + to types {leftType.Type} and {rightType.Type}");

                        case TokenType.Minus:
                        case TokenType.Star:
                        case TokenType.Slash:
                        case TokenType.Percent:
                            if (IsNumericType(leftType.Type) && IsNumericType(rightType.Type))
                            {
                                return CombineNumericTypes(leftType.Type, rightType.Type);
                            }
                            throw new Exception($"Cannot apply {be.Op.Lexeme} to types {leftType.Type} and {rightType.Type}");

                        case TokenType.EqualEqual:
                        case TokenType.BangEqual:
                        case TokenType.Greater:
                        case TokenType.GreaterEqual:
                        case TokenType.Less:
                        case TokenType.LessEqual:
                            // Comparison operators return bool
                            return _boolType;

                        case TokenType.Equal:
                            // Assignment returns the assigned value's type

                            // Check type compatibility for assignment
                            if (!AreTypesCompatible(leftType.Type, rightType.Type))
                            {
                                throw new Exception($"Cannot assign {rightType.Type} to {leftType.Type}");
                            }

                            // Handle ownership transfer in assignment
                            if (be.Right is VariableExpr ve2 && !rightType.IsReference)
                            {
                                movedVars.Add(ve2.Name);

                                // If we're assigning from a variable, transfer ownership
                                if (be.Left is VariableExpr targetVar)
                                {
                                    if (_variables.TryGetValue(targetVar.Name, out var targetInfo))
                                    {
                                        _variables[targetVar.Name] = targetInfo with { Owner = $"var:{targetVar.Name}" };
                                    }
                                }
                            }

                            return leftType;

                        default:
                            throw new Exception($"Unsupported binary operator: {be.Op.Lexeme}");
                    }

                case UnaryExpr ue:
                    var operandType = GetExpressionType(ue.Right, movedVars);

                    switch (ue.Op.Type)
                    {
                        case TokenType.Minus:
                            if (IsNumericType(operandType.Type))
                            {
                                return operandType;
                            }
                            throw new Exception($"Cannot negate non-numeric type: {operandType.Type}");

                        case TokenType.Bang:
                            if (operandType.Type == "bool")
                            {
                                return _boolType;
                            }
                            throw new Exception($"Cannot logically negate non-boolean type: {operandType.Type}");

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
                                var argType = GetExpressionType(ce.Arguments[i], movedVars);
                                var paramInfo = fnInfo.parameters[i].type;

                                if (!AreTypesCompatible(argType.Type, paramInfo.Type))
                                {
                                    throw new Exception($"Argument {i + 1} type mismatch: expected {paramInfo.Type}, got {argType.Type}");
                                }

                                // Check for moves
                                if (ce.Arguments[i] is VariableExpr argVar && paramInfo.IsReference == false)
                                {
                                    // The argument is moved if the parameter takes ownership
                                    movedVars.Add(argVar.Name);
                                }
                            }

                            return fnInfo.returnType;
                        }

                        // Check for built-in functions
                        if (callee.Name == "print") return _voidType;
                        if (callee.Name == "len") return _i32Type;
                        if (callee.Name == "range") return new TypeInfo("range", false, null, false, false, 0);
                        if (callee.Name == "toString") return _stringType;

                        throw new Exception($"Undefined function: {callee.Name}");
                    }
                    else if (ce.Callee is FieldAccessExpr methodAccess)
                    {
                        // Method call on an object
                        var methodName = $"{GetExpressionType(methodAccess.Object, movedVars).Type}::{methodAccess.FieldName}";

                        if (_functions.TryGetValue(methodName, out var methodInfo))
                        {
                            // First param is 'self', check remaining params
                            if (ce.Arguments.Count != methodInfo.parameters.Count - 1)
                            {
                                throw new Exception($"Method {methodName} expects {methodInfo.parameters.Count - 1} arguments but got {ce.Arguments.Count}");
                            }

                            // Check argument types (skipping 'self')
                            for (int i = 0; i < ce.Arguments.Count; i++)
                            {
                                var argType = GetExpressionType(ce.Arguments[i], movedVars);
                                var paramInfo = methodInfo.parameters[i + 1].type;

                                if (!AreTypesCompatible(argType.Type, paramInfo.Type))
                                {
                                    throw new Exception($"Argument {i + 1} type mismatch in call to {methodName}: expected {paramInfo.Type}, got {argType.Type}");
                                }
                            }

                            return methodInfo.returnType;
                        }

                        throw new Exception($"Undefined method: {methodAccess.FieldName} on type {GetExpressionType(methodAccess.Object, movedVars).Type}");
                    }

                    throw new Exception("Call expression must refer to a function or method");

                case FieldAccessExpr fa:
                    var objectType = GetExpressionType(fa.Object, movedVars);

                    // Check if this is a field access on a struct
                    if (_userTypes.TryGetValue(objectType.Type, out var typeInfo) && typeInfo.kind == "struct")
                    {
                        if (typeInfo.members.TryGetValue(fa.FieldName, out var fieldType))
                        {
                            return fieldType;
                        }

                        throw new Exception($"Type {objectType.Type} has no field named {fa.FieldName}");
                    }

                    throw new Exception($"Cannot access fields on type {objectType.Type}");

                case MatchExpr ma:
                    var valueType = GetExpressionType(ma.Value, movedVars);

                    // Verify match arms
                    if (ma.Arms.Count == 0)
                    {
                        throw new Exception("Match expression must have at least one arm");
                    }

                    TypeInfo resultType = null;
                    foreach (var (pattern, result) in ma.Arms)
                    {
                        // Simplified pattern matching - in a full implementation we'd check pattern compatibility
                        var patternType = GetExpressionType(pattern, new HashSet<string>());

                        if (!AreTypesCompatible(valueType.Type, patternType.Type))
                        {
                            throw new Exception($"Pattern type {patternType.Type} is not compatible with matched value type {valueType.Type}");
                        }

                        var armResultType = GetExpressionType(result, movedVars);

                        if (resultType == null)
                        {
                            resultType = armResultType;
                        }
                        else if (!AreTypesCompatible(resultType.Type, armResultType.Type))
                        {
                            throw new Exception($"All match arms must return the same type. Found {resultType.Type} and {armResultType.Type}");
                        }
                    }

                    return resultType;

                case GroupingExpr ge:
                    return GetExpressionType(ge.Expression, movedVars);

                default:
                    throw new Exception($"Unsupported expression type: {expr.GetType().Name}");
            }
        }

        private bool IsValidType(string type)
        {
            return type == "i32" || type == "f64" || type == "bool" || type == "string" ||
                   type == "void" || type == "i64" || type == "f32" || type == "any" ||
                   type.StartsWith("&mut ") || type.StartsWith("&");
        }

        private bool IsNumericType(string type)
        {
            return type == "i32" || type == "f64" || type == "i64" || type == "f32";
        }

        private TypeInfo CombineNumericTypes(string type1, string type2)
        {
            // Simplified numeric type promotion
            if (type1 == "f64" || type2 == "f64") return _f64Type;
            if (type1 == "f32" || type2 == "f32") return new TypeInfo("f32", false, null, false, false, 0);
            if (type1 == "i64" || type2 == "i64") return new TypeInfo("i64", false, null, false, false, 0);
            return _i32Type;
        }

        private bool AreTypesCompatible(string sourceType, string targetType)
        {
            // Check cache first for performance
            var key = (sourceType, targetType);
            if (_typeCompatibilityCache.TryGetValue(key, out bool result))
            {
                return result;
            }

            // Exact match
            if (sourceType == targetType)
            {
                _typeCompatibilityCache[key] = true;
                return true;
            }

            // Numeric types are compatible for coercion
            if (IsNumericType(sourceType) && IsNumericType(targetType))
            {
                _typeCompatibilityCache[key] = true;
                return true;
            }

            // References compatibility
            if (sourceType.StartsWith("&") && targetType.StartsWith("&"))
            {
                var sourceBare = sourceType.StartsWith("&mut ") ? sourceType.Substring(5) : sourceType.Substring(1);
                var targetBare = targetType.StartsWith("&mut ") ? targetType.Substring(5) : targetType.Substring(1);

                // References are compatible if their base types are compatible
                bool compatible = AreTypesCompatible(sourceBare, targetBare);

                // But you can't convert a shared reference to a mutable one
                if (compatible && !sourceType.StartsWith("&mut ") && targetType.StartsWith("&mut "))
                {
                    compatible = false;
                }

                _typeCompatibilityCache[key] = compatible;
                return compatible;
            }

            // Trait compatibility would be checked here

            _typeCompatibilityCache[key] = false;
            return false;
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
        private readonly Dictionary<string, ProgramNode> _packages = new();
        private readonly Dictionary<string, string> _packagePaths = new();

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

        public void RegisterPackagePath(string name, string filePath) // this will be automated 
        {
            _packagePaths[name] = filePath;
        }
        private ProgramNode LoadPackage(string packageName)
        {
            // Check if already loaded
            if (_packages.TryGetValue(packageName, out var program))
            {
                return program;
            }

            // Check if path is registered
            if (!_packagePaths.TryGetValue(packageName, out var path))
            {
                // Try to resolve using standard locations
                path = ResolvePackagePath(packageName);
                if (path == null)
                {
                    throw new Exception($"Could not find package '{packageName}'");
                }
            }

            // Load and parse the package
            string source = File.ReadAllText(path);
            var lexer = new Lexer(source);
            var tokens = lexer.ScanTokens();
            var parser = new Parser(tokens);
            var packageProgram = parser.Parse();

            // Cache the parsed program
            _packages[packageName] = packageProgram;

            return packageProgram;
        }
        private string ResolvePackagePath(string packageName)
        {
            var searchPath = "";

            // Log the package search paths using the "plasticcmd pluginpath" command
            try
            {
                var process = new System.Diagnostics.Process
                {
                    StartInfo = new System.Diagnostics.ProcessStartInfo
                    {
                        FileName = "plasticcmd",
                        Arguments = $"pluginpath",
                        RedirectStandardOutput = true,
                        UseShellExecute = false,
                        CreateNoWindow = true
                    }
                };
                process.Start();
                string output = process.StandardOutput.ReadToEnd();
                process.WaitForExit();
                searchPath = output.Trim();
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Failed to run 'plasticcmd pluginpath': {ex.Message}");
            }

            // Convert package.name to package/name
            var packagePath = packageName.Replace('.', Path.DirectorySeparatorChar);

            // Try with .pl extension
            var fullPath = Path.Combine(searchPath, $"{packagePath}.pl");
            if (File.Exists(fullPath))
            {
                return fullPath;
            }

            return null;
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

            // add file stuff
            _globals["readFile"] = new Func<string, string>(path =>
            {
                if (File.Exists(path))
                {
                    return File.ReadAllText(path);
                }
                throw new Exception($"File '{path}' not found.");
            });
            _globals["writeFile"] = new Action<string, string>((path, content) =>
            {
                File.WriteAllText(path, content);
            });
            _globals["appendFile"] = new Action<string, string>((path, content) =>
            {
                File.AppendAllText(path, content);
            });
            _globals["deleteFile"] = new Action<string>(path =>
            {
                if (File.Exists(path))
                {
                    File.Delete(path);
                }
                else
                {
                    throw new Exception($"File '{path}' not found.");
                }
            });
            _globals["exists"] = new Func<string, bool>(path => File.Exists(path));
            _globals["mkdir"] = new Action<string>(path =>
            {
                if (!Directory.Exists(path))
                {
                    Directory.CreateDirectory(path);
                }
            });
            _globals["rmdir"] = new Action<string>(path =>
            {
                if (Directory.Exists(path))
                {
                    Directory.Delete(path, true);
                }
                else
                {
                    throw new Exception($"Directory '{path}' not found.");
                }
            });
            _globals["listDir"] = new Func<string, List<string>>(path =>
            {
                if (Directory.Exists(path))
                {
                    return Directory.GetFiles(path).ToList();
                }
                throw new Exception($"Directory '{path}' not found.");
            });
            _globals["copyFile"] = new Action<string, string>((source, destination) =>
            {
                if (File.Exists(source))
                {
                    File.Copy(source, destination);
                }
                else
                {
                    throw new Exception($"File '{source}' not found.");
                }
            });
            _globals["moveFile"] = new Action<string, string>((source, destination) =>
            {
                if (File.Exists(source))
                {
                    File.Move(source, destination);
                }
                else
                {
                    throw new Exception($"File '{source}' not found.");
                }
            });
            _globals["renameFile"] = new Action<string, string>((oldName, newName) =>
            {
                if (File.Exists(oldName))
                {
                    File.Move(oldName, newName);
                }
                else
                {
                    throw new Exception($"File '{oldName}' not found.");
                }
            });
        }

        public object? Evaluate(string source, bool readPlugins = true)
        {
            try
            {
                var lexer = new Lexer(source);
                var tokens = lexer.ScanTokens();

                var parser = new Parser(tokens);
                var program = parser.Parse();

                if (readPlugins)
                {
                    ProcessImports(program);
                }

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

        private void ProcessImports(ProgramNode program)
        {
            // Extract import statements
            var imports = program.Statements.Where(s => s is ImportStmt).Cast<ImportStmt>().ToList();

            // Process each import
            foreach (var import in imports)
            {
                var packageProgram = LoadPackage(import.PackageName);

                // Handle different import types
                switch (import.Type)
                {
                    case ImportStmt.ImportType.Import:
                        MergeExportedSymbols(program, packageProgram);
                        break;
                    case ImportStmt.ImportType.Reference:
                        MergeExportedSymbols(program, packageProgram);
                        break;
                    case ImportStmt.ImportType.Using:
                        MergeExportedSymbols(program, packageProgram);
                        break;
                }
            }

            // Remove the import statements from the program
            program.Statements.RemoveAll(s => s is ImportStmt);
        }

        // Merge exported symbols from one program into another
        private void MergeExportedSymbols(ProgramNode target, ProgramNode source)
        {
            // Find all statements with exported symbols
            var exportedSymbols = source.Statements.Where(s =>
                (s is FnDecl && ((FnDecl)s).alwaysTrue) ||
                (s is VarDecl && ((VarDecl)s).alwaysTrue) ||
                (s is StructDecl) ||  // Assume structs are always public
                (s is EnumDecl) ||    // Assume enums are always public 
                (s is TraitDecl)      // Assume traits are always public
            ).ToList();

            // Add them to the target program
            target.Statements.InsertRange(0, exportedSymbols);
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
