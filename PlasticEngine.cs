﻿using Plastic.Helpers;
using System.Collections.Concurrent;
using System.Linq.Expressions;
using System.Runtime.InteropServices;
using static Plastic.TypeChecker;

namespace Plastic
{
    public enum TokenType
    {
        EOF, Identifier, Number, String,
        Fn, Let, If, Else, While, For, Return, Interface,
        Plus, Minus, Star, Slash, Percent,
        Equal, EqualEqual, Bang, BangEqual,
        Greater, GreaterEqual, Less, LessEqual,
        LParen, RParen, LBrace, RBrace, Comma, Semicolon, Arrow,
        Colon, True, False, In,
        Struct, Enum, Trait, Impl, Mut, Pub, Self, Match, As, Dot,
        AtSymbol, Import, Reference, Using, DLLImport,
        Try, Catch, Throw, Finally, AndAnd, OrOr, AndOr, 
    }
    public class ImportStmt : Stmt
    {
        public required string PackageName;
        public required ImportType Type;

        public enum ImportType
        {
            Import,
            Reference,
            Using
        }

        public static Dictionary<string, TypeChecker.TypeInfo> _builtinFunctions = new();
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
            {"import", TokenType.Import}, {"reference", TokenType.Reference}, {"using", TokenType.Using}, {"dllimport", TokenType.DLLImport},
            {"try", TokenType.Try},
            {"catch", TokenType.Catch},
            {"throw", TokenType.Throw},
            {"finally", TokenType.Finally}
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
                        while (Peek() != '\n' && !IsAtEnd()) Advance();
                    }
                    else if (Match('*'))
                    {
                        while (!(Peek() == '*' && PeekNext() == '/') && !IsAtEnd())
                        {
                            if (Peek() == '\n') _line++;
                            Advance();
                        }
                        if (!IsAtEnd())
                        {
                            Advance(); // consume '*'
                            Advance(); // consume '/'
                        }
                        else
                        {
                            throw new Exception("Unterminated multi-line comment.");
                        }
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
                case '&':
                    if (Match('&'))
                        AddToken(TokenType.AndAnd);
                    else if (Match('|'))
                        AddToken(TokenType.AndOr);
                    else
                        throw new Exception($"Unexpected char: {c} at line {_line}");
                    break;
                case '|':
                    if (Match('|'))
                        AddToken(TokenType.OrOr);
                    else
                        throw new Exception($"Unexpected char: {c} at line {_line}");
                    break;
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

    public abstract class AstNode { }
    public class ProgramNode : AstNode { public List<Stmt> Statements { get; } = new(); }
    public abstract class Stmt : AstNode { }
    public abstract class Expr : AstNode { }

    public class PlasticException : Exception
    {
        public object? Value { get; }
        public PlasticException(object? value) : base(value?.ToString() ?? "")
        {
            Value = value;
        }
    }


    public class TryStmt : Stmt
    {
        public required List<Stmt> Body;
        public required List<CatchStmt> Catches;
        public List<Stmt>? Finally;
    }

    public class CatchStmt : Stmt
    {
        public required string ExceptionVariable;
        public required List<Stmt> Body;
    }

    public class ThrowStmt : Stmt
    {
        public required Expr Expression;
    }

    public class FnDecl : Stmt
    {
        public required string Name;
        public required List<(string, string)> Params;
        public required string ReturnType;
        public required List<Stmt> Body;
        public bool IsPublic { get; set; } = false;     
        public bool alwaysTrue = true;    
    }
    public class VarDecl : Stmt
    {
        public required string Name;
        public required string Type;
        public required Expr Initializer;
        public bool IsMutable { get; set; } = false;
        public bool IsPublic { get; set; } = false;
        public bool alwaysTrue { get; set; } = true;     
    }

    public class StructDecl : Stmt
    {
        public required string Name;
        public required List<(string, string, bool)> Fields;    
    }

    public class EnumDecl : Stmt
    {
        public required string Name;
        public required List<(string, List<(string, string)>)> Variants;    
    }

    public class TraitDecl : Stmt
    {
        public required string Name;
        public required List<FnDecl> Methods;
    }

    public class ImplDecl : Stmt
    {
        public required string TypeName;
        public required string? TraitName;     
        public required List<FnDecl> Methods;
    }

    public class MatchExpr : Expr
    {
        public required Expr Value;
        public required List<(Expr, Expr)> Arms;   
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
        public required string Var;         // Loop variable name
        public required Expr Start;         // Range start
        public required Expr End;           // Range end
        public required List<Stmt> Body;    // Loop body
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
     
    public class Parser
    {
        private readonly List<Token> _tokens; public int _current = 0;
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
        public List<string> Errors { get; private set; } = new List<string>();

        public ProgramNode Parse()
        {
            _current = 0;      
            var program = new ProgramNode();

            while (!IsAtEnd())
            {
                try
                {
                    var stmt = ParseDeclaration();
                    program.Statements.Add(stmt);
                }
                catch (Exception ex)
                {
                    Synchronize();

                    Errors.Add("Error at line " + _tokens[_current].Line + ": " + ex.Message);

                    return null;
                }
            }

            return program;
        }

        private void Synchronize()
        {
            Advance();

            while (!IsAtEnd())
            {
                if (Previous().Type == TokenType.Semicolon) return;

                switch (Peek().Type)
                {
                    case TokenType.Fn:
                    case TokenType.Let:
                    case TokenType.If:
                    case TokenType.While:
                    case TokenType.For:
                    case TokenType.Return:
                    case TokenType.Struct:
                    case TokenType.Enum:
                    case TokenType.Trait:
                    case TokenType.Impl:
                        return;
                }

                Advance();
            }
        }


        private Stmt ParseDeclaration()
        {
            if (Match(TokenType.DLLImport))
            {
                return ParseDllImport();
            }

            if (Match(TokenType.Import) || Match(TokenType.Reference) || Match(TokenType.Using))
            {
                return ParseImportStatement();
            }

            if (Match(TokenType.Fn)) return ParseFunction("function");
            if (Match(TokenType.Let)) return ParseVarDecl();
            if (Match(TokenType.Struct)) return ParseStructDecl();
            if (Match(TokenType.Enum)) return ParseEnumDecl();
            if (Match(TokenType.Trait)) return ParseTraitDecl();
            if (Match(TokenType.Impl)) return ParseImplDecl();
            if (Match(TokenType.Try)) return ParseTry();
            if (Match(TokenType.Throw)) return ParseThrow();
            return ParseStatement();
        }



        private Stmt ParseTry()
        {
            var body = new List<Stmt>();
            Consume(TokenType.LBrace, "Expect '{' after 'try'");
            body = ParseBlock();

            var catches = new List<CatchStmt>();
            while (Match(TokenType.Catch))
            {
                Consume(TokenType.LParen, "Expect '(' after 'catch'");
                var exceptionVar = Consume(TokenType.Identifier, "Expect exception variable name").Lexeme;
                Consume(TokenType.RParen, "Expect ')' after catch variable");

                Consume(TokenType.LBrace, "Expect '{' after catch declaration");
                var catchBody = ParseBlock();
                catches.Add(new CatchStmt { ExceptionVariable = exceptionVar, Body = catchBody });
            }

            List<Stmt>? finallyBlock = null;
            if (Match(TokenType.Finally))
            {
                Consume(TokenType.LBrace, "Expect '{' after 'finally'");
                finallyBlock = ParseBlock();
            }

            return new TryStmt { Body = body, Catches = catches, Finally = finallyBlock };
        }

        private Stmt ParseThrow()
        {
            var expr = ParseExpression();
            Consume(TokenType.Semicolon, "Expect ';' after throw expression");
            return new ThrowStmt { Expression = expr };
        }
        private Stmt ParseDllImport()
        {
            Consume(TokenType.Identifier, "Expect 'DllImport' after '@'.");
            Consume(TokenType.LParen, "Expect '(' after 'DllImport'.");
            var dllPath = Consume(TokenType.String, "Expect DLL path as a string.").Literal as string;
            Consume(TokenType.RParen, "Expect ')' after DLL path.");

            var fnDecl = ParseFunction("DLL function");
            return new DllImportStmt { DllPath = dllPath, Function = fnDecl };
        }

        private Stmt ParseImportStatement()
        {
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

            string packageName;
            if (Match(TokenType.String))
            {
                packageName = (string)Previous().Literal!;
            }
            else if (Match(TokenType.Identifier))
            {
                packageName = Previous().Lexeme;

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
        private string ParseFunctionType()
        {
            Consume(TokenType.Fn, "Expect 'fn' keyword in function type");
            Consume(TokenType.LParen, "Expect '(' in function type");

            var paramTypes = new List<string>();
            if (!Check(TokenType.RParen))
            {
                do
                {
                    if (Check(TokenType.Identifier))
                    {
                        var paramName = Advance().Lexeme; // get parameter name
                        Consume(TokenType.Colon, "Expect ':' after parameter name in function type");
                        var typeName = Consume(TokenType.Identifier, "Expect parameter type in function type").Lexeme;
                        paramTypes.Add($"{paramName}: {typeName}");
                    }
                    else
                    {
                        var typeName = Consume(TokenType.Identifier, "Expect parameter type in function type").Lexeme;
                        paramTypes.Add(typeName);
                    }
                } while (Match(TokenType.Comma));
            }

            Consume(TokenType.RParen, "Expect ')' in function type");
            Consume(TokenType.Arrow, "Expect '->' in function type");
            var returnType = Consume(TokenType.Identifier, "Expect return type in function type").Lexeme;

            return $"fn({string.Join(", ", paramTypes)}) -> {returnType}";
        }

        private FnDecl ParseFunction(string kind)
        {
            var fn = new FnDecl
            {
                Name = Consume(TokenType.Identifier, $"Expect {kind} name.").Lexeme,
                Params = new(),
                ReturnType = string.Empty,
                Body = new()
            };

            Consume(TokenType.LParen, "Expect '('.");

            if (!Check(TokenType.RParen))
            {
                do
                {
                    var name = Consume(TokenType.Identifier, "Expect param name.").Lexeme;
                    Consume(TokenType.Colon, "Expect ':'.");

                    // Check if the parameter type is a function type
                    string type;
                    if (Check(TokenType.Fn))
                    {
                        type = ParseFunctionType();
                    }
                    else
                    {
                        type = Consume(TokenType.Identifier, "Expect type name.").Lexeme;
                    }

                    fn.Params.Add((name, type));
                } while (Match(TokenType.Comma));
            }
            if (Check(TokenType.Fn))
            {
                Advance(); // Consume 'fn'
                Consume(TokenType.LParen, "Expect '(' after 'fn'.");
                var paramList = new List<(string, string)>();
                if (!Check(TokenType.RParen))
                {
                    do
                    {
                        var paramName = Consume(TokenType.Identifier, "Expect parameter name.").Lexeme;
                        Consume(TokenType.Colon, "Expect ':' after parameter name.");
                        var paramType = Consume(TokenType.Identifier, "Expect parameter type.").Lexeme;
                        paramList.Add((paramName, paramType));
                    } while (Match(TokenType.Comma));
                }
                Consume(TokenType.RParen, "Expect ')' after function parameters.");
                Consume(TokenType.Arrow, "Expect '->' after function parameters.");
                var returnType = Consume(TokenType.Identifier, "Expect return type.").Lexeme;

                // Create and return a FnDecl object instead of a string
                return new FnDecl
                {
                    Name = "functype", // Function type doesn't need a specific name
                    Params = paramList,
                    ReturnType = returnType,
                    Body = new List<Stmt>()
                };
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
            if (Match(TokenType.Let)) return ParseVarDecl();     
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
        private Stmt ParseFor()
        {
            try
            {
                // For statement structure: for <identifier> in <expr>, <expr> { <body> }
                Consume(TokenType.LParen, "Expect '(' after 'for'.");
                var varName = Consume(TokenType.Identifier, "Expect variable name.").Lexeme;

                Consume(TokenType.In, "Expect 'in' after variable name.");
                var start = ParseExpression();

                Consume(TokenType.Comma, "Expect ',' after start expression.");
                var end = ParseExpression();

                Consume(TokenType.RParen, "Expect ')' after range.");

                List<Stmt> body;
                if (Match(TokenType.LBrace))
                {
                    body = ParseBlock();
                }
                else
                {
                    body = new List<Stmt> { ParseStatement() };
                }

                return new ForStmt
                {
                    Var = varName,
                    Start = start,
                    End = end,
                    Body = body
                };
            }
            catch (Exception ex)
            {
                throw new Exception($"Error parsing for loop: {ex.Message}");
            }
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

        private Expr ParseExpression()
        {
            try
            {
                return ParseAssignment();
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Expression error: {ex.Message}");
                Console.WriteLine($"At token: {Peek().Type} {Peek().Lexeme}");

                while (!IsAtEnd() && Peek().Type != TokenType.Semicolon)
                    Advance();

                return new LiteralExpr { Value = "ERROR" };
            }
        }


        private Expr ParseAssignment()
        {
            var expr = ParseLogical(); 

            if (Match(TokenType.Equal))
            {
                var value = ParseAssignment();

                if (expr is VariableExpr ve)
                {
                    var op = Previous();
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
        private Expr ParseLogical()
        {
            var expr = ParseEquality();

            while (Match(TokenType.AndAnd, TokenType.OrOr, TokenType.AndOr))
            {
                var op = Previous();
                var right = ParseEquality(); // Call ParseEquality directly
                expr = new BinaryExpr { Left = expr, Op = op, Right = right };
            }

            return expr;
        }

        private Expr ParseEquality()
        {
            var expr = ParseComparison();

            while (Match(TokenType.BangEqual, TokenType.EqualEqual))
            {
                var op = Previous();
                var right = ParseComparison();
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
            if (Match(TokenType.Import) || Match(TokenType.Reference) || Match(TokenType.Using))
            {
                ImportStmt stmt = (ImportStmt)ParseImportStatement();
                return new LiteralExpr { Value = "" };
            }
            if(Match(TokenType.DLLImport))
            {
                DllImportStmt dllImportStmt = (DllImportStmt)ParseDllImport();
                return new LiteralExpr { Value = "" };
            }
            if (Check(TokenType.Let) || Check(TokenType.Fn) || Check(TokenType.If) || Check(TokenType.While) || Check(TokenType.For) || Check(TokenType.Return))
            {
                throw new Exception($"Unexpected statement keyword '{Peek().Lexeme}' in expression context.");
            }
            throw new Exception("Expect expression.");
        }

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
    public class DllImportStmt : Stmt
    {
        public required string DllPath { get; set; }
        public required FnDecl Function { get; set; }
    }

    public class TypeChecker
    {
        public record TypeInfo(
            string Type,
            bool IsMutable,
            string? Owner,
            bool IsBorrowed,
            bool IsReference,
            int Lifetime);

        private Dictionary<string, TypeInfo> _variables = new();

        private Dictionary<string, (List<(string name, TypeInfo type)> parameters, TypeInfo returnType)> _functions = new();

        private Dictionary<string, (string kind, Dictionary<string, TypeInfo> members)> _userTypes = new();

        private Dictionary<string, List<(string name, TypeInfo returnType, List<(string, TypeInfo)> parameters)>> _traits = new();
        private Dictionary<string, Dictionary<string, List<(string name, TypeInfo returnType, List<(string, TypeInfo)> parameters)>>> _implementations = new();

        private Stack<HashSet<string>> _movedVariables = new();

        private Dictionary<(string, string), bool> _typeCompatibilityCache = new();

        private int _currentLifetime = 0;

        private string _currentFunction = string.Empty;
        private TypeInfo? _currentReturnType = null;

        private static readonly TypeInfo _boolType = new("bool", false, null, false, false, 0);
        private static readonly TypeInfo _i32Type = new("i32", false, null, false, false, 0);
        private static readonly TypeInfo _f64Type = new("f64", false, null, false, false, 0);
        private static readonly TypeInfo _stringType = new("string", false, null, false, false, 0);
        private static readonly TypeInfo _anyType = new("any", false, null, false, false, 0);
        private static readonly TypeInfo _charType = new("char", false, null, false, false, 0);
        private static readonly TypeInfo _arrayType = new("array", false, null, false, false, 0);
        private static readonly TypeInfo _listType = new("list", false, null, false, false, 0);
        private static readonly TypeInfo _mapType = new("map", false, null, false, false, 0);

        public void Check(ProgramNode program)
        {
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
                CollectUserTypes(program);

                CollectTraits(program);

                CollectFunctionSignatures(program);

                VerifyTraitImplementations();

                foreach (var stmt in program.Statements)
                {
                    CheckStatement(stmt, new HashSet<string>());
                }
            }
            catch (Exception)
            {
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
                            var variantType = $"{ed.Name}::{variantName}";

                            int fieldIndex = 0;
                            foreach (var (fieldName, fieldType) in fields)
                            {
                                if (!IsValidType(fieldType) && !_userTypes.ContainsKey(fieldType))
                                {
                                    throw new Exception($"Unknown type '{fieldType}' for field '{fieldName}' in enum variant '{variantName}'");
                                }

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
                            var isMutable = false;          
                            var isReference = false;     

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
                        else
                        {
                            if (!_userTypes.ContainsKey(impl.TypeName))
                            {
                                throw new Exception($"Implementing methods for unknown type '{impl.TypeName}'");
                            }

                            foreach (var method in impl.Methods)
                            {
                                parameters = new List<(string name, TypeInfo type)>();

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
                    var implementedMethods = methods.Select(m => m.name).ToHashSet();
                    foreach (var traitMethod in traitMethods)
                    {
                        if (!implementedMethods.Contains(traitMethod.name))
                        {
                            throw new Exception($"Type '{typeName}' does not implement required trait method '{traitMethod.name}' from trait '{traitName}'");
                        }

                        var impl = methods.First(m => m.name == traitMethod.name);

                        if (!AreTypesCompatible(impl.returnType.Type, traitMethod.returnType.Type))
                        {
                            throw new Exception($"Return type mismatch in implementation of '{traitMethod.name}' for trait '{traitName}': expected '{traitMethod.returnType.Type}', got '{impl.returnType.Type}'");
                        }

                        if (impl.parameters.Count != traitMethod.parameters.Count)
                        {
                            throw new Exception($"Parameter count mismatch in implementation of '{traitMethod.name}' for trait '{traitName}': expected {traitMethod.parameters.Count}, got {impl.parameters.Count}");
                        }

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

        private void CheckStatement(Stmt stmt, HashSet<string> movedVars)
        {
            switch (stmt)
            {
                case FnDecl fn:
                    CheckFunction(fn);
                    break;

                case VarDecl vd:
                    var exprType = GetExpressionType(vd.Initializer, movedVars);

                    if (exprType.Type != vd.Type && !(IsNumericType(vd.Type) && IsNumericType(exprType.Type)))
                    {
                        throw new Exception($"Type mismatch in variable declaration: expected {vd.Type}, got {exprType.Type}");
                    }

                    string? owner = null;
                    if (vd.Initializer is VariableExpr ve && _variables.TryGetValue(ve.Name, out var sourceVar) && !sourceVar.IsReference)
                    {
                        owner = $"var:{vd.Name}";
                        movedVars.Add(ve.Name);
                    }
                    else
                    {
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

                        foreach (var moved in thenMoved.Intersect(elseMoved))
                        {
                            movedVars.Add(moved);
                        }
                    }
                    else
                    {
                    }
                    break;

                case WhileStmt ws:
                    var whileCondType = GetExpressionType(ws.Condition, movedVars);
                    if (whileCondType.Type != "bool")
                    {
                        throw new Exception("Condition must be a boolean expression");
                    }

                    _movedVariables.Push(new HashSet<string>());

                    foreach (var s in ws.Body)
                    {
                        CheckStatement(s, _movedVariables.Peek());
                    }

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

                    _variables[fs.Var] = new TypeInfo("i32", false, null, false, false, _currentLifetime);

                    var loopMoved = new HashSet<string>();
                    foreach (var s in fs.Body)
                    {
                        CheckStatement(s, loopMoved);
                    }

                    foreach (var moved in loopMoved)
                    {
                        movedVars.Add(moved);
                    }

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
                    _currentLifetime++;
                    var outerLifetime = _currentLifetime;

                    var localMovedVars = new HashSet<string>(movedVars);

                    foreach (var s in bs.Statements)
                    {
                        CheckStatement(s, localMovedVars);
                    }

                    foreach (var moved in localMovedVars)
                    {
                        movedVars.Add(moved);
                    }

                    ValidateNoEscapingReferences(outerLifetime);
                    _currentLifetime--;
                    break;

                case StructDecl _:
                case EnumDecl _:
                case TraitDecl _:
                case ImplDecl _:
                    break;

                default:
                    throw new Exception($"Unsupported statement type: {stmt.GetType().Name}");
            }
        }

        private void ValidateNoEscapingReferences(int lifetime)
        {
            foreach (var (name, info) in _variables)
            {
                if (info.IsReference && info.Lifetime == lifetime)
                {
                    Console.WriteLine($"Warning: Reference in '{name}' might outlive its scope.");
                }
            }
        }

        private void CheckFunction(FnDecl fn)
        {
            var oldFunction = _currentFunction;
            var oldReturnType = _currentReturnType;
            var oldLifetime = _currentLifetime;

            _currentFunction = fn.Name;
            _currentReturnType = new TypeInfo(fn.ReturnType, false, null, false, false, 0);
            _currentLifetime++;

            var movedVars = new HashSet<string>();

            var outerVariables = new Dictionary<string, TypeInfo>(_variables);
            _variables.Clear();

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

            foreach (var stmt in fn.Body)
            {
                CheckStatement(stmt, movedVars);
            }

            if (fn.ReturnType != "void" && !HasReturnOnAllPaths(fn.Body))
            {
                throw new Exception($"Function '{fn.Name}' might not return a value on all code paths");
            }

            _variables = outerVariables;
            _currentFunction = oldFunction;
            _currentReturnType = oldReturnType;
            _currentLifetime = oldLifetime;
        }

        private bool HasReturnOnAllPaths(List<Stmt> statements)
        {
            if (statements.Count == 0)
            {
                return false;
            }

            if (statements[statements.Count - 1] is ReturnStmt)
            {
                return true;
            }

            if (statements[statements.Count - 1] is IfStmt ifStmt)
            {
                bool thenReturns = HasReturnOnAllPaths(ifStmt.ThenBranch);
                bool elseReturns = ifStmt.ElseBranch != null && HasReturnOnAllPaths(ifStmt.ElseBranch);

                return thenReturns && elseReturns;
            }

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
                        int => _i32Type,
                        long => new TypeInfo("i64", false, null, false, false, 0),
                        double or float => _f64Type,
                        bool => _boolType,
                        string => _stringType,
                        char => _charType,
                        object[] => _arrayType,
                        List<object> => _listType,
                        Dictionary<string, object> => _mapType,
                        _ => _anyType
                    };

                case VariableExpr ve:
                    if (_variables.TryGetValue(ve.Name, out var varInfo))
                    {
                        if (movedVars.Contains(ve.Name))
                        {
                            throw new Exception($"Use of moved variable '{ve.Name}'");
                        }

                        return varInfo;
                    }
                    throw new Exception($"Undefined variable: {ve.Name}");

                case BinaryExpr be:
                    var leftType = GetExpressionType(be.Left, movedVars);

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
                            return _boolType;

                        case TokenType.AndAnd:
                            return _boolType;
                        case TokenType.AndOr:
                            return _boolType;
                        case TokenType.OrOr:
                            return _boolType;

                        case TokenType.Equal:
                            if (!AreTypesCompatible(leftType.Type, rightType.Type))
                            {
                                throw new Exception($"Cannot assign {rightType.Type} to {leftType.Type}");
                            }

                            if (be.Right is VariableExpr ve2 && !rightType.IsReference)
                            {
                                movedVars.Add(ve2.Name);

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
                            throw new Exception($"Unsupported binary operator: {be.Op.Lexeme} @ get");
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
                            if (ce.Arguments.Count != fnInfo.parameters.Count)
                            {
                                throw new Exception($"Function {callee.Name} expects {fnInfo.parameters.Count} arguments but got {ce.Arguments.Count}");
                            }

                            for (int i = 0; i < ce.Arguments.Count; i++)
                            {
                                var argType = GetExpressionType(ce.Arguments[i], movedVars);
                                var paramInfo = fnInfo.parameters[i].type;

                                if (!AreTypesCompatible(argType.Type, paramInfo.Type))
                                {
                                    throw new Exception($"Argument {i + 1} type mismatch: expected {paramInfo.Type}, got {argType.Type}");
                                }

                                if (ce.Arguments[i] is VariableExpr argVar && paramInfo.IsReference == false)
                                {
                                    movedVars.Add(argVar.Name);
                                }
                            }

                            return fnInfo.returnType;
                        }

                        var builtinType = GetBuiltinFunctionType(callee.Name);
                        if (builtinType != null)
                            return builtinType;


                        throw new Exception($"Undefined function: {callee.Name}");
                    }
                    else if (ce.Callee is FieldAccessExpr methodAccess)
                    {
                        var methodName = $"{GetExpressionType(methodAccess.Object, movedVars).Type}::{methodAccess.FieldName}";

                        if (_functions.TryGetValue(methodName, out var methodInfo))
                        {
                            if (ce.Arguments.Count != methodInfo.parameters.Count - 1)
                            {
                                throw new Exception($"Method {methodName} expects {methodInfo.parameters.Count - 1} arguments but got {ce.Arguments.Count}");
                            }

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

                    if (ma.Arms.Count == 0)
                    {
                        throw new Exception("Match expression must have at least one arm");
                    }

                    TypeInfo resultType = null;
                    foreach (var (pattern, result) in ma.Arms)
                    {
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
                    throw new Exception($"Unsupported expression type: {expr.GetType().Name} @ get");
            }
        }
        private TypeInfo? GetBuiltinFunctionType(string name)
        {
            if (ImportStmt._builtinFunctions.TryGetValue(name, out var type))
                return type;
            return null;
        }
        private bool IsValidType(string type)
        {
            if (type == "i32" || type == "f64" || type == "bool" || type == "string" ||
                type == "void" || type == "i64" || type == "f32" || type == "any" ||
                type == "char" || type == "array" || type == "list" || type == "map" ||
                type.StartsWith("List<") || type.StartsWith("Map<") ||
                type.StartsWith("&mut ") || type.StartsWith("&") ||
                (type.StartsWith("fn(") && type.Contains("->")))
            {
                return true;
            }

            return _userTypes.ContainsKey(type);
        }

        private bool IsNumericType(string type)
        {
            return type == "i32" || type == "f64" || type == "i64" || type == "f32";
        }

        private TypeInfo CombineNumericTypes(string type1, string type2)
        {
            // Promote to the highest precision type
            if (type1 == "f64" || type2 == "f64") return _f64Type;
            if (type1 == "f32" || type2 == "f32") return new TypeInfo("f32", false, null, false, false, 0);
            if (type1 == "i64" || type2 == "i64") return new TypeInfo("i64", false, null, false, false, 0);
            return _i32Type;
        }

        private bool AreTypesCompatible(string sourceType, string targetType)
        {
            var key = (sourceType, targetType);
            if (_typeCompatibilityCache.TryGetValue(key, out bool result))
            {
                return result;
            }

            if (sourceType == targetType)
            {
                _typeCompatibilityCache[key] = true;
                return true;
            }

            if (IsNumericType(sourceType) && IsNumericType(targetType))
            {
                _typeCompatibilityCache[key] = true;
                return true;
            }

            if (sourceType.StartsWith("&") && targetType.StartsWith("&"))
            {
                var sourceBare = sourceType.StartsWith("&mut ") ? sourceType.Substring(5) : sourceType.Substring(1);
                var targetBare = targetType.StartsWith("&mut ") ? targetType.Substring(5) : targetType.Substring(1);

                bool compatible = AreTypesCompatible(sourceBare, targetBare);

                if (compatible && !sourceType.StartsWith("&mut ") && targetType.StartsWith("&mut "))
                {
                    compatible = false;
                }

                _typeCompatibilityCache[key] = compatible;
                return compatible;
            }

            _typeCompatibilityCache[key] = false;
            return false;
        }

        public int getLine()
        {
            return 0;
        }
    }


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

    public class Interpreter
    {
        private readonly Environment _globals = new();
        private Environment _environment;

        public Interpreter(Dictionary<string, object> globals)
        {
            _environment = _globals;

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
                }

                try
                {
                    var updateFn = _globals.Get("update");
                    if (updateFn != null)
                    {
                        while (true)
                        {
                            result = CallFunction(updateFn as FnDecl, new List<object?>());
                            System.Threading.Thread.Sleep(16);   
                        }
                    }
                }
                catch
                {
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

                case ThrowStmt ts:
                    var throwValue = Evaluate(ts.Expression);
                    throw new PlasticException(throwValue);

                case TryStmt tryStmt:
                    try
                    {
                        ExecuteBlock(tryStmt.Body, new Environment(_environment));
                    }
                    catch (Exception ex)
                    {
                        bool handled = false;
                        foreach (var catch_ in tryStmt.Catches)
                        {
                            var catchEnv = new Environment(_environment);
                            catchEnv.Define(catch_.ExceptionVariable, ex.Message);
                            try
                            {
                                ExecuteBlock(catch_.Body, catchEnv);
                                handled = true;
                                break;
                            }
                            catch
                            {
                                // If this catch block throws, try next catch block
                                continue;
                            }
                        }
                        if (!handled) throw;
                    }
                    finally
                    {
                        if (tryStmt.Finally != null)
                        {
                            ExecuteBlock(tryStmt.Finally, new Environment(_environment));
                        }
                    }
                    return null;

                case DllImportStmt dllImport:
                    RegisterDllFunction(dllImport);
                    return null;

                case WhileStmt ws:
                    while (IsTruthy(Evaluate(ws.Condition)))
                    {
                        var whileEnv = new Environment(_environment);
                        ExecuteBlock(ws.Body, whileEnv);
                    }
                    return null;

                case ForStmt fs:
                    try
                    {
                        var start = Convert.ToInt32(Evaluate(fs.Start));
                        var end = Convert.ToInt32(Evaluate(fs.End));
                        var buffer = new StringBuilder(); 

                        for (int i = start; i <= end; i++)
                        {
                            _environment.Define(fs.Var, i);
                            foreach (var stmt2 in fs.Body)
                            {
                                if (stmt2 is ExprStmt exprStmt && exprStmt.Expression is CallExpr callExpr &&
                                    callExpr.Callee is VariableExpr callee && callee.Name == "print")
                                {
                                    var argument = Evaluate(callExpr.Arguments[0]);
                                    buffer.AppendLine(argument?.ToString());
                                }
                                else
                                {
                                    ExecuteStmt(stmt2);
                                }
                            }
                        }

                        if (buffer.Length > 0)
                        {
                            Console.Write(buffer.ToString());
                        }

                        return null;
                    }
                    catch (Exception ex)
                    {
                        throw new Exception($"Error executing for loop: {ex.Message}");
                    }


                case BlockStmt bs:
                    return ExecuteBlock(bs.Statements, new Environment(_environment));

                default:
                    throw new Exception($"Unsupported statement type: {stmt.GetType().Name}");
            }
        }
        private void RegisterDllFunction(DllImportStmt dllImport)
        {
            var dllPath = dllImport.DllPath;
            var functionName = dllImport.Function.Name;
            var returnType = dllImport.Function.ReturnType;
            var parameters = dllImport.Function.Params;

            var delegateType = CreateDelegateType(returnType, parameters);

            var handle = NativeLibrary.Load(dllPath);
            var functionPointer = NativeLibrary.GetExport(handle, functionName);
            var functionDelegate = Marshal.GetDelegateForFunctionPointer(functionPointer, delegateType);

            _environment.Define(functionName, functionDelegate);
        }

        private Type CreateDelegateType(string returnType, List<(string, string)> parameters)
        {
            var parameterTypes = parameters.Select(p => GetClrType(p.Item2)).ToArray();
            var returnClrType = GetClrType(returnType);

            return Expression.GetDelegateType(parameterTypes.Concat(new[] { returnClrType }).ToArray());
        }
        private Type GetClrType(string plasticType)
        {
            return plasticType switch
            {
                "void" => typeof(void),
                "i32" => typeof(int),
                "f64" => typeof(double),
                "string" => typeof(string),
                _ => throw new Exception($"Unsupported type: {plasticType}")
            };
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
                    if (lit.Value is string s && s.Length == 1 &&
                        lit.Value.ToString().StartsWith("'") && lit.Value.ToString().EndsWith("'"))
                    {
                        return s[0];
                    }
                    else if (lit.Value is string arrayStr &&
                            arrayStr.StartsWith("[") && arrayStr.EndsWith("]"))
                    {
                        var content = arrayStr.Substring(1, arrayStr.Length - 2).Trim();
                        if (string.IsNullOrWhiteSpace(content))
                            return new object[0];

                        var elements = content.Split(',')
                            .Select(e => Evaluate(new LiteralExpr { Value = e.Trim() }))
                            .ToArray();
                        return elements;
                    }
                    else if (lit.Value is string listStr &&
                            listStr.StartsWith("list[") && listStr.EndsWith("]"))
                    {
                        var content = listStr.Substring(5, listStr.Length - 6).Trim();
                        if (string.IsNullOrWhiteSpace(content))
                            return new List<object>();

                        var elements = content.Split(',')
                            .Select(e => Evaluate(new LiteralExpr { Value = e.Trim() }))
                            .ToList();
                        return elements;
                    }
                    else if (lit.Value is string mapStr &&
                            mapStr.StartsWith("{") && mapStr.EndsWith("}"))
                    {
                        var content = mapStr.Substring(1, mapStr.Length - 2).Trim();
                        if (string.IsNullOrWhiteSpace(content))
                            return new Dictionary<string, object>();

                        var map = new Dictionary<string, object>();
                        var pairs = content.Split(',');
                        foreach (var pair in pairs)
                        {
                            var keyValue = pair.Split(':');
                            if (keyValue.Length == 2)
                            {
                                var key = keyValue[0].Trim();
                                if (key.StartsWith("\"") && key.EndsWith("\""))
                                    key = key.Substring(1, key.Length - 2);

                                var value = Evaluate(new LiteralExpr { Value = keyValue[1].Trim() });
                                map[key] = value;
                            }
                        }
                        return map;
                    }
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
                // standard plus
                TokenType.Plus => (left, right) switch
                {
                    (double l, double r) => l + r,
                    (string l, string r) => l + r,
                    (string l, _) => l + right?.ToString(),
                    (_, string r) => left?.ToString() + r,
                    _ => throw new Exception("Operands must be two numbers or two strings.")
                },

                // alias “added” → plus
                TokenType.Identifier when op.Lexeme == "added" => (left, right) switch
                {
                    (double l, double r) => l + r,
                    _ => throw new Exception("‘added’ operator only works on numbers.")
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
                TokenType.AndAnd => IsTruthy(left) && IsTruthy(right),
                TokenType.OrOr => IsTruthy(left) || IsTruthy(right),
                TokenType.AndOr => IsTruthy(left) && !IsTruthy(right),

                // comma operator returns left (so “for(i in 0,5)” can parse 0,5 without error)
                TokenType.Comma => left,

                _ => throw new Exception($"Unsupported binary operator: {op.Lexeme}")
            };
        }

        public object? CallFunction(FnDecl function, List<object?> arguments)
        {
            var environment = new Environment(_globals);

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
    }

    public class ReturnException : Exception
    {
        public object? Value { get; }
        public ReturnException(object? value) { Value = value; }
    }

    public class PlasticEngine
    {
        private readonly Dictionary<string, object> _globals = new();
        private readonly Dictionary<string, ProgramNode> _packages = new();
        private readonly Dictionary<string, string> _packagePaths = new();
        public bool shouldLeave = false;
        public Interpreter terpreter;
        public TypeChecker checker;
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

        public void RegisterPackagePath(string name, string filePath)      
        {
            _packagePaths[name] = filePath;
        }
        private ProgramNode LoadPackage(string packageName)
        {
            if (_packages.TryGetValue(packageName, out var program))
            {
                return program;
            }

            if (!_packagePaths.TryGetValue(packageName, out var path))
            {
                path = ResolvePackagePath(packageName);
                if (path == null)
                {
                    throw new Exception($"Could not find package '{packageName}'");
                }
            }

            string source = File.ReadAllText(path);
            var lexer = new Lexer(source);
            var tokens = lexer.ScanTokens();
            var parser = new Parser(tokens);
            var packageProgram = parser.Parse();

            _packages[packageName] = packageProgram;

            return packageProgram;
        }
        private string ResolvePackagePath(string packageName)
        {
            if (packageName.Contains(Path.DirectorySeparatorChar) || packageName.Contains('/') || packageName.Contains("-="))
            {
                var basePath = System.Environment.CurrentDirectory;
                var fullPath = Path.Combine(basePath, packageName);

                if (Path.HasExtension(fullPath) && File.Exists(fullPath))
                {
                    return fullPath;
                }
                else
                {
                    var plPath = fullPath.EndsWith(".pl", StringComparison.OrdinalIgnoreCase)
                        ? fullPath
                        : fullPath + ".pl";
                    if (File.Exists(plPath))
                    {
                        return plPath;
                    }

                    var plasticPath = fullPath.EndsWith(".plastic", StringComparison.OrdinalIgnoreCase)
                        ? fullPath
                        : fullPath + ".plastic";
                    if (File.Exists(plasticPath))
                    {
                        return plasticPath;
                    }
                }
            }

            string searchPath = "";
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

            var packagePath = packageName.Replace('.', Path.DirectorySeparatorChar);
            var combinedPath = Path.Combine(searchPath, $"{packagePath}.pl");
            if (File.Exists(combinedPath))
            {
                return combinedPath;
            }

            combinedPath = Path.Combine(searchPath, $"{packagePath}.plastic");
            if (File.Exists(combinedPath))
            {
                return combinedPath;
            }

            return null;
        }

        // kinda messy, mb
        BlockingCollection<string> channel = new BlockingCollection<string>();
        private static readonly BlockingCollection<object> _printOpQueue = new();
        private static Thread? _printOpThread;
        private static bool _printOpThreadRunning = false;

        string _lastHttpRequestData = "";
        string _lastHttpRequestMethod = "";
        string _lastHttpRequestPath = "";
        string _lastHttpRequestResponse = "";
        System.Net.HttpListenerContext? _lastHttpContext = null;
        object _httpLock = new();

        private static void StartPrintOpThread()
        {
            if (_printOpThreadRunning) return;
            _printOpThreadRunning = true;
            _printOpThread = new Thread(() =>
            {
                foreach (var o in _printOpQueue.GetConsumingEnumerable())
                {
                    if (o is IEnumerable<object> enumerable)
                    {
                        Console.WriteLine(string.Join(" ", enumerable));
                    }
                    else
                    {
                        Console.WriteLine(o);
                    }
                }
            })
            {
                IsBackground = true,
                Name = "print thread"
            };
            _printOpThread.Start();
        }

        private void RegisterBuiltins()
        {
            WorkingCreateGlobal("print", new Action<object>(o =>
            {
                if (o is IEnumerable<object> enumerable)
                {
                    Console.WriteLine(string.Join(" ", enumerable));
                }
                else
                {
                    Console.WriteLine(o);
                }
            }), "void");
            WorkingCreateGlobal("print_op", new Action<object>(o =>
            {
                StartPrintOpThread();
                _printOpQueue.Add(o);
            }), "void");
            WorkingCreateGlobal("len", new Func<string, int>(s => s.Length), "i32");
            WorkingCreateGlobal("range", new Func<double, double, object>((start, end) => new { Start = start, End = end }), "range");
            WorkingCreateGlobal("sleep", new Action<double>(ms => System.Threading.Thread.Sleep((int)ms)), "void");
            WorkingCreateGlobal("input", new Func<string?>(() => Console.ReadLine()), "string");
            WorkingCreateGlobal("parseInt", new Func<string, double>(s =>
            {
                if (double.TryParse(s, out var result)) return result;
                throw new Exception($"Cannot parse '{s}' as number.");
            }), "f64");
            WorkingCreateGlobal("toString", new Func<object, string>(o => o.ToString()), "string");
            WorkingCreateGlobal("exit", new Action(() =>
            {
                shouldLeave = true;
            }), "void");

            WorkingCreateGlobal("readFile", new Func<string, string>(path =>
            {
                if (File.Exists(path))
                {
                    return File.ReadAllText(path);
                }
                throw new Exception($"File '{path}' not found.");
            }), "string");
            WorkingCreateGlobal("writeFile", new Action<string, string>((path, content) =>
            {
                File.WriteAllText(path, content);
            }), "void");
            WorkingCreateGlobal("appendFile", new Action<string, string>((path, content) =>
            {
                File.AppendAllText(path, content);
            }), "void");
            WorkingCreateGlobal("deleteFile", new Action<string>(path =>
            {
                if (File.Exists(path))
                {
                    File.Delete(path);
                }
                else
                {
                    throw new Exception($"File '{path}' not found.");
                }
            }), "void");
            WorkingCreateGlobal("exists", new Func<string, bool>(path => File.Exists(path)), "bool");
            WorkingCreateGlobal("mkdir", new Action<string>(path =>
            {
                if (!Directory.Exists(path))
                {
                    Directory.CreateDirectory(path);
                }
            }), "void");
            WorkingCreateGlobal("rmdir", new Action<string>(path =>
            {
                if (Directory.Exists(path))
                {
                    Directory.Delete(path, true);
                }
                else
                {
                    throw new Exception($"Directory '{path}' not found.");
                }
            }), "void");
            WorkingCreateGlobal("listDir", new Func<string, List<string>>(path =>
            {
                if (Directory.Exists(path))
                {
                    return Directory.GetFiles(path).ToList();
                }
                throw new Exception($"Directory '{path}' not found.");
            }), "List<string>");
            WorkingCreateGlobal("copyFile", new Action<string, string>((source, destination) =>
            {
                if (File.Exists(source))
                {
                    File.Copy(source, destination);
                }
                else
                {
                    throw new Exception($"File '{source}' not found.");
                }
            }), "void");
            WorkingCreateGlobal("moveFile", new Action<string, string>((source, destination) =>
            {
                if (File.Exists(source))
                {
                    File.Move(source, destination);
                }
                else
                {
                    throw new Exception($"File '{source}' not found.");
                }
            }), "void");
            WorkingCreateGlobal("renameFile", new Action<string, string>((oldName, newName) =>
            {
                if (File.Exists(oldName))
                {
                    File.Move(oldName, newName);
                }
                else
                {
                    throw new Exception($"File '{oldName}' not found.");
                }
            }), "void");
            WorkingCreateGlobal("showErrorBox", new Action<string>((message) =>
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.WriteLine("\n--- ERROR ---");
                Console.WriteLine(message);
                Console.ResetColor();
                Console.WriteLine("Press any key to continue...");
                Console.ReadKey(true);
            }), "void");
            WorkingCreateGlobal("showConfirmBox", new Func<string, string, bool>((message, title) =>
            {
                Console.WriteLine($"\n--- {title} ---");
                Console.WriteLine(message);
                Console.Write("Enter Y for Yes or N for No: ");
                string y = Console.ReadLine();
                return y.ToUpper() == "Y";
            }), "bool");
            WorkingCreateGlobal("openFile", new Func<string, bool>((fileName) =>
            {
                try
                {
                    var process = new System.Diagnostics.Process();
                    process.StartInfo.FileName = fileName;
                    process.StartInfo.UseShellExecute = true;
                    return process.Start();
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"Failed to open file: {ex.Message}");
                    return false;
                }
            }), "bool");
            WorkingCreateGlobal("runProcess", new Func<string, string, bool>((fileName, args) =>
            {
                try
                {
                    var process = new System.Diagnostics.Process();
                    process.StartInfo.FileName = fileName;
                    process.StartInfo.Arguments = args;
                    return process.Start();
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"Failed to run process: {ex.Message}");
                    return false;
                }
            }), "bool");
            WorkingCreateGlobal("runProcessAndWait", new Func<string, string, int>((fileName, args) =>
            {
                try
                {
                    var process = new System.Diagnostics.Process();
                    process.StartInfo.FileName = fileName;
                    process.StartInfo.Arguments = args;
                    process.Start();
                    process.WaitForExit();
                    return process.ExitCode;
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"Failed to run process: {ex.Message}");
                    return -1;
                }
            }), "i32");
            WorkingCreateGlobal("openFileDialog", new Func<string, string, string>((title, filter) =>
            {
                Console.WriteLine($"\n--- {title} ---");
                Console.WriteLine($"File filter: {filter}");
                Console.Write("Please enter the full path to the file: ");
                return Console.ReadLine() ?? string.Empty;
            }), "string");
            WorkingCreateGlobal("saveFileDialog", new Func<string, string, string>((title, filter) =>
            {
                Console.WriteLine($"\n--- {title} ---");
                Console.WriteLine($"File filter: {filter}");
                Console.Write("Please enter the full path where to save the file: ");
                return Console.ReadLine() ?? string.Empty;
            }), "string");
            WorkingCreateGlobal("breakPoint", new Action(() => Console.WriteLine($"breakpoint hit! Line: {checker.getLine()}")), "void");
            WorkingCreateGlobal("now", new Func<string>(() => DateTime.Now.ToString("o")), "string");
            WorkingCreateGlobal("timestamp", new Func<long>(() => DateTimeOffset.UtcNow.ToUnixTimeSeconds()), "i64");
            WorkingCreateGlobal("formatDate", new Func<string, string, string>((format, culture) =>
            {
                return DateTime.Now.ToString(format, new System.Globalization.CultureInfo(culture));
            }), "string");
            WorkingCreateGlobal("abs", new Func<double, double>(Math.Abs), "f64");
            WorkingCreateGlobal("floor", new Func<double, double>(Math.Floor), "f64");
            WorkingCreateGlobal("ceil", new Func<double, double>(Math.Ceiling), "f64");
            WorkingCreateGlobal("round", new Func<double, double>(Math.Round), "f64");
            WorkingCreateGlobal("sqrt", new Func<double, double>(Math.Sqrt), "f64");
            WorkingCreateGlobal("pow", new Func<double, double, double>(Math.Pow), "f64");
            WorkingCreateGlobal("max", new Func<double, double, double>(Math.Max), "f64");
            WorkingCreateGlobal("min", new Func<double, double, double>(Math.Min), "f64");
            WorkingCreateGlobal("clamp", new Func<double, double, double, double>((val, min, max) => Math.Max(min, Math.Min(max, val))), "f64");
            WorkingCreateGlobal("toUpper", new Func<string, string>(s => s.ToUpperInvariant()), "string");
            WorkingCreateGlobal("toLower", new Func<string, string>(s => s.ToLowerInvariant()), "string");
            WorkingCreateGlobal("trim", new Func<string, string>(s => s.Trim()), "string");
            WorkingCreateGlobal("startsWith", new Func<string, string, bool>((s, prefix) => s.StartsWith(prefix)), "bool");
            WorkingCreateGlobal("endsWith", new Func<string, string, bool>((s, suffix) => s.EndsWith(suffix)), "bool");
            WorkingCreateGlobal("contains", new Func<string, string, bool>((s, substr) => s.Contains(substr)), "bool");
            WorkingCreateGlobal("split", new Func<string, string, List<string>>((s, sep) => s.Split(new[] { sep }, StringSplitOptions.None).ToList()), "List<string>");
            WorkingCreateGlobal("join", new Func<List<string>, string, string>((list, sep) => string.Join(sep, list)), "string");
            WorkingCreateGlobal("isNull", new Func<object, bool>(o => o == null), "bool");
            WorkingCreateGlobal("isNumber", new Func<object, bool>(o => double.TryParse(o?.ToString(), out _)), "bool");
            WorkingCreateGlobal("isString", new Func<object, bool>(o => o is string), "bool");
            WorkingCreateGlobal("toInt", new Func<object, int>(o => Convert.ToInt32(o)), "i32");
            WorkingCreateGlobal("toFloat", new Func<object, double>(o => Convert.ToDouble(o)), "f64");
            WorkingCreateGlobal("uuid", new Func<string>(() => Guid.NewGuid().ToString()), "string");
            WorkingCreateGlobal("env", new Func<string, string>(key => System.Environment.GetEnvironmentVariable(key) ?? string.Empty), "string");
            WorkingCreateGlobal("log", new Action<object>(o => Console.WriteLine($"[LOG] {o}")), "void");
            WorkingCreateGlobal("error", new Action<object>(o =>
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.WriteLine($"[ERROR] {o}");
                Console.ResetColor();
            }), "void");
            WorkingCreateGlobal("command", new Func<string, string, int>((command, args) =>
            {
                const int pollIntervalMs = 50;
                try
                {
                    var process = new System.Diagnostics.Process();
                    process.StartInfo.RedirectStandardOutput = true;
                    process.StartInfo.RedirectStandardError = true;
                    process.StartInfo.UseShellExecute = false;
                    process.StartInfo.CreateNoWindow = true;

                    if (OperatingSystem.IsWindows())
                    {
                        process.StartInfo.FileName = "cmd.exe";
                        process.StartInfo.Arguments = $"/c {command} {args}";
                    }
                    else if (OperatingSystem.IsLinux() || OperatingSystem.IsMacOS())
                    {
                        process.StartInfo.FileName = "/bin/bash";
                        process.StartInfo.Arguments = $"-c \"{command.Replace("\"", "\\\"")} {args.Replace("\"", "\\\"")}\"";
                    }
                    else
                    {
                        throw new PlatformNotSupportedException("Unsupported OS for 'command' builtin, please file an issue.");
                    }

                    process.OutputDataReceived += (sender, e) => {
                        if (e.Data != null) Console.WriteLine(e.Data);
                    };
                    process.ErrorDataReceived += (sender, e) => {
                        if (e.Data != null) Console.Error.WriteLine(e.Data);
                    };

                    process.Start();
                    process.BeginOutputReadLine();
                    process.BeginErrorReadLine();

                    while (!process.HasExited)
                    {
                        System.Threading.Thread.Sleep(pollIntervalMs);
                    }

                    return process.ExitCode;
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"Failed to run command: {ex.Message}");
                    return -1;
                }
            }), "i32");
            WorkingCreateGlobal("curl", new Func<string, string, string, string, string>((url, method, data, headers) =>
            {
                try
                {
                    using var client = new System.Net.Http.HttpClient();
                    System.Net.Http.HttpRequestMessage request = new System.Net.Http.HttpRequestMessage(
                        new System.Net.Http.HttpMethod(method.ToUpperInvariant()), url);

                    if (!string.IsNullOrWhiteSpace(headers))
                    {
                        foreach (var line in headers.Split('\n'))
                        {
                            var idx = line.IndexOf(':');
                            if (idx > 0)
                            {
                                var key = line.Substring(0, idx).Trim();
                                var value = line.Substring(idx + 1).Trim();
                                if (!string.IsNullOrEmpty(key))
                                    request.Headers.TryAddWithoutValidation(key, value);
                            }
                        }
                    }

                    if (string.Equals(method, "POST", StringComparison.OrdinalIgnoreCase) ||
                        string.Equals(method, "PUT", StringComparison.OrdinalIgnoreCase) ||
                        string.Equals(method, "PATCH", StringComparison.OrdinalIgnoreCase))
                    {
                        request.Content = new StringContent(
                            data ?? "",
                            System.Text.Encoding.UTF8,
                            "application/json"
                        );
                    }
                    else if (!string.IsNullOrEmpty(data) && (string.Equals(method, "GET", StringComparison.OrdinalIgnoreCase) || string.Equals(method, "DELETE", StringComparison.OrdinalIgnoreCase)))
                    {
                        var separator = url.Contains("?") ? "&" : "?";
                        request.RequestUri = new Uri(url + separator + data);
                    }

                    var response = client.SendAsync(request).GetAwaiter().GetResult();
                    response.EnsureSuccessStatusCode();
                    return response.Content.ReadAsStringAsync().GetAwaiter().GetResult();
                }
                catch (Exception ex)
                {
                    return $"curl error: {ex.Message}";
                }
            }), "string");
            WorkingCreateGlobal("LoadString", new Func<string, string>((source) =>
            {
                var engine = new PlasticEngine();
                return engine.Evaluate(source, false)?.ToString() ?? "";
            }), "string");
            WorkingCreateGlobal("ParseJSON", new Func<string, object>((json) =>
            {
                try
                {
                    return Helpers.JSONSerializer<object>.DeSerialize(json);
                }
                catch (Exception ex)
                {
                    throw new Exception($"Failed to parse JSON: {ex.Message}");
                }
            }), "string");
            WorkingCreateGlobal("SerializeJSON", new Func<object, string>((obj) =>
            {
                try
                {
                    return Helpers.JSONSerializer<object>.Serialize(obj);
                }
                catch (Exception ex)
                {
                    throw new Exception($"Failed to serialize to JSON: {ex.Message}");
                }
            }), "string");
            WorkingCreateGlobal("CreateNewThread", new Func<string, string, object>((source, input) =>
            {
                var engine = new PlasticEngine();
                var thread = new System.Threading.Thread(() =>
                {
                    try
                    {
                        string fileName = null;
                        var args = System.Environment.GetCommandLineArgs();
                        if (string.IsNullOrEmpty(fileName))
                            fileName = source;

                        if (!string.IsNullOrEmpty(fileName) && File.Exists(fileName))
                        {
                            string fileSource = File.ReadAllText(fileName);
                            var lexer = new Lexer(fileSource);
                            var tokens = lexer.ScanTokens();
                            var parser = new Parser(tokens);
                            var packageProgram = parser.Parse();

                            engine._packages["newThreadParentedEngine"] = packageProgram;
                            engine.Evaluate(input, false);
                        }
                        else
                        {
                            engine.Evaluate(input, false);
                        }
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine($"Thread error: {ex.Message}");
                    }
                });
                thread.Start();
                return thread.ManagedThreadId.ToString();
            }), "string");
            WorkingCreateGlobal("GenerateUUID", new Func<string>(() => Guid.NewGuid().ToString()), "string");
            WorkingCreateGlobal("GetCurrentDirectory", new Func<string>(() => System.Environment.CurrentDirectory), "string");
            WorkingCreateGlobal("ConvertToBinary", new Func<string, string>((b) => BMAM.ToBinary(b)), "string");
            WorkingCreateGlobal("BinaryToConvert", new Func<string, string>((b) => BMAM.FromBinary(b)), "string");
            WorkingCreateGlobal("ConvertToMorse", new Func<string, string>((b) => BMAM.ToMorse(b)), "string");
            WorkingCreateGlobal("MorseToConvert", new Func<string, string>((b) => BMAM.DecodeMorse(b)), "string");
            WorkingCreateGlobal("Hash", new Func<string, string>((b) => BMAM.HashText(b)), "string");
            WorkingCreateGlobal("VerifyHash", new Func<string, string, bool>((b, a) => BMAM.VerifyHash(b, a)), "bool");
            WorkingCreateGlobal("array", new Func<int, object[]>(size => new object[size]), "array");
            WorkingCreateGlobal("map", new Func<object>(() => new Dictionary<string, object>()), "map");
            WorkingCreateGlobal("list", new Func<object>(() => new List<object>()), "list");

            WorkingCreateGlobal("addtolist", new Func<List<object>, object, bool>((list, item) =>
            {
                if (list == null) throw new ArgumentNullException(nameof(list));
                list.Add(item);
                return true;
            }), "bool");
            WorkingCreateGlobal("removefromlist", new Func<List<object>, object, bool>((list, item) =>
            {
                if (list == null) throw new ArgumentNullException(nameof(list));
                return list.Remove(item);
            }), "bool");
            WorkingCreateGlobal("getfromlist", new Func<List<object>, int, object>((list, index) =>
            {
                if (list == null) throw new ArgumentNullException(nameof(list));
                if (index < 0 || index >= list.Count)
                    throw new IndexOutOfRangeException($"Index {index} is out of range for list of size {list.Count}.");
                return list[index];
            }), "object");
            WorkingCreateGlobal("settolist", new Func<List<object>, int, object, bool>((list, index, item) =>
            {
                if (list == null) throw new ArgumentNullException(nameof(list));
                if (index < 0 || index >= list.Count)
                    throw new IndexOutOfRangeException($"Index {index} is out of range for list of size {list.Count}.");
                list[index] = item;
                return true;
            }), "bool");

            WorkingCreateGlobal("random", new Func<double>(() => new Random().NextDouble()), "f64");
            WorkingCreateGlobal("time", new Func<long>(() => DateTimeOffset.Now.ToUnixTimeMilliseconds()), "i64");
            WorkingCreateGlobal("parseFloat", new Func<string, double>(s => double.Parse(s)), "f64");
            WorkingCreateGlobal("CollectGarbage", new Func<bool>(() =>
            {
                try
                {
                    GC.Collect();
                    GC.WaitForPendingFinalizers();
                    return true;
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"Garbage collection error: {ex.Message}");
                    return false;
                }
            }), "bool");
            WorkingCreateGlobal("GetCurrentProcessId", new Func<int>(() =>
            {
                return System.Diagnostics.Process.GetCurrentProcess().Id;
            }), "i32");
            WorkingCreateGlobal("GetEnvironmentVariable", new Func<string, string>((key) =>
            {
                return System.Environment.GetEnvironmentVariable(key) ?? string.Empty;
            }), "string");
            WorkingCreateGlobal("GetCallerFileName", new Func<string>(() =>
            {
                var stackTrace = new System.Diagnostics.StackTrace();
                var frame = stackTrace.GetFrame(1);
                if (frame != null)
                {
                    return frame.GetFileName() ?? "Unknown file";
                }
                return "Unknown file";
            }), "string");
            WorkingCreateGlobal("extensiveError", new Action(() => { Console.ForegroundColor = ConsoleColor.Red;
                for (int i = 0; i < 10; i++) { Console.WriteLine("extensiveError Called"); }
                Console.ResetColor();
                Console.ForegroundColor = ConsoleColor.White;
                Thread.Sleep(50);
                shouldLeave = true;
            }), "void");
            WorkingCreateGlobal("startHttpServer", new Func<string, bool>((port) =>
            {
                try
                {
                    var listener = new System.Net.HttpListener();
                    listener.Prefixes.Add($"http://*:{port}/");
                    listener.Start();

                    var thread = new Thread(() =>
                    {
                        while (listener.IsListening)
                        {
                            try
                            {
                                var context = listener.GetContext();
                                var request = context.Request;
                                var response = context.Response;

                                string method = request.HttpMethod;
                                string path = request.Url.AbsolutePath;
                                string body = new StreamReader(request.InputStream).ReadToEnd();

                                lock (_httpLock)
                                {
                                    _lastHttpRequestMethod = method;
                                    _lastHttpRequestPath = path;
                                    _lastHttpRequestData = body;
                                    _lastHttpRequestResponse = "";
                                    _lastHttpContext = context;
                                }

                                int waitMs = 0;
                                while (true)
                                {
                                    lock (_httpLock)
                                    {
                                        if (!string.IsNullOrEmpty(_lastHttpRequestResponse))
                                            break;
                                    }
                                    Thread.Sleep(10);
                                    waitMs += 10;
                                    if (waitMs > 10000) // 10 seconds timeout
                                    {
                                        lock (_httpLock)
                                        {
                                            _lastHttpRequestResponse = "Timeout waiting for response.";
                                        }
                                        break;
                                    }
                                }

                                string responseContent;
                                lock (_httpLock)
                                {
                                    responseContent = _lastHttpRequestResponse;
                                }

                                var buffer = System.Text.Encoding.UTF8.GetBytes(responseContent);
                                response.ContentLength64 = buffer.Length;
                                response.OutputStream.Write(buffer, 0, buffer.Length);
                                response.Close();
                            }
                            catch (Exception ex)
                            {
                                Console.WriteLine($"HTTP server error: {ex.Message}");
                            }
                        }
                    })
                    { IsBackground = true };
                    thread.Start();


                    return true;
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"Failed to start HTTP server: {ex.Message}");
                    return false;
                }
            }), "bool");

            // Work in progress 'Shared Memory'
            WorkingCreateGlobal("SendToChannel", new Action<string>(channel.Add), "void");
            WorkingCreateGlobal("ReceiveFromChannel", new Func<string>(channel.Take), "string");
            WorkingCreateGlobal("GetChannelCount", new Func<double>(() => (double)channel.Count), "i32");

            WorkingCreateGlobal("GetHttpRequestData", new Func<string>(() =>
            {
                lock (_httpLock) { return _lastHttpRequestData; }
            }), "string");
            WorkingCreateGlobal("GetHttpRequestMethod", new Func<string>(() =>
            {
                lock (_httpLock) { return _lastHttpRequestMethod; }
            }), "string");
            WorkingCreateGlobal("GetHttpRequestPath", new Func<string>(() =>
            {
                lock (_httpLock) { return _lastHttpRequestPath; }
            }), "string");
            WorkingCreateGlobal("SetHttpResponse", new Action<string>((resp) =>
            {
                lock (_httpLock) { _lastHttpRequestResponse = resp; }
            }), "void");
        }

        public List<string> GetErrors(string source, bool readPlugins = true)
        {
            var errors = new List<string>();
            try
            {
                var lexer = new Lexer(source);
                List<Token> tokens;
                try
                {
                    tokens = lexer.ScanTokens();
                }
                catch (Exception lexEx)
                {
                    errors.Add($"Lexer error: {lexEx.Message}");
                    return errors;
                }

                var parser = new Parser(tokens);
                ProgramNode program;
                try
                {
                    parser._current = 0;
                    program = parser.Parse();
                    if(program == null)
                    {
                        foreach(string error in parser.Errors)
                        {
                            errors.Add(error);
                        }
                        return errors;
                    }
                }
                catch (Exception parseEx)
                {
                    errors.Add($"Parser error: {parseEx.Message}");
                    return errors;
                }

                if (readPlugins)
                {
                    try
                    {
                        ProcessImports(program);
                    }
                    catch (Exception importEx)
                    {
                        errors.Add($"Import error: {importEx.Message}");
                        return errors;
                    }
                }

                try
                {
                    var checker = new TypeChecker();
                    checker.Check(program);
                }
                catch (Exception typeEx)
                {
                    errors.Add($"Type error: {typeEx.Message}");
                }
            }
            catch (Exception ex)
            {
                errors.Add($"Unknown error: {ex.Message}");
            }
            return errors;
        }

        public void WorkingCreateGlobal(string name, object value, object typeReturn)
        {
            ArgumentNullException.ThrowIfNull(name);
            ArgumentNullException.ThrowIfNull(value);
            if (!ImportStmt._builtinFunctions.ContainsKey(name))
            {
                ImportStmt._builtinFunctions.Add(name, new TypeInfo(typeReturn.ToString(), false, null, false, false, 0));
            }
            else
            {
                ImportStmt._builtinFunctions[name] = new TypeInfo(typeReturn.ToString(), false, null, false, false, 0);
            }
            _globals[name] = value;
        }

        public object? Evaluate(string source, bool readPlugins = true)
        {
            try
            {
                var lexer = new Lexer(source);
                var tokens = lexer.ScanTokens();

                var parser = new Parser(tokens);
                parser._current = 0;
                var program = parser.Parse();

                if (readPlugins)
                {
                    ProcessImports(program);
                }

                checker = new TypeChecker();
                checker.Check(program);

                terpreter = new Interpreter(_globals);
                return terpreter.Execute(program);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
                return null;
            }
        }

        private void ProcessImports(ProgramNode program)
        {
            var imports = program.Statements.Where(s => s is ImportStmt).Cast<ImportStmt>().ToList();

            foreach (var import in imports)
            {
                var packageProgram = LoadPackage(import.PackageName);

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

            program.Statements.RemoveAll(s => s is ImportStmt);
        }

        private void MergeExportedSymbols(ProgramNode target, ProgramNode source)
        {
            var exportedSymbols = source.Statements.Where(s =>
                (s is FnDecl && ((FnDecl)s).alwaysTrue) ||
                (s is VarDecl && ((VarDecl)s).alwaysTrue) ||
                (s is StructDecl) ||       
                (s is EnumDecl) ||          
                (s is TraitDecl)           
            ).ToList();

            target.Statements.InsertRange(0, exportedSymbols);
        }
    }

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

    public static class EngineFeatures
    {
        public class CFunc
        {
            public string _name;
            public string _returnType;
            public Delegate _function;
            public PlasticEngine _engine;

            public CFunc(string name, string returnType, Delegate function, PlasticEngine? engine = null)
            {
                _name = name;
                _returnType = returnType;
                _function = function;
                if (engine != null)
                    _engine = engine;
            }

            public void Register()
            {
                if (!ImportStmt._builtinFunctions.ContainsKey(_name))
                {
                    ImportStmt._builtinFunctions.Add(_name, new TypeInfo(_returnType, false, null, false, false, 0));
                }
                else
                {
                    ImportStmt._builtinFunctions[_name] = new TypeInfo(_returnType, false, null, false, false, 0);
                }
                _engine.RegisterGlobal(_name, _function);
            }
        }

        public static InterpretResult? Interpret(string source, bool logClosed = true, List<CFunc>? functions = null)
        {
            var engine = new PlasticEngine();

            foreach(var func in functions ?? new List<CFunc>())
            {
                func._engine = engine;
                func.Register();
            }

            object result = engine.Evaluate(source);
            if (engine.shouldLeave)
            {
                if(logClosed)
                    Console.WriteLine("Closed.");

                return new InterpretResult
                {
                    Result = result,
                    PlasticEngine = engine
                };
            }
            return new InterpretResult
            {
                Result = result,
                PlasticEngine = engine
            };
        }

        public static bool ShouldLeave(PlasticEngine engine)
        {
            return engine.shouldLeave;
        }

        public class InterpretResult
        {
            public object? Result { get; set; }
            public PlasticEngine PlasticEngine { get; set; }
        }
    }
}