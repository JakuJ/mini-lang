%using mini_lang
%using GardensPoint
%namespace GardensPoint
%output=Parser.cs

%{
    void WriteError(string what)
    {
        Console.WriteLine($"Parsing error at line {lineNumber}: {what}");
    }
    
    public AstBuilder builder = new AstBuilder();
%}

%union
{
public string value;
}

%token Program If Else While Read Write Return Type True False
%token Assign Or And BitOr BitAnd Eq Neq Gt Gte Lt Lte Plus Minus Mult Div Not BitNot
%token LParen RParen LBrace RBrace Semicolon
%token Ident LitInt LitDouble

%token <value> Type Ident LitInt LitDouble

%%

start: program | ;

program: Program { builder.addProgram(); } LBrace lines RBrace;

lines: lines line | ;

line: declaration ;

declaration: Type Ident Semicolon { builder.addVariable($1, $2); } ;

%%

int lineNumber = 1;

public Parser(Scanner scanner) : base(scanner) { }