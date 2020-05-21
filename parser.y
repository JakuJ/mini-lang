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
public string str;
public INode node;
public VarType type;
}

%token Program If Else While Read Write Return Type
%token Assign Or And BitOr BitAnd Eq Neq Gt Gte Lt Lte Plus Minus Mult Div Not BitNot
%token LParen RParen LBrace RBrace Semicolon
%token Ident LitInt LitDouble LitBool

%token <type> Type
%token <str> Ident
%token <node> LitInt LitDouble LitBool

%type <node> rvalue literal

%%

start: program | ;

program: Program { builder.AddProgram(); } LBrace lines RBrace;

lines: lines line | ;

line: expression Semicolon;

expression: declaration | assignment ;

declaration: Type Ident { builder.AddDeclaration($2, $1); } ;

assignment: Ident Assign rvalue { builder.AddAssignment($1, $3); } ;

rvalue: Ident | literal;

literal: LitInt | LitDouble | LitBool ;

%%

int lineNumber = 1;

public Parser(Scanner scanner) : base(scanner)
{ 
    builder.Scanner = scanner;    
}