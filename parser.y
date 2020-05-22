%using mini_lang
%using GardensPoint
%namespace GardensPoint
%output=Parser.cs

%{  
    public AstBuilder builder = new AstBuilder();
%}

%union
{
public string str;
public IEvaluable node;
public VarType type;
}

%token Program If Else While Read Write Return Type
%token Assign Or And BitOr BitAnd Eq Neq Gt Gte Lt Lte Plus Minus Mult Div Not BitNot
%token LParen RParen LBrace RBrace Semicolon
%token Ident String LitInt LitDouble LitBool

%token <type> Type
%token <str> String Ident
%token <node> LitInt LitDouble LitBool

%type <node> rvalue constant rvalorstring

%%

start: program
     | ;

program: Program { builder.AddProgram(); } LBrace lines RBrace ;

lines: lines line 
     | ;

line: expression Semicolon ;

expression: declaration 
          | assignment
          | write
          | read
          | Return { builder.AddReturn(); } ;

declaration: Type Ident { builder.AddDeclaration($2, $1); } ;

assignment: Ident Assign rvalue { builder.AddAssignment($1, $3); } ;

write: Write rvalorstring { builder.AddWrite($2); } ;

read: Read Ident { builder.AddRead($2); } ;

rvalorstring: rvalue 
            | String { $$ = new Constant($1, VarType.String); } ;

rvalue: Ident { $$ = new Identifier($1); }
      | constant ;

constant: LitInt 
        | LitDouble 
        | LitBool ;

%%
public Parser(Scanner scanner) : base(scanner) { }