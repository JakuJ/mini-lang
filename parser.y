%using mini_lang
%using GardensPoint
%namespace GardensPoint
%output=Parser.cs

%{  
    public void ParseError(string message)
    {
        Console.Error.WriteLine(message + " on line " + _scanner.lineNumber.ToString());
        _scanner.errors++;
    }
    
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

%type <node> rhs value_0 writable
%type <node> op_1 op_2 op_3 op_4 op_5 op_6

%%

start: Program { builder.AddProgram(); } block ;

statements: statements statement
          | ;

statement: block
         | oneliner Semicolon
         | oneliner { ParseError("Syntax error, missing semicolon"); }
         | error Semicolon { yyerrok(); }
         | error EOF
         | error { yyerrok(); }
         ;
         
oneliner: declaration
        | assignment
        | write
        | read
        | Return { builder.AddReturn(); }
        ;

block: LBrace statements RBrace ;

declaration: Type Ident { builder.AddDeclaration($2, $1); } ;

read: Read Ident { builder.AddRead($2); } ;

assignment: Ident Assign rhs { builder.AddAssignment($1, $3); } ;

write: Write writable { builder.AddWrite($2); } ;

writable: rhs
        | String { $$ = new Constant($1, VarType.String); }
        ;

rhs: op_6 ; // constants, identifiers and arbitrary expressions

// Operators

value_0: Ident { $$ = new Identifier($1); }
       | LitInt
       | LitDouble 
       | LitBool ;

op_1: Minus op_1
    | BitNot op_1
    | Not op_1
    | LParen Type RParen op_1 // explicit conversion
    | LParen op_6 RParen { $$ = $2; } // parentheses
    | value_0
    ;

op_2: op_2 BitAnd op_1 { $$ = new MathOp("&", $1, $3); }
    | op_2 BitOr op_1 { $$ = new MathOp("|", $1, $3); }
    | op_1 ;

op_3: op_3 Mult op_2 { $$ = new MathOp("*", $1, $3); }
    | op_3 Div op_2 { $$ = new MathOp("/", $1, $3); }
    | op_2 ;

op_4: op_4 Plus op_3 { $$ = new MathOp("+", $1, $3); }
    | op_4 Minus op_3 { $$ = new MathOp("-", $1, $3); }
    | op_3 ;
    
op_5: op_5 Eq op_4 { $$ = new CompOp("==", $1, $3); }
    | op_5 Neq op_4 { $$ = new CompOp("!=", $1, $3); }
    | op_5 Gt op_4 { $$ = new CompOp(">", $1, $3); }
    | op_5 Gte op_4 { $$ = new CompOp(">=", $1, $3); }
    | op_5 Lt op_4 { $$ = new CompOp("<", $1, $3); }
    | op_5 Lte op_4 { $$ = new CompOp("<=", $1, $3); }
    | op_4 ;
    
op_6: op_6 And op_5
    | op_6 Or op_5
    | op_5 ;

%%

private Scanner _scanner;

public Parser(Scanner scanner) : base(scanner) { 
       _scanner = scanner;
}