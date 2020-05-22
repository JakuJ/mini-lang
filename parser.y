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

%type <node> constexpr constant writable
%type <node> op_1 op_2 op_3 op_4 op_5 op_6

%%

start: program
     | ;

program: Program { builder.AddProgram(); } LBrace lines RBrace ;

lines: lines line 
     | ;

line: instruction 
    | expression Semicolon ;

instruction: block ;

block: LBrace lines RBrace ;

expression: declaration 
          | assignment
          | write
          | read
          | Return { builder.AddReturn(); }
          ;

declaration: Type Ident { builder.AddDeclaration($2, $1); } ;

assignment: Ident Assign op_6 { builder.AddAssignment($1, $3); } ;

write: Write writable { builder.AddWrite($2); } ;

read: Read Ident { builder.AddRead($2); } ;

writable: op_6
        | String { $$ = new Constant($1, VarType.String); } ;

constexpr: Ident { $$ = new Identifier($1); }
         | constant ;

constant: LitInt 
        | LitDouble 
        | LitBool ;
        
// Operators

op_1: Minus op_1
    | BitNot op_1
    | Not op_1
    | LParen Type RParen op_1 // explicit conversion
    | LParen op_6 RParen { $$ = $2; } // parentheses
    | constexpr
    ;

op_2: op_2 BitAnd op_1
    | op_2 BitOr op_1
    | op_1 ;

op_3: op_3 Mult op_2 { $$ = new MathOp("*", $1, $3); }
    | op_3 Div op_2 { $$ = new MathOp("/", $1, $3); }
    | op_2 ;

op_4: op_4 Plus op_3 { $$ = new MathOp("+", $1, $3); }
    | op_4 Minus op_3 { $$ = new MathOp("-", $1, $3); }
    | op_3 ;
    
op_5: op_5 Eq op_4
    | op_5 Neq op_4
    | op_5 Gt op_4
    | op_5 Gte op_4
    | op_5 Lt op_4
    | op_5 Lte op_4
    | op_4 ;
    
op_6: op_6 And op_5
    | op_6 Or op_5
    | op_5 ;

%%
public Parser(Scanner scanner) : base(scanner) { }