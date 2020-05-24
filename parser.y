%using mini_lang
%using GardensPoint
%namespace GardensPoint
%output=Parser.cs

%{  
    private AstBuilder builder;
    
    private void ParseError(string message)
    {
        Console.Error.WriteLine(message + " on line " + _scanner.lineNumber.ToString());
        _scanner.Errors++;
    }
%}

%union
{
public string str;
public IEvaluable eval;
public VarType type;
}

%token Program If Else While Read Write Return
%token Assign Or And BitOr BitAnd Eq Neq Gt Gte Lt Lte Plus Minus Mult Div Not BitNot
%token LParen RParen LBrace RBrace Semicolon

%token <type> Type
%token <str> String Ident
%token <eval> LitInt LitDouble LitBool

%type <eval> rhs value_0 writable
%type <eval> op_1 op_2 op_3 op_4 op_5 op_6

%%

start: Program { builder.AddProgram(); } block ;

statements: statements statement
          | ;

statement: block
         | whileStmnt
         | oneliner Semicolon
         | error Semicolon { yyerrok(); }
         | error EOF
         | error { yyerrok(); }
         ;
         
block: LBrace statements RBrace ;

whileStmnt: While LParen rhs RParen {{ builder.StartWhile($3); }} statement {{ builder.EndWhile(); }} ;
         
oneliner: declaration
        | assignment
        | write
        | read
        | Return { builder.AddReturn(); }
        ;

declaration: Type Ident { builder.AddDeclaration($2, $1); } ;

read: Read Ident { builder.AddRead($2); } ;

assignment: Ident Assign rhs { builder.AddAssignment($1, $3); } ;

write: Write writable { builder.AddWrite($2); } ;

writable: rhs
        | String { $$ = new Constant($1, VarType.String); }
        ;

rhs: op_6 ; // constants, identifiers and arbitrary expressions

// Operators

value_0: Ident { $$ = builder.CreateIdentifier($1); }
       | LitInt
       | LitDouble 
       | LitBool ;

op_1: Minus op_1 { $$ = new UnaryOp(UnaryOp.OpType.IntNegate, $2); }
    | BitNot op_1 { $$ = new UnaryOp(UnaryOp.OpType.BitwiseNot, $2); }
    | Not op_1 { $$ = new UnaryOp(UnaryOp.OpType.LogicalNot, $2); }
    | LParen Type RParen op_1 { $$ = new UnaryOp($2, $4); } // explicit conversion
    | LParen op_6 RParen { $$ = $2; } // parentheses
    | value_0
    ;

op_2: op_2 BitAnd op_1 { $$ = new MathOp(MathOp.OpType.BitAnd, $1, $3); }
    | op_2 BitOr op_1 { $$ = new MathOp(MathOp.OpType.BitOr, $1, $3); }
    | op_1 ;

op_3: op_3 Mult op_2 { $$ = new MathOp(MathOp.OpType.Mult, $1, $3); }
    | op_3 Div op_2 { $$ = new MathOp(MathOp.OpType.Div, $1, $3); }
    | op_2 ;

op_4: op_4 Plus op_3 { $$ = new MathOp(MathOp.OpType.Add, $1, $3); }
    | op_4 Minus op_3 { $$ = new MathOp(MathOp.OpType.Sub, $1, $3); }
    | op_3 ;
    
op_5: op_5 Eq op_4 { $$ = new CompOp(CompOp.OpType.Eq, $1, $3); }
    | op_5 Neq op_4 { $$ = new CompOp(CompOp.OpType.Neq, $1, $3); }
    | op_5 Gt op_4 { $$ = new CompOp(CompOp.OpType.Gt, $1, $3); }
    | op_5 Gte op_4 { $$ = new CompOp(CompOp.OpType.Gte, $1, $3); }
    | op_5 Lt op_4 { $$ = new CompOp(CompOp.OpType.Lt, $1, $3); }
    | op_5 Lte op_4 { $$ = new CompOp(CompOp.OpType.Lte, $1, $3); }
    | op_4 ;
    
op_6: op_6 And op_5 { $$ = new LogicOp(LogicOp.OpType.And, $1, $3); }
    | op_6 Or op_5 { $$ = new LogicOp(LogicOp.OpType.Or, $1, $3); }
    | op_5 ;

%%

private Scanner _scanner;

public Parser(Scanner scanner, AstBuilder builder) : base(scanner) { 
       _scanner = scanner;
       this.builder = builder;
}