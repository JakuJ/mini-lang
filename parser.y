%using mini_lang
%using GardensPoint
%using System.Linq
%namespace GardensPoint
%output=Parser.cs

%{  
    private AstBuilder builder;
    public Program Program { get; private set; }
    
    private void ParseError(string message)
    {
        Console.Error.WriteLine(message + " on line " + _scanner.lineNumber.ToString());
        _scanner.Errors++;
    }
%}

%union
{
    public string str;
    public int num;
    public List<string> strings;
    public VarType type;
    public INode node;
    public List<INode> nodes;
    public IEvaluable eval;
    public ILValue lval;
    public List<IEvaluable> evals;
}

%token Program If Else While Read Write Return Break Continue Create
%token Assign Or And BitOr BitAnd Eq Neq Gt Gte Lt Lte Plus Minus Mult Div Not BitNot
%token LParen RParen LBrace RBrace LBracket RBracket Semicolon Comma

%token <type>   Type
%token <str>    String Ident
%token <eval>   LitInt LitDouble LitBool

%type <node>    block statement if while oneliner write read break create
%type <nodes>   declaration declarations statements
%type <lval>    lvalue
%type <eval>    value_0 op_1 op_2 op_3 op_4 op_5 op_6 evaluable
%type <evals>   sizes indexing
%type <type>    type
%type <strings> idents
%type <num>     dims
%%

start: Program block                         { Program = new Program($2); } ;

block: LBrace                                { builder.PushScope(); }
       declarations statements RBrace        { $3.AddRange($4); $$ = new Block($3); builder.PopScope(); } ;

declarations: declarations declaration              { $1.AddRange($2); $$ = $1; }
            |                                       { $$ = new List<INode>(); }
            ;

statements: statements statement    { $1.Add($2); $$ = $1; }
          |                         { $$ = new List<INode>(); } 
          ;
          
declaration: type idents Ident Semicolon    { $$ = $2.Append($3).Select(x => builder.CreateDeclaration(x, $1) as INode).ToList(); }
           | error Semicolon                { yyerrok(); $$ = new List<INode>(); }
           | error EOF
           | error                          { yyerrok(); $$ = new List<INode>(); }
           ;

type: Type
    | Type LBracket dims RBracket { $$ = new VarType.ArrayT($1, $3); }
    ;
    
dims: dims Comma    { $$ = $1 + 1; }
    |               { $$ = 1; }
    ;

idents: idents Ident Comma          { $1.Add($2); $$ = $1; }
      |                             { $$ = new List<string>(); }
      ;

statement: block
         | while
         | if
         | oneliner Semicolon
         | error Semicolon      { yyerrok(); }
         | error EOF
         | error                { yyerrok(); }
         ;

while: While LParen evaluable RParen    { builder.PushLoop(); }
       statement                        { $$ = new While($3, $6); builder.PopLoop(); }
       ;

if: If LParen evaluable RParen statement                  { $$ = new IfElse($3, $5); }
  | If LParen evaluable RParen statement Else statement   { $$ = new IfElse($3, $5, $7); }
  ;
         
oneliner: evaluable     { $$ = new ExprStatement($1); } // discard created value
        | write
        | create
        | read
        | break
        | Continue      { $$ = builder.CreateContinue(); }
        | Return        { $$ = new Return(); }
        ;

read: Read lvalue               { $$ = new Read($2); } ;

write: Write evaluable          { $$ = new Write($2); }
     | Write String             { $$ = new WriteString($2); }
     ;

break: Break            { $$ = builder.CreateBreak(new Constant("1", VarType.Integer)); }
     | Break LitInt     { $$ = builder.CreateBreak($2); }
     ;
     
create: Create Ident indexing   { $$ = builder.CreateArrayCreation($2, $3); } ;

indexing: LBracket sizes evaluable RBracket { $2.Add($3); $$ = $2; } ;

sizes: sizes evaluable Comma    { $1.Add($2); $$ = $1; }
     |                          { $$ = new List<IEvaluable>(); }
     ;

// Operators

lvalue: Ident                  { $$ = builder.CreateIdentifier($1); }
      | Ident indexing         { $$ = new Indexing(builder.CreateIdentifier($1), $2); }
      ;

value_0: lvalue                 { $$ = $1 as IEvaluable; }
       | LitInt
       | LitDouble
       | LitBool ;

op_1: Minus op_1                { $$ = new UnaryOp(UnaryOp.OpType.IntNegate, $2); }
    | BitNot op_1               { $$ = new UnaryOp(UnaryOp.OpType.BitwiseNot, $2); }
    | Not op_1                  { $$ = new UnaryOp(UnaryOp.OpType.LogicalNot, $2); }
    | LParen Type RParen op_1   { $$ = new UnaryOp($2, $4); }
    | LParen op_6 RParen        { $$ = $2; }
    | value_0 ;

op_2: op_2 BitAnd op_1  { $$ = new MathOp(MathOp.OpType.BitAnd, $1, $3); }
    | op_2 BitOr op_1   { $$ = new MathOp(MathOp.OpType.BitOr, $1, $3); }
    | op_1 ;

op_3: op_3 Mult op_2    { $$ = new MathOp(MathOp.OpType.Mult, $1, $3); }
    | op_3 Div op_2     { $$ = new MathOp(MathOp.OpType.Div, $1, $3); }
    | op_2 ;

op_4: op_4 Plus op_3    { $$ = new MathOp(MathOp.OpType.Add, $1, $3); }
    | op_4 Minus op_3   { $$ = new MathOp(MathOp.OpType.Sub, $1, $3); }
    | op_3 ;
    
op_5: op_5 Eq op_4      { $$ = new CompOp(CompOp.OpType.Eq, $1, $3); }
    | op_5 Neq op_4     { $$ = new CompOp(CompOp.OpType.Neq, $1, $3); }
    | op_5 Gt op_4      { $$ = new CompOp(CompOp.OpType.Gt, $1, $3); }
    | op_5 Gte op_4     { $$ = new CompOp(CompOp.OpType.Gte, $1, $3); }
    | op_5 Lt op_4      { $$ = new CompOp(CompOp.OpType.Lt, $1, $3); }
    | op_5 Lte op_4     { $$ = new CompOp(CompOp.OpType.Lte, $1, $3); }
    | op_4 ;
    
op_6: op_6 And op_5     { $$ = new LogicOp(LogicOp.OpType.And, $1, $3); }
    | op_6 Or op_5      { $$ = new LogicOp(LogicOp.OpType.Or, $1, $3); }
    | op_5 ;
    
evaluable: Ident Assign evaluable               { $$ = builder.CreateAssignment($1, $3); }
         | Ident indexing Assign evaluable      { $$ = new Assignment(new Indexing(builder.CreateIdentifier($1), $2), $4); }
         | op_6 ;

%%

private Scanner _scanner;

public Parser(Scanner scanner, AstBuilder builder) : base(scanner) { 
       _scanner = scanner;
       this.builder = builder;
}