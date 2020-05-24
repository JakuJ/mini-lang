%using QUT.Gppg;
%using mini_lang;
%namespace GardensPoint
%option codePage:65001 out:Scanner.cs

%{
    public int lineNumber = 1;
    public int Errors { get; set; }
    
    public override void yyerror(string message, params object[] args)
    {
        Console.Error.WriteLine(message + " on line " + lineNumber.ToString());
        Errors++;
    }
%}

Integer     ("0"|[1-9][0-9]*)
Real        ("0"|[1-9][0-9]*)\.[0-9]+
Bool        ("true"|"false")
Ident       ([a-zA-Z][a-zA-Z0-9]*)
Comment     "//".*
String      "\""[^\n\"]*"\""
BitAnd      "&"{1}
LogicAnd    "&"{2}
BitOr       "|"{1}
LogicOr     "|"{2}

%%

"program"       { return (int)Tokens.Program; }
"{"             { return (int)Tokens.LBrace; }
"}"             { return (int)Tokens.RBrace; }
"int"           { yylval.type = VarType.Integer; return (int)Tokens.Type; }
"double"        { yylval.type = VarType.Double; return (int)Tokens.Type; }
"bool"          { yylval.type = VarType.Bool; return (int)Tokens.Type; }
"write"         { return (int)Tokens.Write; }
"read"          { return (int)Tokens.Read; }
"return"        { return (int)Tokens.Return; }
"while"         { return (int)Tokens.While; }

"=="            { return (int)Tokens.Eq; }
"="             { return (int)Tokens.Assign; }
">="            { return (int)Tokens.Gte; }
">"             { return (int)Tokens.Gt; }
"<="            { return (int)Tokens.Lte; }
"<"             { return (int)Tokens.Lt; }
"!="            { return (int)Tokens.Neq; }
"!"             { return (int)Tokens.Not; }
{LogicOr}       { return (int)Tokens.Or; }
{BitOr}         { return (int)Tokens.BitOr; }
{LogicAnd}      { return (int)Tokens.And; }
{BitAnd}        { return (int)Tokens.BitAnd; }
";"             { return (int)Tokens.Semicolon; }
"-"             { return (int)Tokens.Minus; }
"~"             { return (int)Tokens.BitNot; }
"("             { return (int)Tokens.LParen; }
")"             { return (int)Tokens.RParen; }
"*"             { return (int)Tokens.Mult; }
"/"             { return (int)Tokens.Div; }
"+"             { return (int)Tokens.Plus; }

{Integer}       { yylval.eval = new Constant(yytext, VarType.Integer); return (int)Tokens.LitInt; }
{Real}          { yylval.eval = new Constant(yytext, VarType.Double); return (int)Tokens.LitDouble; }
{Bool}          { yylval.eval = new Constant(yytext, VarType.Bool); return (int)Tokens.LitBool; }
{Ident}         { yylval.str = yytext; return (int)Tokens.Ident; }
{String}        { yylval.str = yytext; return (int)Tokens.String; }
"\n"            { lineNumber++; }
<<EOF>>         { return (int)Tokens.EOF; }
{Comment}       { }
" "             { }
"\t"            { }
.               { yyerror("Invalid token: " + yytext); return (int)Tokens.error; }