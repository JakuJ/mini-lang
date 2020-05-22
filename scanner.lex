%using QUT.Gppg;
%using mini_lang;
%namespace GardensPoint
%option codePage:65001 out:Scanner.cs

%{
    int lineNumber = 1;
    
    public override void yyerror(string message, params object[] args)
    {
        Console.Error.WriteLine("Parsing error at line " + lineNumber.ToString() + ": " + message);
    }
%}

Integer     ("0"|[1-9][0-9]*)
Real        ("0"|[1-9][0-9]*)\.[0-9]+
Bool        ("true"|"false")
Ident       [a-zA-Z][a-zA-Z0-9]*
Comment     "//".*
String      "\""[^\n\"]*"\""

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

"=="            { return (int)Tokens.Eq; }
"="             { return (int)Tokens.Assign; }
">="            { return (int)Tokens.Gte; }
">"             { return (int)Tokens.Gt; }
"<="            { return (int)Tokens.Lte; }
"<"             { return (int)Tokens.Lt; }
"!="            { return (int)Tokens.Neq; }
"!"             { return (int)Tokens.Not; }
"||"            { return (int)Tokens.Or; }
"|"             { return (int)Tokens.BitOr; }
"&&"            { return (int)Tokens.And; }
"&"             { return (int)Tokens.BitAnd; }
";"             { return (int)Tokens.Semicolon; }
"-"             { return (int)Tokens.Minus; }
"~"             { return (int)Tokens.BitNot; }
"("             { return (int)Tokens.LParen; }
")"             { return (int)Tokens.RParen; }
"*"             { return (int)Tokens.Mult; }
"/"             { return (int)Tokens.Div; }
"+"             { return (int)Tokens.Plus; }

{Integer}       { yylval.node = new Constant(yytext, VarType.Integer); return (int)Tokens.LitInt; }
{Real}          { yylval.node = new Constant(yytext, VarType.Double); return (int)Tokens.LitDouble; }
{Bool}          { yylval.node = new Constant(yytext, VarType.Bool); return (int)Tokens.LitBool; }
{Ident}         { yylval.str = yytext; return (int)Tokens.Ident; }
{String}        { yylval.str = yytext; return (int)Tokens.String; }
"\n"            { lineNumber++; }
<<EOF>>         { }
{Comment}       { }
" "             { }
"\t"            { }
.               { yyerror(yytext); }