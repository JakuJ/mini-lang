%using QUT.Gppg;
%using mini_lang;
%namespace GardensPoint
%option codePage:65001 out:Scanner.cs

Integer     ("0"|[1-9][0-9]*)
Real        ("0"|[1-9][0-9]*)\.[0-9]+
Bool        ("true"|"false")
Ident       ([a-zA-Z][a-zA-Z0-9]*)
Comment     "//".*\n
String      \"(\\.|[^"\\\n])*\"
BitAnd      "&"{1}
LogicAnd    "&"{2}
BitOr       "|"{1}
LogicOr     "|"{2}

%%

"program"       { return (int)Tokens.Program; }
"{"             { return (int)Tokens.LBrace; }
"}"             { return (int)Tokens.RBrace; }
"["             { return (int)Tokens.LBracket; }
"]"             { return (int)Tokens.RBracket; }
","             { return (int)Tokens.Comma; }
"int"           { yylval.prim_type = PrimType.Integer; return (int)Tokens.Type; }
"double"        { yylval.prim_type = PrimType.Double; return (int)Tokens.Type; }
"bool"          { yylval.prim_type = PrimType.Bool; return (int)Tokens.Type; }
"create"        { return (int)Tokens.Create; }
"write"         { return (int)Tokens.Write; }
"read"          { return (int)Tokens.Read; }
"return"        { return (int)Tokens.Return; }
"while"         { return (int)Tokens.While; }
"if"            { return (int)Tokens.If; }
"else"          { return (int)Tokens.Else; }
"break"         { return (int)Tokens.Break; }
"continue"      { return (int)Tokens.Continue; }

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

{Integer}       { yylval.eval = new Constant(yytext, PrimType.Integer); return (int)Tokens.LitInt; }
{Real}          { yylval.eval = new Constant(yytext, PrimType.Double); return (int)Tokens.LitDouble; }
{Bool}          { yylval.eval = new Constant(yytext, PrimType.Bool); return (int)Tokens.LitBool; }
{Ident}         { yylval.str = yytext; return (int)Tokens.Ident; }
{String}        { yylval.str = yytext; return (int)Tokens.String; }
<<EOF>>         { return (int)Tokens.EOF; }
{Comment}       { }
" "             { }
"\t"            { }
"\n"            { }
"\r"            { }
.               { InvalidToken(yytext); }

%{
    yylloc = new LexLocation(tokLin,tokCol,tokELin,tokECol);
%}
