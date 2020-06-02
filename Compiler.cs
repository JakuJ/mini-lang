using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using GardensPoint;

namespace mini_lang
{
    #region Extensions

    /// <inheritdoc />
    /// <summary>
    /// This attribute is used to represent a mini-language token string for a value in an enum.
    /// </summary>
    [AttributeUsage(AttributeTargets.Field)]
    public class TokenAttribute : Attribute
    {
        public string Token { get; }
        public TokenAttribute(string value) => Token = value;
    }

    public static class Extensions
    {
        /// <summary>
        /// Will get the string value for a given enums value, this will
        /// only work if you assign the StringValue attribute to
        /// the items in your enum.
        /// </summary>
        public static string GetToken(this Enum value)
        {
            // Get the type
            Type type = value.GetType();

            // Get FieldInfo for this type
            FieldInfo fieldInfo = type.GetField(value.ToString());

            // Get the Token attributes
            if (fieldInfo.GetCustomAttributes(typeof(TokenAttribute), false) is TokenAttribute[] attribs)
            {
                // Return the first if there was a match.
                return attribs.Length > 0 ? attribs[0].Token : null;
            }

            return "<??>"; // TODO: throw an exception?
        }
    }

    #endregion

    #region Common

    /// <summary>
    /// Defines data types available in the language.
    /// </summary>
    public class VarType
    {
        private readonly string _token;
        private VarType(string token) => _token = token;

        public static VarType Integer => IntegerT.Instance;
        public static VarType Double = DoubleT.Instance;
        public static VarType Bool = BoolT.Instance;
        public static VarType String = StringT.Instance;

        public override string ToString() => _token;

        public sealed class IntegerT : VarType
        {
            public static VarType Instance { get; } = new IntegerT();

            private IntegerT() : base("int")
            {
            }
        }

        public sealed class DoubleT : VarType
        {
            public static VarType Instance { get; } = new DoubleT();

            private DoubleT() : base("double")
            {
            }
        }

        public sealed class BoolT : VarType
        {
            public static VarType Instance { get; } = new BoolT();

            private BoolT() : base("bool")
            {
            }
        }

        public sealed class StringT : VarType
        {
            public static VarType Instance { get; } = new StringT();

            private StringT() : base("string")
            {
            }
        }

        public sealed class ArrayT : VarType
        {
            public readonly int Dimensions;
            public readonly VarType Type;

            public ArrayT(VarType type, int dimensions) : base($"{type._token}[{dimensions}]")
            {
                Type = type;
                Dimensions = dimensions;

                if (dimensions > 32)
                {
                    Compiler.Error("Cannot declare array of more than 32 dimensions");
                }
            }
        }
    }

    /// <summary>
    /// An interface for arbitrary nodes in the AST.
    /// </summary>
    /// <seealso cref="INodeVisitor"/>
    public interface INode
    {
        void Accept(INodeVisitor visitor);
    }

    /// <inheritdoc />
    /// <summary>
    /// An interface for expressions evaluable to some <see cref="VarType" />.
    /// </summary>
    public interface IEvaluable : INode
    {
        VarType Type { get; }
    }

    #endregion

    #region AST

    public class Block : INode
    {
        public List<INode> Statements { get; }
        public Block(List<INode> statements) => Statements = statements;
        public void Accept(INodeVisitor visitor) => visitor.VisitBlock(this);
    }

    public class Identifier : IEvaluable
    {
        public readonly string Name;
        public VarType Type { get; }

        public Identifier(string name, VarType type)
        {
            Name = name;
            Type = type;
        }

        void INode.Accept(INodeVisitor visitor) => visitor.VisitIdentifier(this);
    }

    public class Constant : IEvaluable
    {
        public readonly string Value;
        public VarType Type { get; }

        public Constant(string value, VarType type)
        {
            Value = value;
            Type = type;
        }

        void INode.Accept(INodeVisitor visitor) => visitor.VisitConstant(this);
    }

    #region Operators

    public class UnaryOp : IEvaluable
    {
        public VarType Type { get; }

        public enum OpType
        {
            [Token("~")] BitwiseNot,
            [Token("!")] LogicalNot,
            [Token("-")] IntNegate,
            [Token("(int)")] Conv2Int,
            [Token("(double)")] Conv2Double
        }

        public readonly OpType Op;
        public readonly IEvaluable Rhs;

        /// <summary>
        /// Use this constructor for unary operators other than explicit conversions.
        /// </summary>
        /// <param name="op">Operator type - one of <see cref="OpType"/>s</param>
        /// <param name="rhs">Operand</param>
        public UnaryOp(OpType op, IEvaluable rhs)
        {
            Op = op;
            Rhs = rhs;

            switch (Op)
            {
                case OpType.IntNegate:
                    Type = Rhs.Type;
                    if (Type == VarType.Bool) InvalidType();
                    break;
                case OpType.BitwiseNot:
                    Type = VarType.Integer;
                    if (Rhs.Type != Type) InvalidType();
                    break;
                case OpType.LogicalNot:
                    Type = VarType.Bool;
                    if (Rhs.Type != Type) InvalidType();
                    break;
                case OpType.Conv2Int:
                case OpType.Conv2Double:
                    Compiler.Error($"Programmer error: invalid type passed to UnaryOp constructor: {Op}", true);
                    break;
            }
        }

        /// <summary>
        /// Use this constructor for explicit conversions.
        /// </summary>
        /// <param name="convertTo">Target type of the conversion.</param>
        /// <param name="rhs">The value to be converted.</param>
        public UnaryOp(VarType convertTo, IEvaluable rhs)
        {
            Type = convertTo;
            Rhs = rhs;

            switch (convertTo)
            {
                case VarType.IntegerT _:
                    Op = OpType.Conv2Int;
                    break;
                case VarType.DoubleT _:
                    Op = OpType.Conv2Double;
                    break;
                default:
                    Compiler.Error($"Explicit conversion to {convertTo} not supported");
                    break;
            }
        }

        private void InvalidType() => Compiler.Error($"Invalid operand type: {Op.GetToken()}{Rhs.Type}");

        void INode.Accept(INodeVisitor visitor) => visitor.VisitUnaryOp(this);
    }

    /// <inheritdoc />
    /// <summary>
    /// An abstract base class for all binary operators.
    /// </summary>
    public abstract class BinOp : IEvaluable
    {
        public VarType Type { get; protected set; }
        public readonly IEvaluable Lhs, Rhs;

        protected BinOp(IEvaluable lhs, IEvaluable rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }

        public abstract void Accept(INodeVisitor visitor);

        protected void InvalidType(string op) =>
            Compiler.Error($"Invalid operand types: {Lhs.Type} {op} {Rhs.Type}");
    }

    public class MathOp : BinOp
    {
        public enum OpType
        {
            [Token("+")] Add,
            [Token("-")] Sub,
            [Token("*")] Mult,
            [Token("/")] Div,
            [Token("&")] BitAnd,
            [Token("|")] BitOr,
        }

        public readonly OpType Op;
        public bool Conversion { get; }

        public MathOp(OpType op, IEvaluable lhs, IEvaluable rhs) : base(lhs, rhs)
        {
            Op = op;
            if (op == OpType.BitOr || op == OpType.BitAnd) // Bit operators only accept Integers as operands
            {
                if (lhs.Type != VarType.Integer || rhs.Type != VarType.Integer)
                    InvalidType();

                Type = VarType.Integer;
            }
            else
            {
                if (lhs.Type == VarType.Bool || rhs.Type == VarType.Bool)
                {
                    InvalidType();
                    Type = VarType.Bool; // Set something to allow for error recovery
                }
                else if (lhs.Type != rhs.Type) // Only + - * /
                {
                    Conversion = true;
                    Type = VarType.Double;
                }
                else
                {
                    Type = lhs.Type;
                }
            }
        }

        private void InvalidType() => InvalidType(Op.GetToken());
        public override void Accept(INodeVisitor visitor) => visitor.VisitMathOp(this);
    }

    public class CompOp : BinOp
    {
        public enum OpType
        {
            [Token("==")] Eq,
            [Token("!=")] Neq,
            [Token(">")] Gt,
            [Token(">=")] Gte,
            [Token("<")] Lt,
            [Token("<=")] Lte,
        }

        public readonly OpType Op;
        public VarType CastTo { get; }

        public CompOp(OpType op, IEvaluable lhs, IEvaluable rhs) : base(lhs, rhs)
        {
            Op = op;
            Type = VarType.Bool;

            if (op == OpType.Eq || op == OpType.Neq)
            {
                if (lhs.Type == rhs.Type)
                    return;

                if (lhs.Type == VarType.Bool || rhs.Type == VarType.Bool)
                    InvalidType();
                else
                    CastTo = VarType.Double;
            }
            else
            {
                if (lhs.Type == VarType.Bool || rhs.Type == VarType.Bool)
                    InvalidType();
                else if (lhs.Type != rhs.Type)
                    CastTo = VarType.Double;
            }
        }

        private void InvalidType() => InvalidType(Op.GetToken());
        public override void Accept(INodeVisitor visitor) => visitor.VisitCompOp(this);
    }

    public class LogicOp : BinOp
    {
        public enum OpType
        {
            [Token("&&")] And,
            [Token("||")] Or
        }

        public readonly OpType Op;

        public LogicOp(OpType op, IEvaluable lhs, IEvaluable rhs) : base(lhs, rhs)
        {
            Op = op;
            Type = VarType.Bool;

            if (Lhs.Type != VarType.Bool || Rhs.Type != VarType.Bool)
                InvalidType();
        }

        private void InvalidType() => InvalidType(Op.GetToken());
        public override void Accept(INodeVisitor visitor) => visitor.VisitLogicOp(this);
    }

    public class Assignment : IEvaluable
    {
        public Identifier Lhs { get; }
        public IEvaluable Rhs { get; }
        public bool Conversion { get; }

        public VarType Type => Lhs.Type;

        public Assignment(Identifier identifier, IEvaluable rhs)
        {
            Lhs = identifier;
            Rhs = rhs;

            if (Lhs.Type == VarType.Double && Rhs.Type == VarType.Integer)
                Conversion = true;
            else if (Lhs.Type != Rhs.Type)
                Compiler.Error(
                    $"Cannot assign value of type {Rhs.Type} to a variable of type {Lhs.Type}");
        }

        void INode.Accept(INodeVisitor visitor) => visitor.VisitAssignment(this);
    }

    #endregion

    #region Statements

    public class ExprStatement : INode
    {
        public IEvaluable Expression { get; }
        public ExprStatement(IEvaluable expression) => Expression = expression;
        void INode.Accept(INodeVisitor visitor) => visitor.VisitExprStatement(this);
    }

    public class Declaration : INode
    {
        public readonly Identifier Identifier;
        public readonly VarType Type;

        public Declaration(VarType vType, Identifier identifier)
        {
            Type = vType;
            Identifier = identifier;
        }

        void INode.Accept(INodeVisitor visitor) => visitor.VisitDeclaration(this);
    }

    public class Write : INode
    {
        public readonly IEvaluable Rhs;

        public Write(IEvaluable rhs) => Rhs = rhs;

        void INode.Accept(INodeVisitor visitor) => visitor.VisitWrite(this);
    }

    public class Read : INode
    {
        public readonly Identifier Target;

        public Read(Identifier target) => Target = target;

        void INode.Accept(INodeVisitor visitor) => visitor.VisitRead(this);
    }

    public class Return : INode
    {
        void INode.Accept(INodeVisitor visitor) => visitor.VisitReturn();
    }

    public class Break : INode
    {
        public readonly int Levels;

        public Break(int levels)
        {
            if (levels <= 0)
                Compiler.Error("Break level not positive");

            Levels = levels;
        }

        void INode.Accept(INodeVisitor visitor) => visitor.VisitBreak(this);
    }

    public class Continue : INode
    {
        void INode.Accept(INodeVisitor visitor) => visitor.VisitContinue();
    }

    public class While : INode
    {
        public readonly IEvaluable Condition;
        public readonly INode Body;

        public While(IEvaluable condition, INode body)
        {
            Condition = condition;
            Body = body;

            if (condition.Type != VarType.Bool)
                Compiler.Error(
                    $"While loop condition must evaluate to {VarType.Bool}, not {condition.Type}");
        }

        void INode.Accept(INodeVisitor visitor) => visitor.VisitWhile(this);
    }

    public class IfElse : INode
    {
        public readonly IEvaluable Condition;
        public readonly INode ThenBlock;
        public readonly INode ElseBlock;

        public IfElse(IEvaluable condition, INode thenBlock, INode elseBlock = null)
        {
            Condition = condition;
            ThenBlock = thenBlock;
            ElseBlock = elseBlock;
        }

        void INode.Accept(INodeVisitor visitor) => visitor.VisitIfElse(this);
    }

    public class Program : INode
    {
        public Block MainBlock { get; }
        public Program(INode mainBlock) => MainBlock = mainBlock as Block;
        public void Accept(INodeVisitor visitor) => visitor.VisitProgram(this);
    }

    #endregion

    #endregion

    #region Visitors

    public interface INodeVisitor
    {
        // Primitives
        void VisitIdentifier(Identifier identifier);
        void VisitConstant(Constant constant);
        void VisitProgram(Program program);

        // Statements
        void VisitBlock(Block block);
        void VisitExprStatement(ExprStatement exprStatement);
        void VisitDeclaration(Declaration declaration);
        void VisitAssignment(Assignment assignment);
        void VisitWrite(Write write);
        void VisitRead(Read read);
        void VisitWhile(While @while);
        void VisitIfElse(IfElse ifElse);
        void VisitBreak(Break @break);
        void VisitContinue();
        void VisitReturn();

        // Operators
        void VisitMathOp(MathOp mathOp);
        void VisitCompOp(CompOp compOp);
        void VisitLogicOp(LogicOp logicOp);
        void VisitUnaryOp(UnaryOp unaryOp);
    }

    public class CilBuilder : INodeVisitor
    {
        private readonly StreamWriter _sw;
        private Stack<(string, string)> _loopLabels = new Stack<(string, string)>();
        private int _labelNum;

        /// <summary>
        /// A computed property used for generating unique labels.
        /// </summary>
        /// <value>A unique label</value>
        private string Label => $"LABEL_{_labelNum++}";

        /// <summary>
        /// Path to the generated CIL file.
        /// </summary>
        public string OutputFile { get; }

        public CilBuilder(string file)
        {
            OutputFile = file + ".il";
            _sw = new StreamWriter(OutputFile);
        }

        private void EmitLine(string code) => _sw.WriteLine(code);

        public void VisitProgram(Program program)
        {
            EmitPrologue();
            program.MainBlock.Accept(this);
            EmitEpilogue();
            _sw.Flush();
            _sw.Close();
        }

        public void VisitBlock(Block block) => block.Statements.ForEach(x => x.Accept(this));

        public void VisitExprStatement(ExprStatement exprStatement)
        {
            exprStatement.Expression.Accept(this);
            EmitLine("pop");
        }

        public void VisitIdentifier(Identifier identifier) => EmitLine($"ldloc {identifier.Name}");

        public void VisitDeclaration(Declaration declaration)
        {
            switch (declaration.Type)
            {
                case VarType.BoolT _:
                    EmitLine($".locals init ( bool {declaration.Identifier.Name} )");
                    break;
                case VarType.IntegerT _:
                    EmitLine($".locals init ( int32 {declaration.Identifier.Name} )");
                    break;
                case VarType.DoubleT _:
                    EmitLine($".locals init ( float64 {declaration.Identifier.Name} )");
                    break;
            }
        }

        public void VisitConstant(Constant constant)
        {
            switch (constant.Type)
            {
                case VarType.IntegerT _:
                    EmitLine($"ldc.i4 {constant.Value}");
                    break;
                case VarType.DoubleT _:
                    EmitLine($"ldc.r8 {constant.Value}");
                    break;
                case VarType.BoolT _:
                    EmitLine(constant.Value == "true" ? "ldc.i4.1" : "ldc.i4.0");
                    break;
                case VarType.StringT _:
                    EmitLine($"ldstr {constant.Value}");
                    break;
            }
        }

        private void EmitConversion(VarType targetType)
        {
            switch (targetType)
            {
                case VarType.DoubleT _:
                    EmitLine("conv.r8");
                    break;
                case VarType.BoolT _: // TODO: Most likely unused
                case VarType.IntegerT _:
                    EmitLine("conv.i4");
                    break;
            }
        }

        public void VisitAssignment(Assignment assignment)
        {
            assignment.Rhs.Accept(this);

            if (assignment.Conversion)
                EmitConversion(assignment.Lhs.Type);

            EmitLine("dup");
            EmitLine($"stloc {assignment.Lhs.Name}");
        }

        public void VisitWrite(Write write)
        {
            switch (write.Rhs.Type)
            {
                case VarType.IntegerT _:
                    write.Rhs.Accept(this);
                    EmitLine("call void [mscorlib]System.Console::Write(int32)");
                    break;
                case VarType.DoubleT _:
                    EmitLine(
                        "call class [mscorlib]System.Globalization.CultureInfo class [mscorlib]System.Globalization.CultureInfo::get_InvariantCulture()");
                    EmitLine("ldstr \"{0:0.000000}\"");

                    write.Rhs.Accept(this);

                    EmitLine("box [mscorlib]System.Double");
                    EmitLine("call string string::Format(class [mscorlib]System.IFormatProvider, string, object)");
                    EmitLine("call void [mscorlib]System.Console::Write(string)");
                    break;
                case VarType.BoolT _:
                    write.Rhs.Accept(this);
                    EmitLine("call void [mscorlib]System.Console::Write(bool)");
                    break;
                case VarType.StringT _:
                    write.Rhs.Accept(this);
                    EmitLine("call void [mscorlib]System.Console::Write(string)");
                    break;
            }
        }

        public void VisitRead(Read read)
        {
            EmitLine("call string class [mscorlib]System.Console::ReadLine()");
            EmitLine($"ldloca {read.Target.Name}");

            switch (read.Target.Type)
            {
                case VarType.IntegerT _:
                    EmitLine("call bool int32::TryParse(string, [out] int32&)");
                    break;
                case VarType.DoubleT _:
                    EmitLine("call bool float64::TryParse(string, [out] float64&)");
                    break;
                case VarType.BoolT _:
                    EmitLine("call bool bool::TryParse(string, [out] bool&)");
                    break;
            }

            EmitLine("pop");
        }

        public void VisitBreak(Break @break) => EmitLine($"br {_loopLabels.ElementAt(@break.Levels - 1).Item2}");
        public void VisitContinue() => EmitLine($"br {_loopLabels.Peek().Item1}");
        public void VisitReturn() => EmitLine("leave EndMain");

        public void VisitWhile(While @while)
        {
            string startWhile = Label, endWhile = Label;

            EmitLine($"{startWhile}:");
            @while.Condition.Accept(this);
            EmitLine($"brfalse {endWhile}");

            _loopLabels.Push((startWhile, endWhile));
            @while.Body.Accept(this);
            _loopLabels.Pop();

            EmitLine($"br {startWhile}");
            EmitLine($"{endWhile}:");
        }

        public void VisitIfElse(IfElse ifElse)
        {
            string elseLabel = Label;

            ifElse.Condition.Accept(this);
            EmitLine($"brfalse {elseLabel}");
            ifElse.ThenBlock.Accept(this);

            if (ifElse.ElseBlock != null)
            {
                string endLabel = Label;
                EmitLine($"br {endLabel}");
                EmitLine($"{elseLabel}:");
                ifElse.ElseBlock.Accept(this);
                EmitLine($"{endLabel}:");
            }
            else
            {
                EmitLine($"{elseLabel}:");
            }
        }

        public void VisitMathOp(MathOp mathOp)
        {
            mathOp.Lhs.Accept(this);

            if (mathOp.Conversion && mathOp.Lhs.Type != mathOp.Type)
                EmitConversion(mathOp.Type);

            mathOp.Rhs.Accept(this);

            if (mathOp.Conversion && mathOp.Rhs.Type != mathOp.Type)
                EmitConversion(mathOp.Type);

            switch (mathOp.Op)
            {
                case MathOp.OpType.Add:
                    EmitLine("add");
                    break;
                case MathOp.OpType.Sub:
                    EmitLine("sub");
                    break;
                case MathOp.OpType.Mult:
                    EmitLine("mul");
                    break;
                case MathOp.OpType.Div:
                    EmitLine("div");
                    break;
                case MathOp.OpType.BitAnd:
                    EmitLine("and");
                    break;
                case MathOp.OpType.BitOr:
                    EmitLine("or");
                    break;
            }
        }

        public void VisitCompOp(CompOp compOp)
        {
            compOp.Lhs.Accept(this);

            if (compOp.CastTo is VarType type && compOp.Lhs.Type != type)
                EmitConversion(type);


            compOp.Rhs.Accept(this);

            if (compOp.CastTo is VarType type2 && compOp.Rhs.Type != type2)
                EmitConversion(type2);

            switch (compOp.Op)
            {
                case CompOp.OpType.Eq:
                    EmitLine("ceq");
                    break;
                case CompOp.OpType.Neq:
                    EmitLine("ceq\nldc.i4.0\nceq");
                    break;
                case CompOp.OpType.Gt:
                    EmitLine("cgt");
                    break;
                case CompOp.OpType.Gte:
                    EmitLine("clt\nldc.i4.0\nceq");
                    break;
                case CompOp.OpType.Lt:
                    EmitLine("clt");
                    break;
                case CompOp.OpType.Lte:
                    EmitLine("cgt\nldc.i4.0\nceq");
                    break;
            }
        }

        public void VisitLogicOp(LogicOp logicOp)
        {
            string label = Label;
            logicOp.Lhs.Accept(this);
            EmitLine("dup");
            EmitLine(logicOp.Op == LogicOp.OpType.And ? $"brfalse {label}" : $"brtrue {label}");
            EmitLine("pop");
            logicOp.Rhs.Accept(this);
            EmitLine($"{label}:");
        }

        public void VisitUnaryOp(UnaryOp unaryOp)
        {
            unaryOp.Rhs.Accept(this);

            switch (unaryOp.Op)
            {
                case UnaryOp.OpType.BitwiseNot:
                    EmitLine("not");
                    break;
                case UnaryOp.OpType.LogicalNot:
                    EmitLine("ldc.i4.0\nceq");
                    break;
                case UnaryOp.OpType.IntNegate:
                    EmitLine("neg");
                    break;
                case UnaryOp.OpType.Conv2Int:
                    EmitConversion(VarType.Integer);
                    break;
                case UnaryOp.OpType.Conv2Double:
                    EmitConversion(VarType.Double);
                    break;
            }
        }

        private void EmitPrologue()
        {
            EmitLine(".assembly extern mscorlib { }");
            EmitLine(".assembly minilang { }");
            EmitLine(".method static void main()");
            EmitLine("{");
            EmitLine(".entrypoint");
            EmitLine(".try");
            EmitLine("{");
        }

        private void EmitEpilogue()
        {
            EmitLine("leave EndMain");
            EmitLine("}");
            EmitLine("catch [mscorlib]System.Exception");
            EmitLine("{");
            EmitLine("callvirt instance string [mscorlib]System.Exception::get_Message()");
            EmitLine("call void [mscorlib]System.Console::WriteLine(string)");
            EmitLine("leave EndMain");
            EmitLine("}");
            EmitLine("EndMain: ret");
            EmitLine("}");
        }
    }

    #endregion

    public class AstBuilder
    {
        private class Variable
        {
            public readonly string Name;
            public readonly VarType Type;
            public bool Initialized;
            public readonly bool CanBeShadowed;

            public Variable(string name, VarType type, bool initialized, bool canBeShadowed)
            {
                Name = name;
                Type = type;
                Initialized = initialized;
                CanBeShadowed = canBeShadowed;
            }
        }

        private static int _uniqueId;

        // TODO: assumes '_' will never be allowed in identifiers
        private static string UniqueId(string id) => $"{id}_{_uniqueId++}";

        private readonly Stack<Dictionary<string, Variable>> _scopeStack;

        private Dictionary<string, Variable> CurrentScope => _scopeStack.Count > 0 ? _scopeStack.Peek() : null;

        public AstBuilder() => _scopeStack = new Stack<Dictionary<string, Variable>>();

        private int _loopLevel;

        public Identifier CreateIdentifier(string name)
        {
            if (CurrentScope.ContainsKey(name))
            {
                if (!CurrentScope[name].Initialized)
                    Compiler.Error($"Attempting to access uninitialized variable {name}");

                return new Identifier(CurrentScope[name].Name, CurrentScope[name].Type);
            }

            Compiler.Error($"Variable {name} has not been declared, assuming {VarType.Integer}");
            return new Identifier(name, VarType.Integer);
        }

        public Declaration CreateDeclaration(string name, VarType type)
        {
            if (CurrentScope.ContainsKey(name))
            {
                if (CurrentScope[name].CanBeShadowed)
                {
                    // Shadow an existing variable
                    // Actually declares another one
                    // TODO: issue a warning?
                    string trueName = UniqueId(name);
                    CurrentScope[name] = new Variable(trueName, type, false, false);

                    // Skips the initialization check
                    // TODO: do not initialize the value (skip "init" in codegen)
                    return new Declaration(type, new Identifier(trueName, type));
                }

                Compiler.Error($"Redeclaration of variable {name}");
                return null;
            }

            // Declare a new variable
            CurrentScope[name] = new Variable(name, type, _scopeStack.Count == 1, false);
            return new Declaration(type, new Identifier(name, type));
        }

        public Assignment CreateAssignment(string name, IEvaluable rhs)
        {
            if (CurrentScope.ContainsKey(name))
            {
                CurrentScope[name].Initialized = true;
            }

            return new Assignment(CreateIdentifier(name), rhs);
        }

        public Break CreateBreak(IEvaluable rhs)
        {
            if (rhs is Constant c)
            {
                int.TryParse(c.Value, out int levels);

                if (levels > _loopLevel)
                    Compiler.Error($"Trying to break out of {levels} loops in {_scopeStack.Count - 1} nested loops");

                return new Break(levels);
            }

            Compiler.Error("Break level not an integer literal"); // TODO: this should never happen
            return new Break(1);
        }

        public Continue CreateContinue()
        {
            if (_loopLevel < 1)
                Compiler.Error($"Continue statement outside of a loop");

            return new Continue();
        }

        public void PushScope()
        {
            if (CurrentScope != null)
            {
                var newScope = CurrentScope.ToDictionary(e => e.Key,
                    e => new Variable(e.Value.Name, e.Value.Type, true, true));
                _scopeStack.Push(newScope);
            }
            else
            {
                _scopeStack.Push(new Dictionary<string, Variable>());
            }
        }

        public void PopScope() => _scopeStack.Pop();
        public void PushLoop() => ++_loopLevel;
        public void PopLoop() => --_loopLevel;
    }

    #region Main

    /// <inheritdoc />
    /// <summary>
    /// An exception thrown when the AST builder can no longer proceed. 
    /// </summary>
    public class AstException : Exception // TODO: maybe unused
    {
        /// <inheritdoc cref="AstException"/>
        public AstException(string message) : base(message)
        {
        }
    }

    /// <summary>
    /// Main <see cref="Compiler"/> class.
    /// Defines the <see cref="Compile"/> method, which encapsulates the compiler's logic.
    /// </summary>
    public static class Compiler
    {
        /// <summary>
        /// A delegate defining methods used to report compilation errors.
        /// </summary>
        /// <param name="message">A message to be displayed</param>
        /// <param name="interrupt">A flag indicating whether to hat the execution of the program.</param>
        /// <exception cref="AstException">Thrown when <paramref name="interrupt"/> is set</exception>
        public delegate void ErrorLogger(string message, bool interrupt = false);

        /// <inheritdoc cref="ErrorLogger"/>
        /// <remarks>
        /// This should only be assigned once, in the <see cref="Compile"/> method of the <see cref="Compiler"/> class.
        /// Using a thread-static field prevents parallel tests from overwriting each other's delegate,
        /// and by doing so, messing with the error count for a given test.
        /// </remarks>
        /// <seealso cref="ThreadStaticAttribute"/>
        [ThreadStatic] public static ErrorLogger Error;

        public static (Program, int) Compile(string file)
        {
            var source = new FileStream(file, FileMode.Open);

            var builder = new AstBuilder();
            var scanner = new Scanner(source);
            var parser = new Parser(scanner, builder);

            var errors = 0;

            Error = (message, interrupt) =>
            {
                // Hack (or is it?): make use of line tracking in the scanner
                string errorMessage = $"{message} on line {scanner.lineNumber}";
                Console.Error.WriteLine(errorMessage);

                // This is safe since we're modifying a static closure using a thread-static delegate
                // IDE-specific warning suppression below:
                // ReSharper disable once AccessToModifiedClosure
                errors++;

                if (interrupt) // TODO: Maybe unused
                    throw new AstException(errorMessage);
            };

            try
            {
                parser.Parse();
            }
            catch (AstException e)
            {
                Console.Error.WriteLine(e.Message);
                Console.Error.WriteLine($"{errors} errors found"); // TODO: Same thing as in Main because of early exit
                Environment.Exit(1);
            }

            // Also count errors reported by the lexing and parsing classes
            errors += scanner.Errors;

            return (parser.Program, errors);
        }

        [System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage]
        public static int Main(string[] args)
        {
            string file;

            if (args.Length >= 1)
            {
                file = args[0];
            }
            else
            {
                Console.Write("Source file: ");
                file = Console.ReadLine();
            }

            (Program program, int errors) = Compile(file);

            if (errors > 0)
            {
                Console.Error.WriteLine($"{errors} errors found");
                return 1;
            }

            var generator = new CilBuilder(file);
            program.Accept(generator);

            return 0;
        }
    }

    #endregion
}