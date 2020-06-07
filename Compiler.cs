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
        /// Will get the string value for a given enums value.
        /// This will only work if you assign the StringValue attribute to
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
    /// An abstract class serving as a root of the type system.
    /// Think "object" in C#.
    /// </summary>
    public abstract class AbstractType
    {
        private readonly string _repr;

        protected AbstractType(string repr) => _repr = repr;
        public override string ToString() => _repr;
    }

    /// <inheritdoc />
    /// <summary>
    /// A type-safe enumeration defining all primitive types.
    /// </summary>
    public class PrimType : AbstractType
    {
        public static PrimType Integer { get; } = new IntegerT();
        public static PrimType Double  { get; } = new DoubleT();
        public static PrimType Bool    { get; } = new BoolT();

        private PrimType(string token) : base(token) { }

        public sealed class IntegerT : PrimType
        {
            internal IntegerT() : base("int") { }
        }

        public sealed class DoubleT : PrimType
        {
            internal DoubleT() : base("double") { }
        }

        public sealed class BoolT : PrimType
        {
            internal BoolT() : base("bool") { }
        }
    }

    public class ArrayType : AbstractType
    {
        public int      Dimensions { get; }
        public PrimType ElemType   { get; }

        public ArrayType(PrimType elemType, int dimensions) : base($"{elemType}[{dimensions}]")
        {
            ElemType   = elemType;
            Dimensions = dimensions;

            if (dimensions > 32)
                Compiler.Error("Cannot declare array of more than 32 dimensions");
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
    /// An interface for expressions evaluable to some <see cref="PrimType" />.
    /// </summary>
    public interface IEvaluable : INode
    {
        PrimType EvalType { get; }
    }

    /// <inheritdoc />
    /// <summary>
    /// An interface for expressions that can be assigned to.
    /// </summary>
    public interface IAssignable : IEvaluable
    {
        void Assign(IAssignableVisitor visitor, CodeGenerator value);
    }

    #endregion

    #region AST

    public class Block : INode
    {
        public List<INode> Statements { get; }

        public Block(List<INode> statements) => Statements = statements;
        public void Accept(INodeVisitor visitor) => visitor.VisitBlock(this);
    }

    public class Constant : IEvaluable
    {
        public string   Value    { get; }
        public PrimType EvalType { get; }

        public Constant(string value, PrimType type) => (Value, EvalType) = (value, type);
        void INode.Accept(INodeVisitor visitor) => visitor.VisitConstant(this);
    }

    public readonly struct Identifier
    {
        public string       Name { get; }
        public AbstractType Type { get; }

        public Identifier(string name, AbstractType type) => (Name, Type) = ($"'{name}'", type);
    }

    public class Variable : IAssignable
    {
        public Identifier Identifier { get; }
        public PrimType   EvalType   { get; }

        public Variable(Identifier ident)
        {
            Identifier = ident;
            if (ident.Type is PrimType prim)
                EvalType = prim;
            else
                Compiler.Error("Attempting to use a complex object as a primitive one");
        }

        public void Accept(INodeVisitor visitor) => visitor.VisitVariable(this);
        public void Assign(IAssignableVisitor visitor, CodeGenerator value) => visitor.StoreInVariable(this, value);
    }

    public class Indexing : IAssignable
    {
        public Identifier       Identifier { get; }
        public List<IEvaluable> Indices    { get; }
        public PrimType         EvalType   { get; }

        public Indexing(Identifier identifier, List<IEvaluable> indices)
        {
            Identifier = identifier;
            Indices    = indices;

            if (Identifier.Type is ArrayType arr)
            {
                EvalType = arr.ElemType;
                if (arr.Dimensions != Indices.Count)
                    Compiler.Error($"Invalid {Indices.Count}D index for array of {arr.Dimensions} dimensions");

                Indices.ForEach(size =>
                {
                    if (size.EvalType != PrimType.Integer)
                        Compiler.Error($"{size.EvalType} invalid as an array index – expected {PrimType.Integer}");
                });
            }
            else
                Compiler.Error($"Invalid indexing expression - {Identifier.Name} is not of array type");
        }

        void INode.Accept(INodeVisitor visitor) => visitor.VisitIndexing(this);
        public void Assign(IAssignableVisitor visitor, CodeGenerator value) => visitor.StoreInArray(this, value);
    }

    #region Operators

    public class UnaryOp : IEvaluable
    {
        public enum OpType
        {
            [Token("~")]        BitwiseNot,
            [Token("!")]        LogicalNot,
            [Token("-")]        IntNegate,
            [Token("(int)")]    Conv2Int,
            [Token("(double)")] Conv2Double
        }

        public PrimType EvalType { get; }

        public OpType     Op  { get; }
        public IEvaluable Rhs { get; }

        /// <summary>
        /// Use this constructor for unary operators other than explicit conversions.
        /// </summary>
        /// <param name="op">Operator type - one of <see cref="OpType"/>s</param>
        /// <param name="rhs">Operand</param>
        public UnaryOp(OpType op, IEvaluable rhs)
        {
            Op  = op;
            Rhs = rhs;

            switch (Op)
            {
                case OpType.IntNegate:
                    EvalType = Rhs.EvalType;
                    if (EvalType == PrimType.Bool) InvalidType();
                    break;
                case OpType.BitwiseNot:
                    EvalType = PrimType.Integer;
                    if (Rhs.EvalType != EvalType) InvalidType();
                    break;
                case OpType.LogicalNot:
                    EvalType = PrimType.Bool;
                    if (Rhs.EvalType != EvalType) InvalidType();
                    break;
                default:
                    Compiler.Error($"Invalid type passed to UnaryOp constructor: {Op}", true);
                    break;
            }
        }

        /// <summary>
        /// Use this constructor for explicit conversions.
        /// </summary>
        /// <param name="convertTo">Target type of the conversion.</param>
        /// <param name="rhs">The value to be converted.</param>
        public UnaryOp(PrimType convertTo, IEvaluable rhs)
        {
            EvalType = convertTo;
            Rhs      = rhs;

            if (convertTo is PrimType.IntegerT)
                Op = OpType.Conv2Int;
            else if (convertTo is PrimType.DoubleT)
                Op = OpType.Conv2Double;
            else
                Compiler.Error($"Explicit conversion to {convertTo} not supported");
        }

        private void InvalidType() => Compiler.Error($"Invalid operand type: {Op.GetToken()}{Rhs.EvalType}");
        void INode.Accept(INodeVisitor visitor) => visitor.VisitUnaryOp(this);
    }

    /// <inheritdoc />
    /// <summary>
    /// An abstract base class for all binary operators.
    /// </summary>
    public abstract class BinOp : IEvaluable
    {
        public PrimType   EvalType { get; protected set; }
        public IEvaluable Lhs      { get; }
        public IEvaluable Rhs      { get; }

        protected BinOp(IEvaluable lhs, IEvaluable rhs) => (Lhs, Rhs) = (lhs, rhs);

        protected void InvalidType(string op) =>
            Compiler.Error($"Invalid operand types: {Lhs.EvalType} {op} {Rhs.EvalType}");

        public abstract void Accept(INodeVisitor visitor);
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

        public OpType Op { get; }

        public MathOp(OpType op, IEvaluable lhs, IEvaluable rhs) : base(lhs, rhs)
        {
            Op = op;
            if (op == OpType.BitOr || op == OpType.BitAnd) // Bit operators only accept Integers as operands
            {
                EvalType = PrimType.Integer;
                if (lhs.EvalType != PrimType.Integer || rhs.EvalType != PrimType.Integer)
                    InvalidType(Op.GetToken());
            }
            else // Only + - * /
            {
                if (lhs.EvalType == PrimType.Bool || rhs.EvalType == PrimType.Bool)
                {
                    EvalType = PrimType.Bool; // Set some type to allow for error recovery
                    InvalidType(Op.GetToken());
                }
                else if (lhs.EvalType != rhs.EvalType) // Integers or Doubles
                {
                    EvalType = PrimType.Double;
                }
                else
                {
                    EvalType = lhs.EvalType;
                }
            }
        }

        public override void Accept(INodeVisitor visitor) => visitor.VisitMathOp(this);
    }

    public class CompOp : BinOp
    {
        public enum OpType
        {
            [Token("==")] Eq,
            [Token("!=")] Neq,
            [Token(">")]  Gt,
            [Token(">=")] Gte,
            [Token("<")]  Lt,
            [Token("<=")] Lte,
        }

        public OpType   Op     { get; }
        public PrimType CastTo { get; }

        public CompOp(OpType op, IEvaluable lhs, IEvaluable rhs) : base(lhs, rhs)
        {
            Op       = op;
            EvalType = PrimType.Bool;

            if (op == OpType.Eq || op == OpType.Neq)
            {
                if (lhs.EvalType == rhs.EvalType)
                    return;

                if (lhs.EvalType == PrimType.Bool || rhs.EvalType == PrimType.Bool)
                    InvalidType();
                else
                    CastTo = PrimType.Double;
            }
            else
            {
                if (lhs.EvalType == PrimType.Bool || rhs.EvalType == PrimType.Bool)
                    InvalidType();
                else if (lhs.EvalType != rhs.EvalType)
                    CastTo = PrimType.Double;
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

        public OpType Op { get; }

        public LogicOp(OpType op, IEvaluable lhs, IEvaluable rhs) : base(lhs, rhs)
        {
            Op       = op;
            EvalType = PrimType.Bool;

            if (Lhs.EvalType != PrimType.Bool || Rhs.EvalType != PrimType.Bool)
                InvalidType(Op.GetToken());
        }

        public override void Accept(INodeVisitor visitor) => visitor.VisitLogicOp(this);
    }

    public class Assignment : IEvaluable
    {
        public IAssignable Lhs { get; }
        public IEvaluable  Rhs { get; }

        public PrimType EvalType => Lhs.EvalType;

        public Assignment(IAssignable assignable, IEvaluable rhs)
        {
            Lhs = assignable;
            Rhs = rhs;

            if (EvalType != Rhs.EvalType && !(EvalType == PrimType.Double && Rhs.EvalType == PrimType.Integer))
                Compiler.Error($"Cannot assign a value of type {Rhs.EvalType}, expected {EvalType}");
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
        public Identifier Identifier { get; }

        public Declaration(Identifier identifier) => Identifier = identifier;
        void INode.Accept(INodeVisitor visitor) => visitor.VisitDeclaration(this);
    }

    public class ArrayCreation : INode
    {
        public Indexing Indexing { get; }

        public ArrayCreation(Indexing indexing) => Indexing = indexing;
        void INode.Accept(INodeVisitor visitor) => visitor.VisitArrayCreation(this);
    }

    public class Write : INode
    {
        public IEvaluable Rhs { get; }

        public Write(IEvaluable rhs) => Rhs = rhs;
        void INode.Accept(INodeVisitor visitor) => visitor.VisitWrite(this);
    }

    public class WriteString : INode
    {
        public string String { get; }

        public WriteString(string s) => String = s;
        void INode.Accept(INodeVisitor visitor) => visitor.VisitWriteString(this);
    }

    public class Read : INode
    {
        public IAssignable Target { get; }

        public Read(IAssignable target) => Target = target;
        void INode.Accept(INodeVisitor visitor) => visitor.VisitRead(this);
    }

    public class Return : INode
    {
        void INode.Accept(INodeVisitor visitor) => visitor.VisitReturn();
    }

    public class Break : INode
    {
        public int Levels { get; }

        public Break(int levels)
        {
            Levels = levels;
            if (Levels < 1)
                Compiler.Error("Break level not positive");
        }

        void INode.Accept(INodeVisitor visitor) => visitor.VisitBreak(this);
    }

    public class Continue : INode
    {
        void INode.Accept(INodeVisitor visitor) => visitor.VisitContinue();
    }

    public class While : INode
    {
        public IEvaluable Condition { get; }
        public INode      Body      { get; }

        public While(IEvaluable condition, INode body)
        {
            Condition = condition;
            Body      = body;

            if (condition.EvalType != PrimType.Bool)
                Compiler.Error($"While loop condition must evaluate to {PrimType.Bool}, not {condition.EvalType}");
        }

        void INode.Accept(INodeVisitor visitor) => visitor.VisitWhile(this);
    }

    public class IfElse : INode
    {
        public IEvaluable Condition { get; }
        public INode      ThenBlock { get; }
        public INode      ElseBlock { get; }

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
        void VisitProgram(Program program);
        void VisitConstant(Constant constant);
        void VisitVariable(Variable variable);
        void VisitIndexing(Indexing indexing);

        // Statements
        void VisitBlock(Block block);
        void VisitExprStatement(ExprStatement exprStatement);
        void VisitDeclaration(Declaration declaration);
        void VisitAssignment(Assignment assignment);
        void VisitArrayCreation(ArrayCreation arrayCreation);
        void VisitWrite(Write write);
        void VisitWriteString(WriteString writeString);
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

    public delegate void CodeGenerator();

    public interface IAssignableVisitor
    {
        void StoreInVariable(Variable variable, CodeGenerator value);
        void StoreInArray(Indexing indexing, CodeGenerator value);
    }

    public class CilBuilder : INodeVisitor, IAssignableVisitor
    {
        private          int                     _labelNum;
        private readonly StreamWriter            _sw;
        private readonly Stack<(string, string)> _loopLabels = new Stack<(string, string)>();

        // Helper dictionaries
        private readonly Dictionary<PrimType, string> _longTypes = new Dictionary<PrimType, string>
        {
            {PrimType.Integer, "int32"},
            {PrimType.Double, "float64"},
            {PrimType.Bool, "bool"}
        };

        private readonly Dictionary<PrimType, string> _shortTypes = new Dictionary<PrimType, string>
        {
            {PrimType.Integer, "i4"},
            {PrimType.Double, "r8"},
            {PrimType.Bool, "i1"}
        };

        private string UniqueLabel(string prefix) => $"{prefix}_{_labelNum++}";

        public string OutputFile { get; }

        public CilBuilder(string file, string outFile = null)
        {
            OutputFile = outFile ?? file + ".il";
            _sw        = new StreamWriter(OutputFile);
        }

        private void EmitLine(string code) => _sw.WriteLine(code);

        // L-value helper methods

        public void StoreInVariable(Variable variable, CodeGenerator value)
        {
            value();
            EmitLine($"stloc {variable.Identifier.Name}");
        }

        public void StoreInArray(Indexing indexing, CodeGenerator value)
        {
            EmitLine($"ldloc {indexing.Identifier.Name}"); // TODO: same as in VisitVariable
            indexing.Indices.ForEach(ix => ix.Accept(this));

            value();

            int dim = indexing.Indices.Count;

            if (dim == 1)
            {
                EmitLine($"stelem.{_shortTypes[indexing.EvalType]}");
            }
            else
            {
                var zeros = string.Join(",", Enumerable.Repeat("0...", dim));
                var ints  = string.Join(", ", Enumerable.Repeat("int32", dim));
                EmitLine(
                    $"call instance void {_longTypes[indexing.EvalType]}[{zeros}]::Set({ints}, {_longTypes[indexing.EvalType]})");
            }
        }

        // INodeVisitor methods

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

        public void VisitVariable(Variable variable) => EmitLine($"ldloc {variable.Identifier.Name}");

        public void VisitDeclaration(Declaration declaration)
        {
            Identifier ident = declaration.Identifier;

            switch (ident.Type)
            {
                case ArrayType arr when arr.Dimensions == 1:
                    EmitLine($".locals init ( {_longTypes[arr.ElemType]}[] {ident.Name} )");
                    break;
                case ArrayType arr:
                {
                    var zeros = string.Join(",", Enumerable.Repeat("0...", arr.Dimensions));
                    EmitLine($".locals init ( {_longTypes[arr.ElemType]}[{zeros}] {ident.Name} )");
                    break;
                }
                case PrimType prim:
                    EmitLine($".locals init ( {_longTypes[prim]} {ident.Name} )");
                    break;
            }
        }

        public void VisitConstant(Constant constant)
        {
            switch (constant.EvalType)
            {
                case PrimType.IntegerT _:
                    EmitLine($"ldc.i4 {constant.Value}");
                    break;
                case PrimType.DoubleT _:
                    EmitLine($"ldc.r8 {constant.Value}");
                    break;
                case PrimType.BoolT _:
                    EmitLine(constant.Value == "true" ? "ldc.i4.1" : "ldc.i4.0");
                    break;
            }
        }

        private void EmitConversion(PrimType targetType)
        {
            switch (targetType)
            {
                case PrimType.DoubleT _:
                    EmitLine("conv.r8");
                    break;
                case PrimType.BoolT _:
                case PrimType.IntegerT _:
                    EmitLine("conv.i4");
                    break;
            }
        }

        public void VisitAssignment(Assignment assignment)
        {
            void Action()
            {
                assignment.Rhs.Accept(this);
                if (assignment.EvalType != assignment.Rhs.EvalType)
                    EmitConversion(assignment.EvalType);
            }

            Action(); // because assignment leaves a value on the stack
            assignment.Lhs.Assign(this, Action);
        }

        public void VisitIndexing(Indexing indexing)
        {
            EmitLine($"ldloc {indexing.Identifier.Name}"); // TODO: same as in VisitVariable
            indexing.Indices.ForEach(x => x.Accept(this));

            int dim = indexing.Indices.Count;

            if (dim == 1)
            {
                EmitLine($"ldelem.{_shortTypes[indexing.EvalType]}");
            }
            else
            {
                var    zeros = string.Join(",", Enumerable.Repeat("0...", dim));
                var    ints  = string.Join(", ", Enumerable.Repeat("int32", dim));
                string tc    = _longTypes[indexing.EvalType];
                EmitLine($"call instance {tc} {tc}[{zeros}]::Get({ints})");
            }
        }

        public void VisitArrayCreation(ArrayCreation arrayCreation)
        {
            int dim = arrayCreation.Indexing.Indices.Count;
            arrayCreation.Indexing.Indices.ForEach(node => node.Accept(this));

            if (dim == 1)
            {
                var boxedTypes = new Dictionary<PrimType, string>
                {
                    {PrimType.Integer, "Int32"},
                    {PrimType.Double, "Double"},
                    {PrimType.Bool, "Boolean"}
                };
                EmitLine($"newarr [mscorlib]System.{boxedTypes[arrayCreation.Indexing.EvalType]}");
            }
            else
            {
                var zeros = string.Join(",", Enumerable.Repeat("0...", dim));
                var ints  = string.Join(", ", Enumerable.Repeat("int32", dim));
                EmitLine($"newobj instance void {_longTypes[arrayCreation.Indexing.EvalType]}[{zeros}]::.ctor({ints})");
            }

            EmitLine($"stloc {arrayCreation.Indexing.Identifier.Name}");
        }

        public void VisitWrite(Write write)
        {
            switch (write.Rhs.EvalType)
            {
                case PrimType.DoubleT _:
                    EmitLine(
                        "call class [mscorlib]System.Globalization.CultureInfo class [mscorlib]System.Globalization.CultureInfo::get_InvariantCulture()");
                    EmitLine(@"ldstr ""{0:0.000000}""");

                    write.Rhs.Accept(this);

                    EmitLine("box [mscorlib]System.Double");
                    EmitLine("call string string::Format(class [mscorlib]System.IFormatProvider, string, object)");
                    EmitLine("call void [mscorlib]System.Console::Write(string)");
                    break;
                default:
                    write.Rhs.Accept(this);
                    EmitLine($"call void [mscorlib]System.Console::Write({_longTypes[write.Rhs.EvalType]})");
                    break;
            }
        }

        public void VisitWriteString(WriteString writeString)
        {
            EmitLine($"ldstr {writeString.String}");
            EmitLine($"call void [mscorlib]System.Console::Write(string)");
        }

        public void VisitRead(Read read)
        {
            void Action()
            {
                EmitLine("call string class [mscorlib]System.Console::ReadLine()");
                EmitLine($"call {_longTypes[read.Target.EvalType]} {_longTypes[read.Target.EvalType]}::Parse(string)");
            }

            read.Target.Assign(this, Action);
        }

        public void VisitBreak(Break @break) => EmitLine($"br {_loopLabels.ElementAt(@break.Levels - 1).Item2}");
        public void VisitContinue() => EmitLine($"br {_loopLabels.Peek().Item1}");
        public void VisitReturn() => EmitLine("leave EndMain");

        public void VisitWhile(While @while)
        {
            string startWhile = UniqueLabel("WHILE"),
                   endWhile   = UniqueLabel("ENDWHILE");

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
            string elseLabel = UniqueLabel("ELSE");
            ifElse.Condition.Accept(this);
            EmitLine($"brfalse {elseLabel}");
            ifElse.ThenBlock.Accept(this);

            if (ifElse.ElseBlock != null)
            {
                string endLabel = UniqueLabel("ENDIF");
                EmitLine($"br {endLabel}");
                EmitLine($"{elseLabel}:");
                ifElse.ElseBlock.Accept(this);
                EmitLine($"{endLabel}:");
            }
            else
                EmitLine($"{elseLabel}:");
        }

        public void VisitMathOp(MathOp mathOp)
        {
            mathOp.Lhs.Accept(this);
            if (mathOp.Lhs.EvalType != mathOp.EvalType) // Only double / int type mismatch is possible at this point
                EmitConversion(mathOp.EvalType);

            mathOp.Rhs.Accept(this);
            if (mathOp.Rhs.EvalType != mathOp.EvalType)
                EmitConversion(mathOp.EvalType);

            var opcodes = new Dictionary<MathOp.OpType, string>
            {
                {MathOp.OpType.Add, "add"},
                {MathOp.OpType.Sub, "sub"},
                {MathOp.OpType.Mult, "mul"},
                {MathOp.OpType.Div, "div"},
                {MathOp.OpType.BitAnd, "and"},
                {MathOp.OpType.BitOr, "or"},
            };

            EmitLine(opcodes[mathOp.Op]);
        }

        public void VisitCompOp(CompOp compOp)
        {
            compOp.Lhs.Accept(this);
            if (compOp.CastTo is PrimType type && compOp.Lhs.EvalType != type)
                EmitConversion(type);

            compOp.Rhs.Accept(this);
            if (compOp.CastTo is PrimType type2 && compOp.Rhs.EvalType != type2)
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
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        public void VisitLogicOp(LogicOp logicOp)
        {
            string label = UniqueLabel("LOGIC");
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
                    EmitConversion(PrimType.Integer);
                    break;
                case UnaryOp.OpType.Conv2Double:
                    EmitConversion(PrimType.Double);
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
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
        private readonly struct VariableInfo
        {
            internal readonly Identifier Identifier;
            internal readonly bool       CanBeShadowed;

            public VariableInfo(Identifier identifier, bool canBeShadowed)
            {
                Identifier    = identifier;
                CanBeShadowed = canBeShadowed;
            }
        }

        private        int _loopLevel;
        private static int _uniqueId;

        private static string UniqueId(string id) => $"{id}_{_uniqueId++}";

        private readonly Stack<Dictionary<string, VariableInfo>> _scopeStack;
        private Dictionary<string, VariableInfo> CurrentScope => _scopeStack.Count > 0 ? _scopeStack.Peek() : null;

        public AstBuilder() => _scopeStack = new Stack<Dictionary<string, VariableInfo>>();

        public Identifier CreateIdentifier(string name)
        {
            if (CurrentScope.TryGetValue(name, out VariableInfo info))
                return info.Identifier;

            Compiler.Error($"Variable {name} has not been declared, assuming {PrimType.Integer} for further analysis");
            return new Identifier(name, PrimType.Integer);
        }

        public Declaration CreateDeclaration(string name, AbstractType type)
        {
            if (CurrentScope.TryGetValue(name, out VariableInfo info))
            {
                if (info.CanBeShadowed) // Shadow an existing variable
                {
                    // Actually declare another one
                    string uniqueName    = UniqueId(name);
                    var    newIdentifier = new Identifier(uniqueName, type);
                    CurrentScope[name] = new VariableInfo(newIdentifier, false);

                    // TODO: do not initialize the value (skip "init" in code generation)
                    return new Declaration(newIdentifier);
                }

                Compiler.Error($"Redeclaration of variable {name}");
                return null;
            }

            // Declare a new variable
            var ident = new Identifier(name, type);
            CurrentScope[name] = new VariableInfo(ident, false);
            return new Declaration(ident);
        }

        public Break CreateBreak(Constant rhs)
        {
            int.TryParse(rhs.Value, out int levels);

            if (levels > _loopLevel)
                Compiler.Error($"Trying to break out of {levels} loops in {_loopLevel} nested loops");

            return new Break(levels);
        }

        public Continue CreateContinue()
        {
            if (_loopLevel < 1)
                Compiler.Error("Continue statement outside of a loop");

            return new Continue();
        }

        public void PushScope()
        {
            _scopeStack.Push(CurrentScope == null
                                 ? new Dictionary<string, VariableInfo>()
                                 : CurrentScope.ToDictionary(e => e.Key,
                                                             e => new VariableInfo(e.Value.Identifier, true)));
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
        public AstException(string message) : base(message) { }
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
            var parser  = new Parser(scanner, builder);

            var errors        = 0;
            var lastErrorLine = -1;

            Error = (message, interrupt) =>
            {
                // Hack (or is it?): make use of line tracking in the scanner
                int currentLine = scanner.lineNumber;

                if (currentLine == lastErrorLine)
                    return;

                lastErrorLine = currentLine;

                string errorMessage = $"{message} on line {currentLine}";
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