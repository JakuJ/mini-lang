using System;
using System.Collections.Generic;
using System.IO;
using GardensPoint;

namespace mini_lang
{
    #region Common

    /// <summary>
    /// Defines data types available in the language.
    /// </summary>
    public enum VarType
    {
        Integer,
        Double,
        Bool,
        String
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
            BitwiseNot,
            LogicalNot,
            IntNegate,
            Conv2Int,
            Conv2Double
        }

        public readonly OpType Op;
        public readonly IEvaluable Rhs;

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
            }
        }

        public UnaryOp(VarType convertTo, IEvaluable rhs)
        {
            Type = convertTo;
            Rhs = rhs;

            switch (convertTo)
            {
                case VarType.Integer:
                    Op = OpType.Conv2Int;
                    break;
                case VarType.Double:
                    Op = OpType.Conv2Double;
                    break;
                default:
                    Compiler.Error($"Explicit conversion to {convertTo} not supported");
                    break;
            }
        }

        private void InvalidType() => Compiler.Error($"Invalid operand type: {Op} {Rhs.Type}");

        void INode.Accept(INodeVisitor visitor) => visitor.VisitUnaryOp(this);
    }

    /// <inheritdoc />
    /// <summary>
    /// An abstract base class for all binary operators.
    /// </summary>
    public abstract class BinOp : IEvaluable
    {
        public VarType Type { get; protected set; }

        public readonly string Op;
        public readonly IEvaluable Lhs, Rhs;

        protected BinOp(string op, IEvaluable lhs, IEvaluable rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
            Op = op;
        }

        public abstract void Accept(INodeVisitor visitor);

        protected void InvalidType() => Compiler.Error($"Invalid operand types: {Lhs.Type} {Op} {Rhs.Type}");
    }

    public class MathOp : BinOp
    {
        public bool Conversion { get; }

        public MathOp(string op, IEvaluable lhs, IEvaluable rhs) : base(op, lhs, rhs)
        {
            if (op == "|" || op == "&") // Bit operators only accept Integers as operands
            {
                if (lhs.Type != VarType.Integer || rhs.Type != VarType.Integer)
                {
                    InvalidType();
                }

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

        public override void Accept(INodeVisitor visitor) => visitor.VisitMathOp(this);
    }

    public class CompOp : BinOp
    {
        public VarType? CastTo { get; }

        public CompOp(string op, IEvaluable lhs, IEvaluable rhs) : base(op, lhs, rhs)
        {
            Type = VarType.Bool;

            if (Op == "==" || Op == "!=")
            {
                if (lhs.Type == rhs.Type) return;
                if (lhs.Type == VarType.Bool || rhs.Type == VarType.Bool)
                {
                    InvalidType();
                }
                else
                {
                    CastTo = VarType.Double;
                }
            }
            else
            {
                if (lhs.Type == VarType.Bool || rhs.Type == VarType.Bool)
                {
                    InvalidType();
                }
                else if (lhs.Type != rhs.Type)
                {
                    CastTo = VarType.Double;
                }
            }
        }

        public override void Accept(INodeVisitor visitor) => visitor.VisitCompOp(this);
    }

    public class LogicOp : BinOp
    {
        public LogicOp(string op, IEvaluable lhs, IEvaluable rhs) : base(op, lhs, rhs)
        {
            Type = VarType.Bool;

            if (Lhs.Type != VarType.Bool || Rhs.Type != VarType.Bool)
            {
                InvalidType();
            }
        }

        public override void Accept(INodeVisitor visitor) => visitor.VisitLogicOp(this);
    }

    #endregion

    #region Statements

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

    public class Assignment : INode
    {
        public readonly Identifier Lhs;
        public readonly IEvaluable Rhs;
        public readonly bool Conversion;

        public Assignment(Identifier identifier, IEvaluable rhs)
        {
            Lhs = identifier;
            Rhs = rhs;

            if (Lhs.Type == VarType.Double && Rhs.Type == VarType.Integer)
            {
                Conversion = true;
            }
            else if (Lhs.Type != Rhs.Type)
            {
                Compiler.Error($"Cannot assign value of type {Rhs.Type} to a variable of type {Lhs.Type}");
            }
        }

        void INode.Accept(INodeVisitor visitor) => visitor.VisitAssignment(this);
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
        void INode.Accept(INodeVisitor visitor) => visitor.VisitReturn(this);
    }

    public class Program : INode
    {
        public List<INode> Instructions { get; }

        public Program() => Instructions = new List<INode>();

        public void Accept(INodeVisitor visitor) => visitor.VisitProgram(this);
    }

    #endregion

    #endregion

    #region Visitors

    public interface INodeVisitor
    {
        // Statements
        void VisitProgram(Program program);
        void VisitIdentifier(Identifier identifier);
        void VisitDeclaration(Declaration declaration);
        void VisitAssignment(Assignment assignment);
        void VisitConstant(Constant constant);
        void VisitWrite(Write write);
        void VisitRead(Read read);
        void VisitReturn(Return ret);

        // Operators
        void VisitMathOp(MathOp mathOp);
        void VisitCompOp(CompOp compOp);
        void VisitLogicOp(LogicOp logicOp);
        void VisitUnaryOp(UnaryOp unaryOp);
    }

    public class AstBuilder
    {
        public Program Program { get; private set; }

        private readonly Dictionary<string, VarType> _declared;

        public AstBuilder() => _declared = new Dictionary<string, VarType>();

        public Identifier CreateIdentifier(string name)
        {
            if (_declared.ContainsKey(name)) return new Identifier(name, _declared[name]);

            Compiler.Error($"Variable {name} has not been declared, assuming Integer type");
            return new Identifier(name, VarType.Integer);
        }

        public void AddProgram() => Program = new Program();

        private void Append(INode node) => Program.Instructions.Add(node);

        public void AddDeclaration(string name, VarType type)
        {
            if (_declared.ContainsKey(name))
            {
                Compiler.Error($"Redeclaration of variable {name}");
            }
            else
            {
                // Declare a new Identifier
                _declared[name] = type;
                Append(new Declaration(type, CreateIdentifier(name)));
            }
        }

        public void AddAssignment(string name, IEvaluable value) =>
            Append(new Assignment(CreateIdentifier(name), value));

        public void AddWrite(IEvaluable node) => Append(new Write(node));

        public void AddRead(string name) => Append(new Read(CreateIdentifier(name)));

        public void AddReturn() => Append(new Return());
    }

    public class CilBuilder : INodeVisitor
    {
        private readonly StreamWriter _sw;
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
            program.Instructions.ForEach(x => x.Accept(this));
            EmitEpilogue();
            _sw.Flush();
            _sw.Close();
        }

        public void VisitIdentifier(Identifier identifier) => EmitLine($"ldloc {identifier.Name}");

        public void VisitDeclaration(Declaration declaration)
        {
            switch (declaration.Type)
            {
                case VarType.Bool:
                {
                    EmitLine($".locals init ( bool {declaration.Identifier.Name} )");
                    break;
                }
                case VarType.Integer:
                {
                    EmitLine($".locals init ( int32 {declaration.Identifier.Name} )");
                    break;
                }
                case VarType.Double:
                {
                    EmitLine($".locals init ( float64 {declaration.Identifier.Name} )");
                    break;
                }
            }
        }

        public void VisitConstant(Constant constant)
        {
            switch (constant.Type)
            {
                case VarType.Integer:
                    EmitLine($"ldc.i4 {constant.Value}");
                    break;
                case VarType.Double:
                    EmitLine($"ldc.r8 {constant.Value}");
                    break;
                case VarType.Bool:
                    EmitLine(constant.Value == "true" ? "ldc.i4.1" : "ldc.i4.0");
                    break;
                case VarType.String:
                    EmitLine($"ldstr {constant.Value}");
                    break;
            }
        }

        private void EmitConversion(VarType targetType)
        {
            switch (targetType)
            {
                case VarType.Double:
                    EmitLine("conv.r8");
                    break;
                case VarType.Bool: // TODO: Most likely unused
                case VarType.Integer:
                    EmitLine("conv.i4");
                    break;
            }
        }

        public void VisitAssignment(Assignment assignment)
        {
            assignment.Rhs.Accept(this);

            if (assignment.Conversion)
            {
                EmitConversion(assignment.Lhs.Type);
            }

            EmitLine($"stloc {assignment.Lhs.Name}");
        }

        public void VisitWrite(Write write)
        {
            switch (write.Rhs.Type)
            {
                case VarType.Integer:
                    write.Rhs.Accept(this);
                    EmitLine("call void [mscorlib]System.Console::Write(int32)");
                    break;
                case VarType.Double:
                    EmitLine(
                        "call class [mscorlib]System.Globalization.CultureInfo class [mscorlib]System.Globalization.CultureInfo::get_InvariantCulture()");
                    EmitLine("ldstr \"{0:0.000000}\"");

                    write.Rhs.Accept(this);

                    EmitLine("box [mscorlib]System.Double");
                    EmitLine("call string string::Format(class [mscorlib]System.IFormatProvider, string, object)");
                    EmitLine("call void [mscorlib]System.Console::Write(string)");
                    break;
                case VarType.Bool:
                    write.Rhs.Accept(this);
                    EmitLine("call void [mscorlib]System.Console::Write(bool)");
                    break;
                case VarType.String:
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
                case VarType.Integer:
                    EmitLine("call bool int32::TryParse(string, [out] int32&)");
                    break;
                case VarType.Double:
                    EmitLine("call bool float64::TryParse(string, [out] float64&)");
                    break;
                case VarType.Bool:
                    EmitLine("call bool bool::TryParse(string, [out] bool&)");
                    break;
            }

            EmitLine("pop");
        }

        public void VisitReturn(Return ret) => EmitLine("leave EndMain");

        public void VisitMathOp(MathOp mathOp)
        {
            mathOp.Lhs.Accept(this);

            if (mathOp.Conversion && mathOp.Lhs.Type != mathOp.Type)
            {
                EmitConversion(mathOp.Type);
            }

            mathOp.Rhs.Accept(this);

            if (mathOp.Conversion && mathOp.Rhs.Type != mathOp.Type)
            {
                EmitConversion(mathOp.Type);
            }

            var opToOpcode = new Dictionary<string, string>()
            {
                {"+", "add"},
                {"-", "sub"},
                {"*", "mul"},
                {"/", "div"},
                {"|", "or"},
                {"&", "and"},
            };

            EmitLine(opToOpcode[mathOp.Op]);
        }

        public void VisitCompOp(CompOp compOp)
        {
            compOp.Lhs.Accept(this);

            if (compOp.CastTo is VarType type && compOp.Lhs.Type != type)
            {
                EmitConversion(type);
            }

            compOp.Rhs.Accept(this);

            if (compOp.CastTo is VarType type2 && compOp.Rhs.Type != type2)
            {
                EmitConversion(type2);
            }

            var opToOpcode = new Dictionary<string, string>()
            {
                {"==", "ceq"},
                {"!=", "ceq\nldc.i4.0\nceq"}, // not ==
                {"<", "clt"},
                {"<=", "cgt\nldc.i4.0\nceq"}, // not >
                {">", "cgt"},
                {">=", "clt\nldc.i4.0\nceq"}, // not <
            };

            EmitLine(opToOpcode[compOp.Op]);
        }

        public void VisitLogicOp(LogicOp logicOp)
        {
            string label = Label;
            logicOp.Lhs.Accept(this);
            EmitLine("dup");
            EmitLine(logicOp.Op == "&&" ? $"brfalse {label}" : $"brtrue {label}");
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

    [System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage]
    public class PrettyPrinter : INodeVisitor
    {
        public void VisitProgram(Program program)
        {
            Console.WriteLine("program {");
            program.Instructions.ForEach(x => x.Accept(this));
            Console.WriteLine("}");
        }

        public void VisitIdentifier(Identifier identifier) => Console.Write(identifier.Name);

        public void VisitDeclaration(Declaration d) => Console.WriteLine($"{d.Type} {d.Identifier.Name};");

        public void VisitConstant(Constant constant) => Console.Write(constant.Value);

        public void VisitAssignment(Assignment assignment)
        {
            Console.Write($"{assignment.Lhs.Name} = ");
            assignment.Rhs.Accept(this);
            Console.WriteLine(";");
        }

        public void VisitWrite(Write write)
        {
            Console.Write("write ");
            write.Rhs.Accept(this);
            Console.WriteLine(";");
        }

        public void VisitRead(Read read) => Console.WriteLine($"read {read.Target.Name}");

        public void VisitReturn(Return ret) => Console.WriteLine("return;");

        public void VisitUnaryOp(UnaryOp unaryOp)
        {
            Console.Write(unaryOp.Op);
            unaryOp.Rhs.Accept(this);
        }

        private void VisitBinOp(BinOp binOp)
        {
            binOp.Lhs.Accept(this);
            Console.Write($" {binOp.Op} ");
            binOp.Rhs.Accept(this);
        }

        public void VisitMathOp(MathOp mathOp) => VisitBinOp(mathOp);

        public void VisitCompOp(CompOp compOp) => VisitBinOp(compOp);

        public void VisitLogicOp(LogicOp logicOp) => VisitBinOp(logicOp);
    }

    #endregion

    #region Main

    /// <inheritdoc />
    /// <summary>
    /// An exception thrown when the AST builder can no longer proceed. 
    /// </summary>
    public class AstException : Exception
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
                Console.Error.WriteLine($"{message} on line {scanner.lineNumber}");

                // This is safe since we're modifying a static closure using a thread-static delegate
                // IDE-specific warning suppression below:
                // ReSharper disable once AccessToModifiedClosure
                errors++;

                if (interrupt) // TODO: Maybe unused
                {
                    throw new AstException("Fatal error, cannot analyze further");
                }
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

            return (builder.Program, errors);
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

            var printer = new PrettyPrinter();
            program.Accept(printer);

            var generator = new CilBuilder(file);
            program.Accept(generator);

            return 0;
        }
    }

    #endregion
}