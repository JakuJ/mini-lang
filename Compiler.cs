using System;
using System.Collections.Generic;
using System.IO;
using GardensPoint;

namespace mini_lang
{
    #region Common

    public enum VarType
    {
        Integer,
        Double,
        Bool,
        String
    }

    public interface INode
    {
        void Accept(INodeVisitor visitor);
    }

    public interface IEvaluable : INode
    {
        VarType Type { get; }
    }

    #endregion

    #region AST

    public class Identifier : IEvaluable
    {
        public readonly string Name;

        public VarType Type
        {
            get
            {
                if (!Declaration.Declared.ContainsKey(Name))
                {
                    Compiler.Error($"Variable {Name} has not been declared", true);
                }

                return Declaration.Declared[Name];
            }
        }


        public Identifier(string name)
        {
            if (!Declaration.Declared.ContainsKey(name))
            {
                Compiler.Error($"Variable {name} has not been declared");
            }

            Name = name;
        }

        public void Accept(INodeVisitor visitor)
        {
            visitor.VisitIdentifier(this);
        }
    }

    public class Declaration : INode
    {
        public readonly Identifier Identifier;
        public readonly VarType Type;

        public static Dictionary<string, VarType> Declared = new Dictionary<string, VarType>();

        public Declaration(VarType vType, string name)
        {
            if (Declared.ContainsKey(name))
            {
                Compiler.Error($"Redeclaration of variable {name}");
            }

            Type = vType;

            Declared[name] = Type; // Do not raise an "undeclared variable" exception in Identifier
            Identifier = new Identifier(name);
        }

        public void Accept(INodeVisitor visitor)
        {
            visitor.VisitDeclaration(this);
        }
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

        public void Accept(INodeVisitor visitor)
        {
            visitor.VisitConstant(this);
        }
    }

    #region BinOps

    public abstract class BinOp : IEvaluable
    {
        public VarType Type { get; protected set; }

        public readonly string Op;
        public bool Conversion { get; protected set; }
        public readonly IEvaluable Lhs, Rhs;

        protected BinOp(string op, IEvaluable lhs, IEvaluable rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
            Op = op;
        }

        public abstract void Accept(INodeVisitor visitor);

        protected static void InvalidType(string op, VarType type) =>
            Compiler.Error($"Invalid type for operand {op}: {type}");
    }

    public class MathOp : BinOp
    {
        public MathOp(string op, IEvaluable lhs, IEvaluable rhs) : base(op, lhs, rhs)
        {
            if (lhs.Type == VarType.Bool || rhs.Type == VarType.Bool)
            {
                InvalidType(Op, VarType.Bool);
            }

            if (lhs.Type != rhs.Type)
            {
                Conversion = true;
                Type = VarType.Double;
            }
            else
            {
                Type = lhs.Type;
            }
        }

        public override void Accept(INodeVisitor visitor)
        {
            visitor.VisitMathOp(this);
        }
    }

    #endregion

    public class Assignment : INode
    {
        public readonly Identifier Lhs;
        public readonly IEvaluable Rhs;
        public readonly bool Conversion = false;

        public Assignment(string name, IEvaluable rhs)
        {
            Lhs = new Identifier(name);
            Rhs = rhs;

            try
            {
                if (Lhs.Type == VarType.Double && Rhs.Type == VarType.Integer)
                {
                    Conversion = true;
                }
                else if (Lhs.Type != Rhs.Type)
                {
                    Compiler.Error($"Cannot assign value of type {Rhs.Type} to a variable of type {Lhs.Type}");
                }
            }
            catch (CompilerException)
            {
                // Couldn't get type information - identifier was not declared
            }
        }

        public void Accept(INodeVisitor visitor)
        {
            visitor.VisitAssignment(this);
        }
    }

    public class Write : INode
    {
        public readonly IEvaluable Rhs;

        public Write(IEvaluable rhs)
        {
            Rhs = rhs;
        }

        public void Accept(INodeVisitor visitor)
        {
            visitor.VisitWrite(this);
        }
    }

    public class Read : INode
    {
        public readonly Identifier Target;

        public Read(Identifier target)
        {
            Target = target;
        }

        public void Accept(INodeVisitor visitor)
        {
            visitor.VisitRead(this);
        }
    }

    public class Return : INode
    {
        public void Accept(INodeVisitor visitor)
        {
            visitor.VisitReturn(this);
        }
    }

    public class Program : INode
    {
        public List<INode> Instructions { get; }

        public Program()
        {
            Instructions = new List<INode>();
        }

        public void Accept(INodeVisitor visitor)
        {
            visitor.VisitProgram(this);
        }
    }

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

        // Operations
        void VisitMathOp(MathOp mathOp);
    }

    #region Builders

    public class AstBuilder
    {
        private Program _program;

        public static implicit operator Program(AstBuilder builder) => builder._program;

        public void AddProgram() => _program = new Program();

        private void Append(INode node) => _program.Instructions.Add(node);

        public void AddDeclaration(string name, VarType type) => Append(new Declaration(type, name));

        public void AddAssignment(string name, IEvaluable value) => Append(new Assignment(name, value));

        public void AddWrite(IEvaluable node) => Append(new Write(node));

        public void AddRead(string name) => Append(new Read(new Identifier(name)));

        public void AddReturn() => Append(new Return());
    }

    public class CilBuilder : INodeVisitor, IDisposable
    {
        private readonly StreamWriter _sw;

        public string OutputFile { get; }

        public CilBuilder(string file)
        {
            OutputFile = file + ".il";
            _sw = new StreamWriter(OutputFile);
        }

        public void Dispose() => _sw.Dispose();

        public void VisitProgram(Program program)
        {
            EmitPrologue();
            program.Instructions.ForEach(x => x.Accept(this));
            EmitEpilogue();
            _sw.Flush();
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
                case VarType.Integer:
                    EmitLine("conv.i4");
                    break;
                case VarType.Bool:
                    EmitLine("conv.i4");
                    break;
                default:
                    Compiler.Error($"Conversion to type {targetType} unknown");
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
                {"/", "div"}
            };

            EmitLine(opToOpcode[mathOp.Op]);
        }

        private void EmitLine(string code = null) => _sw.WriteLine(code);

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

    #region misc

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
            Console.Write($"write ");
            write.Rhs.Accept(this);
            Console.WriteLine(";");
        }

        public void VisitRead(Read read) => Console.WriteLine($"read {read.Target.Name}");

        public void VisitReturn(Return ret) => Console.WriteLine("return;");

        public void VisitMathOp(MathOp mathOp)
        {
            mathOp.Lhs.Accept(this);
            Console.Write($" {mathOp.Op} ");
            mathOp.Rhs.Accept(this);
        }
    }

    #endregion

    #endregion

    public class CompilerException : Exception
    {
        public CompilerException(string message) : base(message)
        {
        }
    }

    public static class Compiler
    {
        public delegate void ErrorLogger(string message, bool interrupt = false);

        public static ErrorLogger Error;

        public static (Program, int) Compile(string file)
        {
            var source = new FileStream(file, FileMode.Open);

            var scanner = new Scanner(source);
            var parser = new Parser(scanner);

            var errors = 0;

            Error = (string message, bool interrupt) =>
            {
                // Hack - make use of the built-in line tracking in the scanner
                Console.Error.WriteLine($"{message} on line {scanner.lineNumber}");
                errors++;
                if (interrupt)
                {
                    throw new CompilerException("Irrecoverable state");
                }
            };

            try
            {
                parser.Parse();
            }
            catch (CompilerException)
            {
            }

            errors += scanner.errors;

            return (parser.builder, errors);
        }

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
}