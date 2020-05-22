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
                    Compiler.LogError($"Variable {Name} has not been declared");
                }

                return Declaration.Declared[Name];
            }
        }


        public Identifier(string name)
        {
            if (!Declaration.Declared.ContainsKey(name))
            {
                Compiler.LogError($"Variable {name} has not been declared");
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

        public static readonly Dictionary<string, VarType> Declared = new Dictionary<string, VarType>();

        public Declaration(VarType vType, string name)
        {
            if (vType == VarType.String)
            {
                Compiler.LogError("Strings are only allowed in write expressions");
            }

            if (Declared.ContainsKey(name))
            {
                Compiler.LogError($"Redeclaration of variable {name}");
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

    public class Constant : INode, IEvaluable
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

    public class Assignment : INode
    {
        public readonly Identifier Lhs;
        public readonly IEvaluable Rhs;

        public Assignment(string name, IEvaluable rhs)
        {
            Lhs = new Identifier(name);
            Rhs = rhs;
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
        void VisitProgram(Program program);
        void VisitIdentifier(Identifier identifier);
        void VisitDeclaration(Declaration declaration);
        void VisitAssignment(Assignment assignment);
        void VisitConstant(Constant constant);
        void VisitWrite(Write write);
        void VisitReturn(Return ret);
    }

    #region Builders

    public class AstBuilder
    {
        public Program Program { get; private set; }

        public void AddProgram() => Program = new Program();

        private void Append(INode node) => Program.Instructions.Add(node);

        public void AddDeclaration(string name, VarType type) => Append(new Declaration(type, name));

        public void AddAssignment(string name, IEvaluable value) => Append(new Assignment(name, value));

        public void AddWrite(IEvaluable node) => Append(new Write(node));

        public void AddReturn() => Append(new Return());
    }

    public class CilBuilder : INodeVisitor, IDisposable
    {
        private readonly StreamWriter _sw;

        public CilBuilder(string file) => _sw = new StreamWriter(file + ".il");

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

        public void VisitAssignment(Assignment assignment)
        {
            assignment.Rhs.Accept(this);
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
                    EmitLine("call class [mscorlib]System.Globalization.CultureInfo class [mscorlib]System.Globalization.CultureInfo::get_InvariantCulture()");
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

        public void VisitReturn(Return ret)
        {
            EmitLine("leave EndMain");
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
            EmitLine();
            EmitLine("// prolog");
            EmitLine();
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

        public void VisitReturn(Return ret) => Console.WriteLine("return;");
    }

    #endregion

    #endregion

    public static class Compiler
    {
        public delegate void ErrorLogger(string message);

        public static ErrorLogger LogError;

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

            var source = new FileStream(file, FileMode.Open);

            var scanner = new Scanner(source);
            var parser = new Parser(scanner);

            // Hack - make use of the built-in line tracking in the scanner
            LogError = (string message) =>
            {
                scanner.yyerror(message);
                Environment.Exit(1);
            };

            bool success = parser.Parse();
            Program program = parser.builder.Program;

            var printer = new PrettyPrinter();
            program.Accept(printer);

            var generator = new CilBuilder(file);
            program.Accept(generator);

            return success ? 0 : 1;
        }
    }
}