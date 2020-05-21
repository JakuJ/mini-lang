using System;
using System.Collections.Generic;
using System.IO;
using GardensPoint;

namespace mini_lang
{
    #region AST

    public enum VarType
    {
        Integer,
        Double,
        Bool
    }

    public interface INode
    {
        void Accept(INodeVisitor visitor);
    }

    public class Declaration : INode
    {
        public readonly string Name;
        public readonly VarType Type;

        public Declaration(VarType vType, string name)
        {
            Type = vType;
            Name = name;
        }

        public void Accept(INodeVisitor visitor)
        {
            visitor.VisitDeclaration(this);
        }
    }

    public class Constant : INode
    {
        public readonly string Value;
        public readonly VarType Type;

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
        public readonly string Lhs;
        public readonly INode Rhs;

        public Assignment(string lhs, INode rhs)
        {
            Lhs = lhs;
            Rhs = rhs;
        }

        public void Accept(INodeVisitor visitor)
        {
            visitor.VisitAssignment(this);
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
        void VisitDeclaration(Declaration declaration);
        void VisitAssignment(Assignment assignment);
        void VisitConstant(Constant constant);
    }

    #endregion

    #region Builders

    public class AstBuilder
    {
        public Program Program { get; private set; }
        private readonly Dictionary<string, VarType> _declared;

        public Scanner Scanner { private get; set; }

        public AstBuilder()
        {
            _declared = new Dictionary<string, VarType>();
        }

        public void AddProgram()
        {
            Program = new Program();
        }

        private void Append(INode node)
        {
            Program.Instructions.Add(node);
        }

        public void AddDeclaration(string name, VarType type)
        {
            if (_declared.ContainsKey(name))
            {
                Scanner?.yyerror($"Redeclaration of variable {name}");
            }

            _declared[name] = type;
            Append(new Declaration(type, name));
        }

        public void AddAssignment(string name, INode value)
        {
            if (!_declared.ContainsKey(name))
            {
                Scanner?.yyerror($"Undeclared variable {name}");
                Environment.Exit(1);
            }

            Append(new Assignment(name, value));
        }
    }

    public class CilBuilder : INodeVisitor, IDisposable
    {
        private readonly StreamWriter _sw;

        public CilBuilder(string file)
        {
            _sw = new StreamWriter(file + ".il");
        }

        public void Dispose()
        {
            _sw.Dispose();
        }

        public void VisitProgram(Program program)
        {
            EmitPrologue();
            program.Instructions.ForEach(x => x.Accept(this));
            EmitEpilogue();
            _sw.Flush();
        }

        public void VisitDeclaration(Declaration declaration)
        {
            switch (declaration.Type)
            {
                case VarType.Bool:
                {
                    EmitLine($".locals init ( bool {declaration.Name} )");
                    break;
                }
                case VarType.Integer:
                {
                    EmitLine($".locals init ( int32 {declaration.Name} )");
                    break;
                }
                case VarType.Double:
                {
                    EmitLine($".locals init ( float64 {declaration.Name} )");
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
            }
        }

        public void VisitAssignment(Assignment assignment)
        {
            assignment.Rhs.Accept(this);
            EmitLine($"stloc {assignment.Lhs}");
        }

        private void EmitLine(string code = null)
        {
            _sw.WriteLine(code);
        }

        private void EmitLine(string code, params object[] args)
        {
            _sw.WriteLine(code, args);
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

        public void VisitDeclaration(Declaration declaration)
        {
            Console.WriteLine($"{declaration.Type} {declaration.Name};");
        }

        public void VisitConstant(Constant constant)
        {
            Console.Write(constant.Value);
        }

        public void VisitAssignment(Assignment assignment)
        {
            Console.Write($"{assignment.Lhs} = ");
            assignment.Rhs.Accept(this);
            Console.WriteLine(";");
        }
    }

    #endregion

    public static class Compiler
    {
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