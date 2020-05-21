using System;
using System.Collections.Generic;
using System.IO;
using GardensPoint;

namespace mini_lang
{
    #region AST

    public interface INode
    {
        void Accept(INodeVisitor visitor);
    }

    public class Variable : INode
    {
        public enum VType
        {
            Integer,
            Double,
            Bool
        }

        public readonly string Name;
        public readonly VType Type;

        public Variable(VType vType, string name)
        {
            Type = vType;
            Name = name;
        }

        public void Accept(INodeVisitor visitor)
        {
            visitor.VisitVariable(this);
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
        void VisitVariable(Variable variable);
    }

    #endregion

    #region Builders

    public class AstBuilder
    {
        public Program Program { get; private set; }

        public void addProgram()
        {
            Program = new Program();
        }

        public void addVariable(string value, string name)
        {
            Variable.VType vType;
            switch (value)
            {
                case "int":
                    vType = Variable.VType.Integer;
                    break;
                case "double":
                    vType = Variable.VType.Double;
                    break;
                case "bool":
                    vType = Variable.VType.Bool;
                    break;
                default:
                    throw new ArgumentException($"Invalid variable type {value}");
            }

            Program.Instructions.Add(new Variable(vType, name));
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

        public void VisitVariable(Variable variable)
        {
            switch (variable.Type)
            {
                case Variable.VType.Bool:
                {
                    EmitLine($".locals init ( bool {variable.Name} )");
                    break;
                }
                case Variable.VType.Integer:
                {
                    EmitLine($".locals init ( int32 {variable.Name} )");
                    break;
                }
                case Variable.VType.Double:
                {
                    EmitLine($".locals init ( float64 {variable.Name} )");
                    break;
                }
            }
        }

        private void EmitLine(string code = null)
        {
            _sw.WriteLine(code);
        }

        private void EmitLine(string code, params object[] args)
        {
            _sw.WriteLine(code, args);
        }

        public void EmitPrologue()
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
            // emit variables here ?
            EmitLine();
        }

        public void EmitEpilogue()
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

        public void VisitVariable(Variable variable)
        {
            Console.WriteLine($"{variable.Type} {variable.Name};");
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