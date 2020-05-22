using System.Collections;
using System.Collections.Generic;
using System.IO;
using mini_lang;
using NUnit.Framework;

namespace UnitTests
{
    [TestFixture]
    public class ValidPrograms
    {
        private static (Program, int) RunCompiler(string file)
        {
            string path = Path.Combine(TestContext.CurrentContext.TestDirectory, $"TestSources/Valid/{file}");
            return Compiler.Compile(path);
        }

        [SetUp]
        public void Setup()
        {
            Declaration.Declared = new Dictionary<string, VarType>();
        }

        [TestCaseSource(typeof(Data), nameof(Data.AllPrograms))]
        public void CorrectProgram(string path)
        {
            var errs = 0;
            Program program = null;

            Assert.DoesNotThrow(() =>
            {
                (Program p, int errors) = RunCompiler(path);
                errs = errors;
                program = p;
            });

            Assert.Zero(errs);

            var printer = new PrettyPrinter();
            Assert.DoesNotThrow(() => { program.Accept(printer); });

            var codeGen = new CilBuilder(Path.Combine(TestContext.CurrentContext.WorkDirectory, path));
            Assert.DoesNotThrow(() => { program.Accept(codeGen); });
        }
    }

    public class Data
    {
        public static IEnumerable AllPrograms
        {
            get
            {
                string folder = Path.Combine(TestContext.CurrentContext.TestDirectory, "TestSources/Valid");
                foreach (string path in Directory.EnumerateFiles(folder))
                {
                    yield return Path.GetFileName(path);
                }
            }
        }
    }
}