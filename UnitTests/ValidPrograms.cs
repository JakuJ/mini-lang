using System.Collections;
using System.IO;
using mini_lang;
using NUnit.Framework;

namespace UnitTests
{
    [TestFixture]
    [Parallelizable]
    public class ValidPrograms
    {
        private static (Program, int) RunCompiler(string file)
        {
            string path = Path.Combine(TestContext.CurrentContext.TestDirectory, $"TestSources/Valid/{file}");
            return Compiler.Compile(path);
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
                },
                "AST construction should not throw");

            Assert.Zero(errs);

            var codeGen = new CilBuilder(Path.Combine(TestContext.CurrentContext.WorkDirectory, path));
            Assert.DoesNotThrow(() => { program.Accept(codeGen); }, "Code generation should not throw");

            string ilPath = codeGen.OutputFile;

            Utils.Link(ilPath);
            Utils.Verify(Path.ChangeExtension(ilPath, "exe"));
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