using System.IO;
using mini_lang;
using NUnit.Framework;

namespace UnitTests
{
    [TestFixture]
    public class InvalidPrograms
    {
        private static int CompilationErrors(string file)
        {
            string path = Path.Combine(TestContext.CurrentContext.TestDirectory, $"TestSources/Invalid/{file}");
            return Compiler.Compile(path).Item2;
        }

        [Test]
        public void MismatchedBraces() => Assert.AreEqual(1, CompilationErrors("program.mini"));

        [Test]
        public void VariableNotDeclared() => Assert.AreEqual(1, CompilationErrors("undeclared.mini"));
    }
}