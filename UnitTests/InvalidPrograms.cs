using System.IO;
using NUnit.Framework;
using mini_lang;

namespace UnitTests
{
    [TestFixture]
    public class InvalidPrograms
    {
        private static TestDelegate RunCompiler(string file)
        {
            string path = Path.Combine(TestContext.CurrentContext.TestDirectory, $"TestSources/Invalid/{file}");
            return () => Compiler.Compile(path);
        }

        [Test]
        public void MismatchedBraces() => Assert.Throws<ParsingException>(RunCompiler("program.mini"));

        [Test]
        public void VariableNotDeclared() => Assert.Throws<ParsingException>(RunCompiler("undeclared.mini"));
    }
}