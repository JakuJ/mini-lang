using System.IO;
using NUnit.Framework;
using mini_lang;

namespace UnitTests
{
    [TestFixture]
    public class ValidPrograms
    {
        private static Program RunCompiler(string file)
        {
            string path = Path.Combine(TestContext.CurrentContext.TestDirectory, $"TestSources/Valid/{file}");
            return Compiler.Compile(path);
        }

        [Test]
        public void EmptyProgram()
        {
            Assert.DoesNotThrow(() =>
            {
                Program p = RunCompiler("empty.mini");
                Assert.IsEmpty(p.Instructions);
            });
        }
    }
}