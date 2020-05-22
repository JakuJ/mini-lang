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

        [TestCaseSource(typeof(Data), nameof(Data.EmptyPrograms))]
        public void EmptyProgram(string program)
        {
            (Program p, int errors) = RunCompiler(program);
            Assert.Zero(errors);
            Assert.IsEmpty(p.Instructions);
        }
    }

    public class Data
    {
        public static IEnumerable<string> EmptyPrograms = new string[]
        {
            "empty1.mini",
            "empty2.mini",
            "empty3.mini",
            "empty4.mini"
        };
    }
}