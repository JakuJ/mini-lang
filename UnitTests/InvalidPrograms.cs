using System;
using System.Collections;
using System.IO;
using mini_lang;
using NUnit.Framework;

namespace UnitTests
{
    [TestFixture]
    [Parallelizable]
    public class InvalidPrograms
    {
        private static int CompilationErrors(string file)
        {
            string path = Path.Combine(TestContext.CurrentContext.TestDirectory, $"TestSources/Invalid/{file}");
            return Compiler.Compile(path).Item2;
        }

        [TestCaseSource(typeof(InvalidSources), nameof(InvalidSources.Sources))]
        public void InvalidProgram(string path, int atLeast)
        {
            var errs = 0;
            Assert.DoesNotThrow(() => { errs = CompilationErrors(path); });
            Assert.GreaterOrEqual(errs, atLeast);
            Console.Error.WriteLine($"Passed, {errs} errors found");
        }
    }

    public class InvalidSources
    {
        public static IEnumerable Sources
        {
            get
            {
                (string, int)[] cases = new[]
                {
                    ("eof.mini", 1),
                    ("undeclared.mini", 3),
                    ("redeclaration.mini", 3),
                    ("invalid_assignment.mini", 11),
                    ("identifiers.mini", 10),
                    ("case.mini", 5),
                    ("string.mini", 5),
                    ("missing_arg.mini", 5),
                    ("math_ops.mini", 7),
                    ("bit_ops.mini", 5),
                    ("comp_ops.mini", 4),
                    ("logic_ops.mini", 4),
                    ("unary.mini", 5),
                };

                foreach ((string path, int expected) in cases)
                {
                    yield return new TestCaseData(path, expected);
                }
            }
        }
    }
}