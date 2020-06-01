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
                (string, int)[] cases =
                {
                    ("bit_ops.mini", 5),
                    ("break.mini", 4),
                    ("break_n.mini", 7),
                    ("case.mini", 5),
                    ("comp_ops.mini", 4),
                    ("emptyfile.mini", 1),
                    ("eof.mini", 1),
                    ("identifiers.mini", 10),
                    ("ifelse.mini", 3),
                    ("invalid_assignment.mini", 12),
                    ("logic_ops.mini", 4),
                    ("math_ops.mini", 7),
                    ("missing_arg.mini", 5),
                    ("mixed_decls.mini", 3),
                    ("mixed_inner_decls.mini", 1),
                    ("multi_assignment.mini", 5),
                    ("multi_decls.mini", 2),
                    ("no_conv2bool.mini", 1),
                    ("onetoomany.mini", 1),
                    ("out_of_scope.mini", 1),
                    ("redeclaration.mini", 5),
                    ("string.mini", 6),
                    ("unary.mini", 5),
                    ("undeclared.mini", 3),
                    ("uninitialized.mini", 2),
                    ("while.mini", 4),
                };

                foreach ((string path, int expected) in cases)
                {
                    yield return new TestCaseData(path, expected);
                }
            }
        }
    }
}