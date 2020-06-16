using System.Collections;
using System.IO;
using mini_lang;
using NUnit.Framework;

namespace UnitTests
{
    [TestFixture]
    [Parallelizable]
    public class OutputPrograms
    {
        private static (Program, int) RunCompiler(string file)
        {
            string path = Path.Combine(TestContext.CurrentContext.TestDirectory, $"TestSources/Outputs/{file}");
            return Compiler.Compile(path);
        }

        [TestCaseSource(typeof(Outputs), nameof(Outputs.Sources))]
        public string ProgramOutput(string filename)
        {
            var     errs    = 0;
            Program program = null;

            Assert.DoesNotThrow(() =>
                                {
                                    (Program p, int errors) = RunCompiler(filename);
                                    errs                    = errors;
                                    program                 = p;
                                },
                                "AST construction should not throw");

            Assert.Zero(errs, "Expected there to be no errors found in source code");

            var codeGen = new CodeBuilder(Path.Combine(TestContext.CurrentContext.WorkDirectory, filename));

            Assert.DoesNotThrow(() => { program.Accept(codeGen); }, "Code generation should not throw");

            string ilPath  = codeGen.OutputFile;
            string exePath = Path.ChangeExtension(ilPath, "exe");

            Utils.Link(ilPath);
            Utils.Verify(exePath);
            return Utils.Execute(exePath);
        }
    }

    public class Outputs
    {
        public static IEnumerable Sources
        {
            get
            {
                (string, string)[] cases =
                {
                    ("arrays.mini", "44 33 22 11 00 "),
                    ("bit_ops.mini", "15 40.000000 0 3"),
                    ("break.mini", "100 20"),
                    ("break_n.mini", "100 20"),
                    ("brodka1.mini", "5 123.456000 True "),
                    ("brodka2.mini", "1 12 True"),
                    ("comp_ops.mini", "True False False False True True True True True"),
                    ("continue.mini", "1 3 5 7 9 "),
                    ("default.mini", "0 0.000000 False"),
                    ("ifelse.mini", "7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 iters: 16"),
                    ("logic_ops.mini", "True True False True False"),
                    ("math_ops.mini", "3 -1 2 0.000000 0.500000 2.000000 6 -2.000000 25.000000 6 6"),
                    ("monte_carlo.mini", "PI between 3 and 3.3: True"),
                    ("multi_assignment.mini", "6 6 6.000000"),
                    ("return.mini", "return"),
                    ("scopes.mini", "0 5 0 2"),
                    ("strings.mini", "\\\"\n"),
                    ("unary.mini", "-1 5 False True -2 1 3.560000 8.000000 0.000000 1.000000 5 1 3 1 0"),
                    ("while.mini", "40320 1 2 3 4 5 9 -2 0"),
                };

                foreach ((string path, string expected) in cases)
                {
                    yield return new TestCaseData(path).Returns(expected);
                }
            }
        }
    }
}