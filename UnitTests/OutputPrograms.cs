using System.Collections;
using System.Diagnostics;
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
            var errs = 0;
            Program program = null;

            Assert.DoesNotThrow(() =>
            {
                (Program p, int errors) = RunCompiler(filename);
                errs = errors;
                program = p;
            });

            Assert.Zero(errs);

            var codeGen = new CilBuilder(Path.Combine(TestContext.CurrentContext.WorkDirectory, filename));

            Assert.DoesNotThrow(() => { program.Accept(codeGen); });

            string ilPath = codeGen.OutputFile;

            Assert.DoesNotThrow(() =>
            {
                var linkerProcess = new Process
                {
                    StartInfo = new ProcessStartInfo
                    {
                        FileName = "ilasm",
                        Arguments = ilPath,
                        UseShellExecute = false,
                        RedirectStandardOutput = true,
                        CreateNoWindow = true
                    }
                };

                linkerProcess.Start();
                linkerProcess.WaitForExit();
                Assert.AreEqual(0, linkerProcess.ExitCode);
            });

            var output = "";

            Assert.DoesNotThrow(() =>
            {
                var monoProcess = new Process
                {
                    StartInfo = new ProcessStartInfo
                    {
                        FileName = "mono",
                        Arguments = Path.ChangeExtension(ilPath, "exe"),
                        UseShellExecute = false,
                        RedirectStandardOutput = true,
                        RedirectStandardError = true,
                        CreateNoWindow = true
                    }
                };

                monoProcess.Start();
                output = monoProcess.StandardOutput.ReadToEnd();
                monoProcess.WaitForExit();
                Assert.AreEqual(0, monoProcess.ExitCode);
            });

            return output;
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
                    ("task1.mini", "5 123.456000 True "),
                    ("return.mini", "return"),
                    ("math_ops.mini", "3 -1 2 0.000000 0.500000 2.000000 6 -2.000000 25.000000 6 6 "),
                    ("bit_ops.mini", "15 40.000000 0 3 "),
                    ("comp_ops.mini", "True False False False True True True True True"),
                    ("logic_ops.mini", "True True False True False"),
                    ("unary.mini", "-1 5 False True -2 1 3.560000 8.000000 0.000000 1.000000 5 1 3 1 0"),
                    ("while.mini", "40320 1 2 3 4 5 9 -2 0"),
                    ("ifelse.mini", "7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 iters: 16"),
                    ("monte_carlo.mini", "PI between 3 and 3.3: True"),
                };

                foreach ((string path, string expected) in cases)
                {
                    yield return new TestCaseData(path).Returns(expected);
                }
            }
        }
    }
}