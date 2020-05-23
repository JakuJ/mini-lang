using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using mini_lang;
using NUnit.Framework;

namespace UnitTests
{
    [TestFixture]
    public class OutputPrograms
    {
        private static (Program, int) RunCompiler(string file)
        {
            string path = Path.Combine(TestContext.CurrentContext.TestDirectory, $"TestSources/Outputs/{file}");
            return Compiler.Compile(path);
        }

        [SetUp]
        public void Setup()
        {
            Declaration.Declared = new Dictionary<string, VarType>();
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
                        UseShellExecute = false, RedirectStandardOutput = true,
                        CreateNoWindow = true
                    }
                };

                linkerProcess.Start();
                linkerProcess.WaitForExit();
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
                        CreateNoWindow = true
                    }
                };

                monoProcess.Start();

                output = monoProcess.StandardOutput.ReadToEnd();

                monoProcess.WaitForExit();
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
                (string, string)[] cases = new[]
                {
                    ("task1.mini", "5\n123.456000\nTrue\n"),
                    ("return.mini", "return"),
                    ("math_ops.mini", "3\n-1\n2\n0.000000\n0.500000\n2.000000\n6\n-2.000000\n25.000000\n6\n6\n"),
                    ("bit_ops.mini", "15\n40.000000\n0\n3\n"),
                };

                foreach ((string path, string expected) in cases)
                {
                    yield return new TestCaseData(path).Returns(expected);
                }
            }
        }
    }
}