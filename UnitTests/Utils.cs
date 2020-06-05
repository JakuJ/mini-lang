using System.Diagnostics;
using System.Threading.Tasks;
using NUnit.Framework;

namespace UnitTests
{
    public static class Utils
    {
        private static string RunExternal(string program, string args)
        {
            var process = new Process
            {
                StartInfo = new ProcessStartInfo
                {
                    FileName = program,
                    Arguments = args,
                    UseShellExecute = false,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    CreateNoWindow = true
                }
            };

            process.Start();
            Task<string> output = process.StandardOutput.ReadToEndAsync();

            bool exited = process.WaitForExit(5000);
            Assert.IsTrue(exited, $"{program} took more than 5 seconds to finish");
            Assert.AreEqual(0, process.ExitCode, $"{program} exited with code {process.ExitCode}");

            return output.GetAwaiter().GetResult();
        }

        public static void Link(string ilPath) => RunExternal("ilasm", ilPath);

        public static void Verify(string exePath) => RunExternal("peverify", $"--verify all {exePath}");

        public static string Execute(string exePath) => RunExternal("mono", exePath);
    }
}