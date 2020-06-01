using System.Diagnostics;
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
            string output = process.StandardOutput.ReadToEnd();
            bool exited = process.WaitForExit(1000);
            Assert.IsTrue(exited, $"{program} took more than 1 second to finish");
            Assert.AreEqual(0, process.ExitCode);
            return output;
        }

        public static void Link(string ilPath) => RunExternal("ilasm", ilPath);

        public static void Verify(string exePath) => RunExternal("peverify", $"--verify all {exePath}");

        public static string Execute(string exePath) => RunExternal("mono", exePath);
    }
}