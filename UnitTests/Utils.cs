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
            process.WaitForExit();
            Assert.AreEqual(0, process.ExitCode);
            return output;
        }

        public static void Link(string ilPath) => RunExternal("ilasm", ilPath);

        public static void Verify(string exePath) => RunExternal("peverify", $"--verify all {exePath}");

        public static string Execute(string exePath) => RunExternal("mono", exePath);
    }
}