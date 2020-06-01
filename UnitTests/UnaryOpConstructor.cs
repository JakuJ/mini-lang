using System;
using mini_lang;
using NUnit.Framework;

namespace UnitTests
{
    [TestFixture]
    public class UnaryOpConstructor
    {
        [SetUp]
        public void Setup()
        {
            Compiler.Error = (message, interrupt) =>
            {
                Console.Error.WriteLine(message);
                if (interrupt)
                    throw new AstException(message);
            };
        }

        [Test]
        public void UnexpectedTypeInConstructor()
        {
            Assert.Throws<AstException>(() =>
            {
                var unused = new UnaryOp(UnaryOp.OpType.Conv2Int, null);
            });

            Assert.Throws<AstException>(() =>
            {
                var unused = new UnaryOp(UnaryOp.OpType.Conv2Double, null);
            });
        }
    }
}