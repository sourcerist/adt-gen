using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Linq;

namespace AlgebraicDataTypes.Optics.UnitTest
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void LensWorks()
        {
            var p1 = new ProductType1("asdf", 5, 3.14);
            var p2 = ProductType1.Id.MyString().Set("qwerty")(p1);
            Assert.AreEqual(p1.MyString, "asdf");
            Assert.AreEqual(p2.MyString, "qwerty");
        }

        [TestMethod]
        public void PrismWorks()
        {
            var p1 = new ProductType1("asdf", 5, 3.14);
            var s1 = SumType1.Create(p1);
            var s2 = SumType1.Id.ProductType1().MyString().Set("qwerty")(s1);
            Assert.IsTrue(SumType1.Id.ProductType1().MyString().ToEnumerableOf(s1).Contains("asdf"));
            Assert.IsFalse(SumType1.Id.ProductType1().MyString().ToEnumerableOf(s2).Contains("asdf"));
            Assert.IsFalse(SumType1.Id.ProductType1().MyString().ToEnumerableOf(s1).Contains("qwerty"));
            Assert.IsTrue(SumType1.Id.ProductType1().MyString().ToEnumerableOf(s2).Contains("qwerty"));
        }
    }
}
