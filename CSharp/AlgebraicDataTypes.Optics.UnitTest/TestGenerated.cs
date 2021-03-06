using System;
using System.Collections.Generic;
using AlgebraicDataTypes.Optics;
namespace AlgebraicDataTypes.Optics.UnitTest
{
	public partial class ProductType1
	{
		public String MyString{ get; }
		public Int32 MyInt{ get; }
		public Double MyDouble{ get; }
		public ProductType1(String myString, Int32 myInt, Double myDouble) => (MyString, MyInt, MyDouble) = (myString, myInt, myDouble);
		
		public static AlgebraicDataTypes.Optics.Lens<ProductType1, ProductType1, ProductType1, ProductType1> Id { get; } = AlgebraicDataTypes.Optics.Lens.Create<ProductType1, ProductType1, ProductType1, ProductType1>(s => s, (a2b,s) => a2b(s));
	}
	
	public static partial class ProductType1Optics
	{
		public static Lens<ProductType1, ProductType1, String, String> MyStringLens = Lens.Create<ProductType1, ProductType1, String, String>(s => s.MyString, (s2a, s) => new ProductType1(s2a(s.MyString), s.MyInt, s.MyDouble));
		public static ILens<S,T, String, String> MyString<S,T>(this ILens<S,T, ProductType1, ProductType1> other) => other.ComposeWith(MyStringLens);
		public static ITraversal<S,T, String, String> MyString<S,T>(this ITraversal<S,T, ProductType1, ProductType1> other) => other.ComposeWith(MyStringLens);
		public static ISetter<S,T, String, String> MyString<S,T>(this ISetter<S,T, ProductType1, ProductType1> other) => other.ComposeWith(MyStringLens);
		public static IGetter<S, String> MyString<S>(this IGetter<S, ProductType1> other) => other.ComposeWith(MyStringLens);
		public static IFold<S, String> MyString<S>(this IFold<S, ProductType1> other) => other.ComposeWith(MyStringLens);
		
		public static Lens<ProductType1, ProductType1, Int32, Int32> MyIntLens = Lens.Create<ProductType1, ProductType1, Int32, Int32>(s => s.MyInt, (s2a, s) => new ProductType1(s.MyString, s2a(s.MyInt), s.MyDouble));
		public static ILens<S,T, Int32, Int32> MyInt<S,T>(this ILens<S,T, ProductType1, ProductType1> other) => other.ComposeWith(MyIntLens);
		public static ITraversal<S,T, Int32, Int32> MyInt<S,T>(this ITraversal<S,T, ProductType1, ProductType1> other) => other.ComposeWith(MyIntLens);
		public static ISetter<S,T, Int32, Int32> MyInt<S,T>(this ISetter<S,T, ProductType1, ProductType1> other) => other.ComposeWith(MyIntLens);
		public static IGetter<S, Int32> MyInt<S>(this IGetter<S, ProductType1> other) => other.ComposeWith(MyIntLens);
		public static IFold<S, Int32> MyInt<S>(this IFold<S, ProductType1> other) => other.ComposeWith(MyIntLens);
		
		public static Lens<ProductType1, ProductType1, Double, Double> MyDoubleLens = Lens.Create<ProductType1, ProductType1, Double, Double>(s => s.MyDouble, (s2a, s) => new ProductType1(s.MyString, s.MyInt, s2a(s.MyDouble)));
		public static ILens<S,T, Double, Double> MyDouble<S,T>(this ILens<S,T, ProductType1, ProductType1> other) => other.ComposeWith(MyDoubleLens);
		public static ITraversal<S,T, Double, Double> MyDouble<S,T>(this ITraversal<S,T, ProductType1, ProductType1> other) => other.ComposeWith(MyDoubleLens);
		public static ISetter<S,T, Double, Double> MyDouble<S,T>(this ISetter<S,T, ProductType1, ProductType1> other) => other.ComposeWith(MyDoubleLens);
		public static IGetter<S, Double> MyDouble<S>(this IGetter<S, ProductType1> other) => other.ComposeWith(MyDoubleLens);
		public static IFold<S, Double> MyDouble<S>(this IFold<S, ProductType1> other) => other.ComposeWith(MyDoubleLens);
	}
	public partial class SumType1
	{
		public virtual bool IsProductType1 { get => false; }
		public static _ProductType1 Create(ProductType1 val) => new _ProductType1(val);
		public partial class _ProductType1 : SumType1
		{
			public ProductType1 Value { get; }
			public _ProductType1(ProductType1 value) => Value = value;
			public override bool IsProductType1 { get => true; }
		}

		public virtual bool IsQueueStackInt32 { get => false; }
		public static _QueueStackInt32 Create(Queue<Stack<Int32>> val) => new _QueueStackInt32(val);
		public partial class _QueueStackInt32 : SumType1
		{
			public Queue<Stack<Int32>> Value { get; }
			public _QueueStackInt32(Queue<Stack<Int32>> value) => Value = value;
			public override bool IsQueueStackInt32 { get => true; }
		}
		public static AlgebraicDataTypes.Optics.Lens<SumType1, SumType1, SumType1, SumType1> Id { get; } = AlgebraicDataTypes.Optics.Lens.Create<SumType1, SumType1, SumType1, SumType1>(s => s, (a2b,s) => a2b(s));
	}
	
	public static partial class SumType1Optics
	{
		public static Prism<SumType1, SumType1, ProductType1, ProductType1> ProductType1Prism = Prism.Create<SumType1, SumType1, ProductType1, ProductType1>(
			b => SumType1.Create(b),
			s => { if (s is SumType1._ProductType1 a) return new Either<SumType1, ProductType1>.Right(a.Value); else return new Either<SumType1, ProductType1>.Left(s); });
		public static IPrism<S,T, ProductType1, ProductType1> ProductType1<S,T>(this IPrism<S,T, SumType1, SumType1> other) => other.ComposeWith(ProductType1Prism);
		public static ITraversal<S,T, ProductType1, ProductType1> ProductType1<S,T>(this ITraversal<S,T, SumType1, SumType1> other) => other.ComposeWith(ProductType1Prism);

		public static Prism<SumType1, SumType1, Queue<Stack<Int32>>, Queue<Stack<Int32>>> QueueStackInt32Prism = Prism.Create<SumType1, SumType1, Queue<Stack<Int32>>, Queue<Stack<Int32>>>(
			b => SumType1.Create(b),
			s => { if (s is SumType1._QueueStackInt32 a) return new Either<SumType1, Queue<Stack<Int32>>>.Right(a.Value); else return new Either<SumType1, Queue<Stack<Int32>>>.Left(s); });
		public static IPrism<S,T, Queue<Stack<Int32>>, Queue<Stack<Int32>>> QueueStackInt32<S,T>(this IPrism<S,T, SumType1, SumType1> other) => other.ComposeWith(QueueStackInt32Prism);
		public static ITraversal<S,T, Queue<Stack<Int32>>, Queue<Stack<Int32>>> QueueStackInt32<S,T>(this ITraversal<S,T, SumType1, SumType1> other) => other.ComposeWith(QueueStackInt32Prism);
	}
}
