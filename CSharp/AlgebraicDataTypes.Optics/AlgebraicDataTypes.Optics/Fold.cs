using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace AlgebraicDataTypes.Optics
{
    public interface IFold<S, A>
    {
        Func<S, IEnumerable<A>> ToEnumerableOf { get; }
        IFold<S, B> ComposeWith<B>(IFold<A, B> other);
    }

    public class Fold<S, A> : IFold<S, A>
    {
        public Func<S, IEnumerable<A>> ToEnumerableOf { get; }
        public Fold(Func<S, IEnumerable<A>> toEnumerableOf) => ToEnumerableOf = toEnumerableOf;

        public IFold<S, B> ComposeWith<B>(IFold<A, B> other) => Fold.Create<S, B>(s => ToEnumerableOf(s).SelectMany(other.ToEnumerableOf));
    }

    public static class Fold
    {
        public static Fold<S, A> Create<S, A>(Func<S, IEnumerable<A>> toEnumerableOf) => new Fold<S, A>(toEnumerableOf);
    }
}
