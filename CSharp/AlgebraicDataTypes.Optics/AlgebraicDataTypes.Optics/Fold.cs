using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AlgebraicDataTypes.Optics
{
    public interface IFold<S, A>
    {
        Func<S, IEnumerable<A>> ToEnumerableOf();
        IFold<S, B> ComposeWith<B>(IFold<A, B> other);
    }

    public class Fold<S, A> : IFold<S, A>
    {
        private Func<S, IEnumerable<A>> _toEnumerableOf;
        public Fold(Func<S, IEnumerable<A>> toEnumerableOf) => _toEnumerableOf = toEnumerableOf;

        public IFold<S, B> ComposeWith<B>(IFold<A, B> other) => Fold.Create<S, B>(s => _toEnumerableOf(s).SelectMany(other.ToEnumerableOf()));

        public Func<S, IEnumerable<A>> ToEnumerableOf() => _toEnumerableOf;
    }

    public static class Fold
    {
        public static Fold<S, A> Create<S, A>(Func<S, IEnumerable<A>> toEnumerableOf) => new Fold<S, A>(toEnumerableOf);
    }
}
