using System;
using System.Collections.Generic;
using System.Linq;

namespace AlgebraicDataTypes.Optics
{
    public interface IGetter<S, A> : IFold<S, A>
    {
        Func<S, A> Get();
        IGetter<S, B> ComposeWith<B>(IGetter<A, B> other);
    }

    public class Getter<S, A>: IGetter<S, A>
    {
        private Func<S, A> _get;
        public Getter(Func<S, A> get)
        {
            _get = get;
        }

        public IGetter<S, B> ComposeWith<B>(IGetter<A, B> other) => Getter.Create(_get.ComposeWith(other.Get()));

        public IFold<S, B> ComposeWith<B>(IFold<A, B> other) => Fold.Create<S,B>(s => other.ToEnumerableOf()(_get(s)));

        public Func<S, A> Get() => _get;

        public Func<S, IEnumerable<A>> ToEnumerableOf() => YieldSingle;

        private IEnumerable<A> YieldSingle(S s) { yield return _get(s); }
    }

    public static class Getter
    {
        public static Getter<S, A> Create<S, A>(Func<S, A> get) => new Getter<S, A>(get);
    }
}
