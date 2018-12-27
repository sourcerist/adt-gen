using System;
using System.Collections.Generic;
using System.Text;

namespace AlgebraicDataTypes.Optics
{
    public interface ITraversal<S, T, A, B> : IFold<S, A>, ISetter<S, T, A, B>
    {
    }

    public class Traversal<S, T, A, B> : ITraversal<S, T, A, B>
    {
        IFold<S, A> _fold;
        ISetter<S, T, A, B> _setter;
        public Traversal(IFold<S,A> fold, ISetter<S, T, A, B> setter)
        {
            _fold = fold;
            _setter = setter;
        }

        public IFold<S, C> ComposeWith<C>(IFold<A, C> other) => _fold.ComposeWith(other);

        public ISetter<S, T, C, D> ComposeWith<C, D>(ISetter<A, B, C, D> other) => _setter.ComposeWith(other);

        public Func<S, T> Over(Func<A, B> f) => _setter.Over(f);

        public Func<S, IEnumerable<A>> ToEnumerableOf() => _fold.ToEnumerableOf();
    }
}
