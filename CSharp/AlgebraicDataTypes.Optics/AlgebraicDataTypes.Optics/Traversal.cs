using System;
using System.Collections.Generic;
using System.Text;

namespace AlgebraicDataTypes.Optics
{
    public interface ITraversal<S, T, A, B> : IFold<S, A>, ISetter<S, T, A, B>
    {
        ITraversal<S, T, C, D> ComposeWith<C, D>(ITraversal<A, B, C, D> other);
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

        public ITraversal<S, T, C, D> ComposeWith<C, D>(ITraversal<A, B, C, D> other) => new Traversal<S,T,C,D>(_fold.ComposeWith(other), _setter.ComposeWith(other));

        public Func<S, T> Over(Func<A, B> f) => _setter.Over(f);

        Func<S, IEnumerable<A>> IFold<S, A>.ToEnumerableOf => s => _fold.ToEnumerableOf(s);
    }

    public static class Traversal
    {
        public static Traversal<S, T, A, B> Create<S, T, A, B>(IFold<S, A> fold, ISetter<S, T, A, B> setter) => new Traversal<S, T, A, B>(fold, setter);
    }
}
