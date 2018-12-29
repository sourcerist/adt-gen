using System;
using System.Collections.Generic;
using System.Text;

namespace AlgebraicDataTypes.Optics
{
    public interface ILens<S, T, A, B> : IGetter<S, A>, ISetter<S, T, A, B>, ITraversal<S, T, A, B>
    {
        ILens<S, T, C, D> ComposeWith<C, D>(ILens<A, B, C, D> other);
    }

    public class Lens<S, T, A, B> : ILens<S, T, A, B>
    {
        private IGetter<S, A> _getter;
        private ISetter<S, T, A, B> _setter;
        public Lens(IGetter<S, A> getter, ISetter<S,T,A,B> setter) => (_getter, _setter) = (getter, setter);

        public Func<S, A> Get => _getter.Get;

        public Func<S, IEnumerable<A>> ToEnumerableOf => _getter.ToEnumerableOf;

        public ILens<S, T, C, D> ComposeWith<C, D>(ILens<A, B, C, D> other) => new Lens<S, T, C, D>(_getter.ComposeWith(other), _setter.ComposeWith(other));

        public IGetter<S, C> ComposeWith<C>(IGetter<A, C> other) => _getter.ComposeWith(other);

        public IFold<S, C> ComposeWith<C>(IFold<A, C> other) => _getter.ComposeWith(other);

        public ISetter<S, T, C, D> ComposeWith<C, D>(ISetter<A, B, C, D> other) => _setter.ComposeWith(other);

        public ITraversal<S, T, C, D> ComposeWith<C, D>(ITraversal<A, B, C, D> other) => new Traversal<S, T, C, D>(_getter.ComposeWith(other), _setter.ComposeWith(other));

        public Func<S, T> Over(Func<A, B> f) => _setter.Over(f);
    }

    public static class Lens
    {

    }
}
