using System;
using System.Collections.Generic;
using System.Linq;

namespace AlgebraicDataTypes.Optics
{
    public interface IGetter<S, A> : IFold<S, A>
    {
        Func<S, A> Get { get; }
        IGetter<S, B> ComposeWith<B>(IGetter<A, B> other);
    }

    public class Getter<S, A>: IGetter<S, A>
    {
        public Func<S, A> Get { get; }

        Func<S, IEnumerable<A>> ToEnumerableOf => s => YieldSingle(s);

        Func<S, IEnumerable<A>> IFold<S, A>.ToEnumerableOf => ToEnumerableOf;

        public Getter(Func<S, A> get) => Get = get;

        public IGetter<S, B> ComposeWith<B>(IGetter<A, B> other) => Getter.Create(Get.ComposeWith(other.Get));

        public IFold<S, B> ComposeWith<B>(IFold<A, B> other) => Fold.Create<S,B>(s => other.ToEnumerableOf(Get(s)));

        private IEnumerable<A> YieldSingle(S s) { yield return Get(s); }
    }

    public static class Getter
    {
        public static Getter<S, A> Create<S, A>(Func<S, A> get) => new Getter<S, A>(get);
    }
}
