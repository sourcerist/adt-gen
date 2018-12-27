using System;
using System.Collections.Generic;
using System.Text;

namespace AlgebraicDataTypes.Optics
{
    public interface ISetter<S, T, A, B>
    {
        Func<S, T> Over(Func<A, B> f);
        ISetter<S, T, C, D> ComposeWith<C, D>(ISetter<A, B, C, D> other);
    }

    public class Setter<S, T, A, B> : ISetter<S, T, A, B>
    {
        private Func<Func<A, B>, S, T> _over;
        public Setter(Func<Func<A, B>, S, T> over) => _over = over;

        public ISetter<S, T, C, D> ComposeWith<C, D>(ISetter<A, B, C, D> other) => Setter.Create<S, T, C, D>((c2d, s) => Over(a => other.Over(c2d)(a))(s));

        public Func<S, T> Over(Func<A, B> a2b) => s => _over(a2b, s);
    }

    public static class Setter
    {
        public static Setter<S, T, A, B> Create<S, T, A, B>(Func<Func<A, B>, S, T> over) => new Setter<S, T, A, B>(over);

        public static Func<S, T> Set<S, T, A, B>(this ISetter<S, T, A, B> setter, B val) => setter.Over(_ => val);
    }
}
