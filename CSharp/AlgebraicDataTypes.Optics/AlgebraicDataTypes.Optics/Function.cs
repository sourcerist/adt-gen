using System;
using System.Collections.Generic;
using System.Text;

namespace AlgebraicDataTypes.Optics
{
    public static class Function
    {
        public static Func<A, C> Compose<A, B, C>(this Func<B, C> f, Func<A, B> g) => x => f(g(x));

        public static Func<A, C> ComposeWith<A, B, C>(this Func<A, B> g, Func<B, C> f) => x => f(g(x));
    }
}
