using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AlgebraicDataTypes.Optics
{
    public interface IPrism<S, T, A, B> : ITraversal<S, T, A, B>
    {
        Func<S, Either<T, A>> _s2t_or_a { get; } // fix this up later
        Func<B, T> Unto { get; }
        IPrism<S, T, C, D> ComposeWith<C, D>(IPrism<A, B, C, D> other);
    }

    public class Prism<S, T, A, B> : IPrism<S, T, A, B>
    {
        private Func<B, T> _b2t { get; }
        public Func<S, Either<T, A>> _s2t_or_a { get; }
        public Prism(Func<B, T> b2t, Func<S, Either<T, A>> s2t_or_a)
        {
            _b2t = b2t;
            _s2t_or_a = s2t_or_a;
        }

        public Func<B, T> Unto => _b2t;

        public IFold<S, C> ComposeWith<C>(IFold<A, C> other) => Fold.Create<S,C>(s => from a in ToEnumerableOf()(s) from c in other.ToEnumerableOf()(a) select c);

        public ISetter<S, T, C, D> ComposeWith<C, D>(ISetter<A, B, C, D> other) => Setter.Create<S, T, C, D>((c2d, s) => Over(other.Over(c2d))(s));

        public IPrism<S, T, C, D> ComposeWith<C, D>(IPrism<A, B, C, D> other)
        {
            Func<S, Either<T, C>> composed_s2t_or_a = s =>
            {
                switch (_s2t_or_a(s))
                {
                    case Either<T, A>.Left left: return new Either<T, C>.Left(left.Value);
                    case Either<T, A>.Right right:
                        switch (other._s2t_or_a(right.Value))
                        {
                            case Either<B, C>.Left innerLeft: return new Either<T, C>.Left(Unto(innerLeft.Value));
                            case Either<B, C>.Right innerRight: return new Either<T, C>.Right(innerRight.Value);
                            default: throw new ArgumentException("Unrecognized type");
                        }
                    default: throw new ArgumentException("Unrecognized type");
                }
            };

            return Prism.Create(Unto.Compose(other.Unto), composed_s2t_or_a);
        }

        public Func<S, T> Over(Func<A, B> f) =>
            s =>
            {
                switch (_s2t_or_a(s))
                {
                    case Either<T, A>.Left left: return left.Value;
                    case Either<T, A>.Right right: return _b2t(f(right.Value));
                    default: throw new ArgumentException("Unrecognized type");
                }
            }; 

        public Func<S, IEnumerable<A>> ToEnumerableOf() => YieldLeft;

        private IEnumerable<A> YieldLeft(S s)
        {
            if (_s2t_or_a(s) is Either<T, A>.Right right)
                yield return right.Value;
        }
    }

    public static class Prism
    {
        public static Prism<S, T, A, B> Create<S, T, A, B>(Func<B, T> b2t, Func<S, Either<T, A>> s2t_or_a) => new Prism<S, T, A, B>(b2t, s2t_or_a);
    }
}