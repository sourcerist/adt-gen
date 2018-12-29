using System;
using System.Collections.Generic;
using System.Text;

namespace AlgebraicDataTypes.Optics
{
    public class Either<L,R>
    {
        public virtual bool IsLeft { get; } = false;
        public class Left : Either<L, R>
        {
            public L Value { get; }
            public Left(L value) => Value = value;
            public override bool IsLeft => true;
        }

        public virtual bool IsRight { get; } = false;
        public class Right : Either<L, R>
        {
            public R Value { get; }
            public Right(R value) => Value = value;
            public override bool IsRight => true;
        }
    }
}
