using System;
using System.Collections.Generic;
using System.Text;

namespace AlgebraicDataTypes.Optics
{
    public class Either<L,R>
    {
        public class Left : Either<L, R>
        {
            public L Value { get; }
            public Left(L value) => Value = value;
        }

        public class Right : Either<L, R>
        {
            public R Value { get; }
            public Right(R value) => Value = value;
        }
    }
}
