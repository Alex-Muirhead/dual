module std.dual.exponential;

import std.traits;
import std.dual.core;

/**
 * Calculates e$(SUPERSCRIPT x).
 * Params:
 *      x = A dual number
 * Returns:
 *      The dual base e exponential of `x`
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)                           $(TH exp(x)))
 *      $(TR $(TD ($(PLUSMN)0, +0))            $(TD (1, +0)))
 *      $(TR $(TD (any, +$(INFIN)))            $(TD ($(NAN), $(NAN))))
 *      $(TR $(TD (any, $(NAN))                $(TD ($(NAN), $(NAN)))))
 *      $(TR $(TD (+$(INFIN), +0))             $(TD (+$(INFIN), +0)))
 *      $(TR $(TD (-$(INFIN), any))            $(TD ($(PLUSMN)0, cis(x.du))))
 *      $(TR $(TD (+$(INFIN), any))            $(TD ($(PLUSMN)$(INFIN), cis(x.du))))
 *      $(TR $(TD (-$(INFIN), +$(INFIN)))      $(TD ($(PLUSMN)0, $(PLUSMN)0)))
 *      $(TR $(TD (+$(INFIN), +$(INFIN)))      $(TD ($(PLUSMN)$(INFIN), $(NAN))))
 *      $(TR $(TD (-$(INFIN), $(NAN)))         $(TD ($(PLUSMN)0, $(PLUSMN)0)))
 *      $(TR $(TD (+$(INFIN), $(NAN)))         $(TD ($(PLUSMN)$(INFIN), $(NAN))))
 *      $(TR $(TD ($(NAN), +0))                $(TD ($(NAN), +0)))
 *      $(TR $(TD ($(NAN), any))               $(TD ($(NAN), $(NAN))))
 *      $(TR $(TD ($(NAN), $(NAN)))            $(TD ($(NAN), $(NAN))))
 *      )
 */
Dual!T exp(T)(Dual!T x) @trusted pure nothrow @nogc // TODO: @safe
{
    static import std.math;

    auto re = std.math.exp(x.re);
    return Dual!T(re, x.du * re);
}

///
@("exp")
@safe pure nothrow @nogc unittest {
    import std.math.operations : isClose;
    import std.math.constants : E;

    assert(exp(dual(0.0, 0.0)) == dual(1.0, 0.0));

    auto a = dual(2.0, 1.0);
    assert(exp(conj(a)) == conj(exp(a)));

    auto b = exp(dual(1.0L, 1.0L));
    assert(isClose(b.re, E, 0.0, 1e-15));
}

@("exp (limits)")
@safe pure nothrow @nogc unittest {
    import std.math.operations : isClose;
    import std.math.traits : isNaN, isInfinity;

    import std.stdio;
    import std.format;

    auto a = exp(dual(0.0, +double.infinity));
    assert(isClose(a.re, 1.0) && a.du.isInfinity);
    auto b = exp(dual(0.0, -double.infinity));
    assert(isClose(b.re, 1.0) && b.du.isInfinity);
    auto c = exp(dual(0.0, double.nan));
    assert(isClose(c.re, 1.0) && c.du.isNaN);

    auto d = exp(dual(+double.infinity, 0.0));
    assert(d.re == double.infinity && d.du.isNaN);
    auto e = exp(dual(-double.infinity, 0.0));
    assert(e == dual(0.0));
    auto f = exp(dual(-double.infinity, 1.0));
    assert(f == dual(0.0));
    auto g = exp(dual(+double.infinity, 1.0));
    assert(g == dual(double.infinity, double.infinity));
    auto h = exp(dual(-double.infinity, +double.infinity));
    assert(h.re == 0.0 && h.du.isNaN);
    auto i = exp(dual(+double.infinity, +double.infinity));
    assert(i.re.isInfinity && i.du.isInfinity);
    auto j = exp(dual(-double.infinity, double.nan));
    assert(j.re == 0.0 && j.du.isNaN);
    auto k = exp(dual(+double.infinity, double.nan));
    assert(k.re.isInfinity && k.du.isNaN);

    auto l = exp(dual(double.nan, 0));
    assert(l.re.isNaN && l.du.isNaN);
    auto m = exp(dual(double.nan, 1));
    assert(m.re.isNaN && m.du.isNaN);
    auto n = exp(dual(double.nan, double.nan));
    assert(n.re.isNaN && n.du.isNaN);
}

@("more exp")
@safe pure nothrow @nogc unittest {
    import std.math.constants : E;
    import std.math.operations : isClose;

    auto a = exp(dual(0.0, 1.0));
    assert(isClose(a.re, 1.0));
    assert(isClose(a.du, 1.0));
    auto b = exp(dual(1.0, 2.0));
    assert(isClose(b.re, E));
    assert(isClose(b.du, 2 * E));
}

/**
 * Calculate the natural logarithm of x.
 * The branch cut is along the negative axis.
 * Params:
 *      x = A dual number
 * Returns:
 *      The dual natural logarithm of `x`
 *
 *      $(TABLE_SV
 *      $(TR $(TH x)                           $(TH log(x)))
 *      $(TR $(TD (-0, +0))                    $(TD (-$(INFIN), $(PI))))
 *      $(TR $(TD (+0, +0))                    $(TD (-$(INFIN), +0)))
 *      $(TR $(TD (any, +$(INFIN)))            $(TD (+$(INFIN), $(PI)/2)))
 *      $(TR $(TD (any, $(NAN)))               $(TD ($(NAN), $(NAN))))
 *      $(TR $(TD (-$(INFIN), any))            $(TD (+$(INFIN), $(PI))))
 *      $(TR $(TD (+$(INFIN), any))            $(TD (+$(INFIN), +0)))
 *      $(TR $(TD (-$(INFIN), +$(INFIN)))      $(TD (+$(INFIN), 3$(PI)/4)))
 *      $(TR $(TD (+$(INFIN), +$(INFIN)))      $(TD (+$(INFIN), $(PI)/4)))
 *      $(TR $(TD ($(PLUSMN)$(INFIN), $(NAN))) $(TD (+$(INFIN), $(NAN))))
 *      $(TR $(TD ($(NAN), any))               $(TD ($(NAN), $(NAN))))
 *      $(TR $(TD ($(NAN), +$(INFIN)))         $(TD (+$(INFIN), $(NAN))))
 *      $(TR $(TD ($(NAN), $(NAN)))            $(TD ($(NAN), $(NAN))))
 *      )
 */
Dual!T log(T)(Dual!T x) @safe pure nothrow @nogc {
    static import std.math;

    immutable deriv = x.du / x.re;
    return Dual!T(std.math.log(x.re), deriv);
}

///
@("log?")
@safe pure nothrow @nogc unittest {
    import core.math : sqrt;
    import std.math.operations : isClose;

    auto a = dual(2.0, 1.0);
    assert(log(conj(a)) == conj(log(a)));

    auto b = 2.0 * log10(dual(0.5, 0.5));
    auto c = 4.0 * log10(dual(sqrt(2.0) / 2, sqrt(2.0) / 4));
    assert(isClose(b.re, c.re, 0.0, 1e-15));
    assert(isClose(b.du, c.du, 0.0, 1e-15));
}

@("more log?")
@safe pure nothrow @nogc unittest {
    import std.math.traits : isNaN, isInfinity;
    import std.math.operations : isClose;
    import std.math.constants : PI, PI_2, PI_4;

    // NOTE: Don't compare the sign of NaN, as this is never
    //       defined by any specification
    auto a = log(dual(-0.0L, 0.0L));
    assert(a.re == -real.infinity);
    assert(a.du.isNaN);
    auto b = log(dual(0.0L, 0.0L));
    assert(b.re == -real.infinity);
    assert(b.du.isNaN);
    auto c = log(dual(1.0L, real.infinity));
    assert(c == dual(0.0, real.infinity));
    auto d = log(dual(1.0L, real.nan));
    assert(isClose(d.re, 0.0, 0.0, real.epsilon));
    assert(d.du.isNaN);

    auto e = log(dual(real.infinity, 1.0L));
    assert(e == dual(real.infinity, 0.0L));
    auto f = log(dual(real.infinity, real.infinity));
    assert(f.re == real.infinity && f.du.isNaN);
    auto g = log(dual(real.infinity, real.nan));
    assert(g.re.isInfinity && g.du.isNaN);

    auto h = log(dual(real.nan, 1.0L));
    assert(h.re.isNaN && h.du.isNaN);
    auto i = log(dual(real.nan, real.infinity));
    assert(i.re.isNaN && i.du.isNaN);
    auto j = log(dual(real.nan, real.nan));
    assert(j.re.isNaN && j.du.isNaN);
}

/**
 * Calculate the base-10 logarithm of x.
 * Params:
 *      x = A dual number
 * Returns:
 *      The dual base 10 logarithm of `x`
 */
Dual!T log10(T)(Dual!T x) @safe pure nothrow @nogc {
    import std.math.constants : LN10;

    return log(x) / Dual!T(LN10);
}

///
// @safe pure nothrow @nogc
@("log10")
unittest {
    import core.math : sqrt;
    import std.math.constants : LN10, PI;
    import std.math.operations : isClose;

    auto a = dual(2.0, 1.0);
    assert(log10(a) == log(a) / log(dual(10.0)));

    auto b = log10(dual(0.5, 1.0)) * 2.0;
    auto c = log10(dual(sqrt(2.0) / 2, sqrt(2.0) / 2)) * 4.0;
    assert(isClose(b.re, c.re, 0.0, 1e-15));
    assert(isClose(b.du, c.du, 0.0, 1e-15));
}

/**
 * Calculates x$(SUPERSCRIPT n).
 * The branch cut is on the negative axis.
 * Params:
 *      x = base
 *      n = exponent
 * Returns:
 *      `x` raised to the power of `n`
 */
Dual!T pow(T, Int)(Dual!T x, const Int n) @safe pure nothrow @nogc if (isIntegral!Int) {
    alias UInt = Unsigned!(Unqual!Int);

    UInt m = (n < 0) ? -cast(UInt) n : n;
    Dual!T y = (m % 2) ? x : Dual!T(1);

    while (m >>= 1) {
        x *= x;
        if (m % 2)
            y *= x;
    }

    return (n < 0) ? Dual!T(1) / y : y;
}

///
@("pow")
@safe pure nothrow @nogc unittest {
    import std.math.operations : isClose;

    auto a = dual(1.0, 2.0);
    assert(pow(a, 2) == a * a);
    assert(pow(a, 3) == a * a * a);
    assert(pow(a, -2) == 1.0 / (a * a));
    auto b = 1.0 / (a * a * a);
    assert(isClose(pow(a, -3).re, b.re));
    assert(isClose(pow(a, -3).du, b.du));
}

/// ditto
Dual!T pow(T)(Dual!T x, const T n) @trusted pure nothrow @nogc {
    static import std.math;

    if (x == 0.0)
        return Dual!T(0.0);

    auto value = std.math.pow(x.re, n);
    auto deriv = n * value / x.re;
    return Dual!T(value, x.du * deriv);
}

///
@("more pow")
@safe pure nothrow @nogc unittest {
    import std.math.operations : isClose;

    assert(pow(dual(0.0), 2.0) == dual(0.0));
    assert(pow(dual(5.0), 2.0) == dual(25.0));

    auto a = pow(dual(9.0, 0.0), 0.5);
    assert(isClose(a.re, 3.0));
    assert(isClose(a.du, 0.0, 0.0, 1e-16));

    auto b = pow(dual(4.0, -2.0), 0.5);
    assert(isClose(b.re, 2.0));
    assert(isClose(b.du, -0.5));
}

/// ditto
Dual!T pow(T)(Dual!T x, Dual!T y) @trusted pure nothrow @nogc {
    return (x == 0) ? Dual!T(0) : exp(y * log(x));
}

///
@("even more pow")
@safe pure nothrow @nogc unittest {
    import std.math.operations : isClose;
    import std.math.exponential : exp;
    import std.math.constants : PI;

    auto a = dual(0.0);
    auto b = dual(2.0);
    assert(pow(a, b) == dual(0.0));
}

/// ditto
Dual!T pow(T)(const T x, Dual!T n) @trusted pure nothrow @nogc {
    static import std.math;

    if (x == 0.0)
        return Dual!T(0.0);

    auto value = std.math.pow(x, n.re);
    auto deriv = value * std.math.log(x);

    return Dual!T(value, n.du * deriv);
}

///
@("even more more pow")
@safe pure nothrow @nogc unittest {
    import std.math.operations : isClose;

    assert(pow(2.0, dual(0.0)) == dual(1.0));
    assert(pow(2.0, dual(5.0)) == dual(32.0));

    auto a = pow(-2.0, dual(-1.0));
    assert(isClose(a.re, -0.5, 0.0, 1e-16));

    auto b = pow(-0.5, dual(-1.0));
    assert(isClose(b.re, -2.0, 0.0, 1e-15));
}

// @safe pure nothrow @nogc
@("sweet jesus how much pow")
unittest {
    import std.math.constants : PI;
    import std.math.operations : isClose;

    auto a = pow(dual(3.0, 4.0), 2);
    assert(isClose(a.re, 9.0));
    import std.stdio;

    assert(isClose(a.du, 24.0));

    auto b = pow(dual(3.0, 4.0), PI);
    assert(deqrel(b, dual(-152.91512205297134, 35.547499631917738)) >= double.mant_dig - 3);

    auto c = pow(dual(3.0, 4.0), dual(-2.0, 1.0));
    assert(deqrel(c, dual(0.015351734187477306, -0.0038407695456661503)) >= double.mant_dig - 3);

    auto d = pow(PI, dual(2.0, -1.0));
    assert(deqrel(d, dual(4.0790296880118296, -8.9872469554541869)) >= double.mant_dig - 1);

    auto e = dual(2.0);
    assert(deqrel(pow(e, 3), exp(3 * log(e))) >= double.mant_dig - 1);
}

@("surely this is the last pow")
@safe pure nothrow @nogc unittest {
    import std.meta : AliasSeq;
    import std.math : RealFormat, floatTraits;

    static foreach (T; AliasSeq!(float, double, real)) {
        {
            static if (floatTraits!T.realFormat == RealFormat.ibmExtended) {
                /* For IBM real, epsilon is too small (since 1.0 plus any double is
                representable) to be able to expect results within epsilon * 100.  */
            } else {
                T eps = T.epsilon * 100;

                T a = -1.0;
                T b = 0.5;
                Dual!T ref1 = pow(dual(a), dual(b));
                Dual!T res1 = pow(a, dual(b));
                Dual!T res2 = pow(dual(a), b);
                assert(deqrel(ref1, res1) > 12);
                assert(deqrel(ref1, res2) > 12);
                assert(deqrel(res1, res2) > 12);

                T c = -3.2;
                T d = 1.4;
                Dual!T ref2 = pow(dual(a), dual(b));
                Dual!T res3 = pow(a, dual(b));
                Dual!T res4 = pow(dual(a), b);
                assert(deqrel(ref2, res3) > 12);
                assert(deqrel(ref2, res4) > 12);
                assert(deqrel(res3, res4) > 12);
            }
        }
    }
}
