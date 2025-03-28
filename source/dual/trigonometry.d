module std.dual.trigonometry;

import std.dual.core;
import std.dual.exponential;

/**
    Trigonometric functions on dual numbers.

    Params: d = A dual number.
    Returns: The sine, cosine and tangent of `d`, respectively.
*/
Dual!T sin(T)(Dual!T d) @safe pure nothrow @nogc {
    auto cs = expi(d.re);
    auto csh = coshisinh(d.du);
    return typeof(return)(cs.du * csh.re, cs.re * csh.du);
}

///
@("sin")
@safe pure nothrow unittest {
    static import core.math;

    assert(sin(dual(0.0)) == 0.0);
    assert(sin(dual(2.0, 0)) == core.math.sin(2.0));
}

@("sin (v2)")
@safe pure nothrow unittest {
    static import core.math;

    assert(deqrel(sin(dual(2.0L, 0)), dual(core.math.sin(2.0L))) >= real.mant_dig - 1);
}

/// ditto
Dual!T cos(T)(Dual!T d) @safe pure nothrow @nogc {
    auto cs = expi(d.re);
    auto csh = coshisinh(d.du);
    return typeof(return)(cs.re * csh.re, -cs.du * csh.du);
}

///
@("cos")
@safe pure nothrow unittest {
    static import core.math;
    static import std.math;

    assert(cos(dual(0.0)) == 1.0);
    assert(cos(dual(1.3, 0.0)) == core.math.cos(1.3));
    assert(cos(dual(0.0, 5.2)) == std.math.cosh(5.2));
}

@("cos (v2)")
@safe pure nothrow unittest {
    static import core.math;
    static import std.math;

    assert(deqrel(cos(dual(0, 5.2L)), dual(std.math.cosh(5.2L), 0.0L)) >= real.mant_dig - 1);
    assert(deqrel(cos(dual(1.3L)), dual(core.math.cos(1.3L))) >= real.mant_dig - 1);
}

/// ditto
Dual!T tan(T)(Dual!T d) @safe pure nothrow @nogc {
    return sin(d) / cos(d);
}

///
@("tan")
@safe pure nothrow @nogc unittest {
    static import std.math;

    int deqrel(T)(const Dual!T x, const Dual!T y) @safe pure nothrow @nogc {
        import std.math.operations : feqrel;

        const r = feqrel(x.re, y.re);
        const i = feqrel(x.du, y.du);
        return r < i ? r : i;
    }

    assert(deqrel(tan(dual(1.0, 0.0)), dual(std.math.tan(1.0), 0.0)) >= double.mant_dig - 2);
    assert(deqrel(tan(dual(0.0, 1.0)), dual(0.0, std.math.tanh(1.0))) >= double.mant_dig - 2);
}

/**
    Inverse trigonometric functions on dual numbers.

    Params: d = A dual number.
    Returns: The arcsine, arccosine and arctangent of `d`, respectively.
*/
Dual!T asin(T)(Dual!T d) @safe pure nothrow @nogc {
    import std.math : asin, sqrt;

    auto value = asin(d.re);
    auto deriv = 1 / sqrt(1 - d.re * d.re);
    return Dual!T(value, deriv * d.du);
}

///
@("asin")
@safe pure nothrow unittest {
    import std.math.operations : isClose;
    import std.math.constants : PI;

    assert(asin(dual(0.0)) == 0.0);
    assert(isClose(asin(dual(0.5L)).re, PI / 6));
}

@("asin (v2)")
@safe pure nothrow unittest {
    import std.math.operations : isClose;
    import std.math.constants : PI;

    version (DigitalMars) {
    } else // Disabled because of https://issues.dlang.org/show_bug.cgi?id=21376
        assert(isClose(asin(dual(0.5f)).re, float(PI) / 6));
}

/// ditto
Dual!T acos(T)(Dual!T d) @safe pure nothrow @nogc {
    static import std.math;

    auto as = asin(d);
    return Dual!T(T(std.math.PI_2) - as.re, -as.du);
}

///
@("acos")
@safe pure nothrow unittest {
    import std.math.operations : isClose;
    import std.math.constants : PI;
    import std.math.trigonometry : std_math_acos = acos;

    assert(acos(dual(0.0)) == std_math_acos(0.0));
    assert(isClose(acos(dual(0.5L)).re, PI / 3));
}

@("acos (v2)")
@safe pure nothrow unittest {
    import std.math.operations : isClose;
    import std.math.constants : PI;

    version (DigitalMars) {
    } else // Disabled because of https://issues.dlang.org/show_bug.cgi?id=21376
        assert(isClose(acos(dual(0.5f)).re, float(PI) / 3));
}

/// ditto
Dual!T atan(T)(Dual!T d) @safe pure nothrow @nogc {
    static import std.math;

    const T re2 = d.re * d.re;
    const T x = 1 - re2 - d.du * d.du;

    T num = d.du + 1;
    T den = d.du - 1;

    num = re2 + num * num;
    den = re2 + den * den;

    return Dual!T(T(0.5) * std.math.atan2(2 * d.re, x), T(0.25) * std.math.log(num / den));
}

///
@("atan")
@safe pure nothrow @nogc unittest {
    import std.math.operations : isClose;
    import std.math.constants : PI;

    assert(atan(dual(0.0)) == 0.0);
    assert(isClose(atan(sqrt(dual(3.0L))).re, PI / 3));
    assert(isClose(atan(sqrt(dual(3.0f))).re, float(PI) / 3));
}

/**
    Hyperbolic trigonometric functions on dual numbers.

    Params: d = A dual number.
    Returns: The hyperbolic sine, cosine and tangent of `d`, respectively.
*/
Dual!T sinh(T)(Dual!T d) @safe pure nothrow @nogc {
    static import core.math, std.math;

    return Dual!T(std.math.sinh(d.re) * core.math.cos(d.du),
        std.math.cosh(d.re) * core.math.sin(d.du));
}

///
@("sinh")
@safe pure nothrow unittest {
    static import std.math;

    assert(sinh(dual(0.0)) == 0.0);
    assert(sinh(dual(1.0L)) == std.math.sinh(1.0L));
    assert(sinh(dual(1.0f)) == std.math.sinh(1.0f));
}

/// ditto
Dual!T cosh(T)(Dual!T d) @safe pure nothrow @nogc {
    static import core.math, std.math;

    return Dual!T(std.math.cosh(d.re) * core.math.cos(d.du),
        std.math.sinh(d.re) * core.math.sin(d.du));
}

///
@("cosh")
@safe pure nothrow unittest {
    static import std.math;

    assert(cosh(dual(0.0)) == 1.0);
    assert(cosh(dual(1.0L)) == std.math.cosh(1.0L));
    assert(cosh(dual(1.0f)) == std.math.cosh(1.0f));
}

/// ditto
Dual!T tanh(T)(Dual!T d) @safe pure nothrow @nogc {
    return sinh(d) / cosh(d);
}

///
@("tanh")
@safe pure nothrow @nogc unittest {
    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_tanh = tanh;

    assert(tanh(dual(0.0)) == 0.0);
    assert(isClose(tanh(dual(1.0L)).re, std_math_tanh(1.0L)));
    assert(isClose(tanh(dual(1.0f)).re, std_math_tanh(1.0f)));
}

/**
    Inverse hyperbolic trigonometric functions on dual numbers.

    Params: d = A dual number.
    Returns: The hyperbolic arcsine, arccosine and arctangent of `d`, respectively.
*/
Dual!T asinh(T)(Dual!T d) @safe pure nothrow @nogc {
    auto t = Dual!T((d.re - d.du) * (d.re + d.du) + 1, 2 * d.re * d.du);
    return log(sqrt(t) + d);
}

///
@("asinh")
@safe pure nothrow unittest {
    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_asinh = asinh;

    assert(asinh(dual(0.0)) == 0.0);
    assert(isClose(asinh(dual(1.0L)).re, std_math_asinh(1.0L)));
    assert(isClose(asinh(dual(1.0f)).re, std_math_asinh(1.0f)));
}

/// ditto
Dual!T acosh(T)(Dual!T d) @safe pure nothrow @nogc {
    return 2 * log(sqrt(T(0.5) * (d + 1)) + sqrt(T(0.5) * (d - 1)));
}

///
@("acosh")
@safe pure nothrow unittest {
    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_acosh = acosh;

    assert(acosh(dual(1.0)) == 0.0);
    assert(isClose(acosh(dual(3.0L)).re, std_math_acosh(3.0L)));
    assert(isClose(acosh(dual(3.0f)).re, std_math_acosh(3.0f)));
}

/// ditto
Dual!T atanh(T)(Dual!T d) @safe pure nothrow @nogc {
    static import std.math;

    const T im2 = d.du * d.du;
    const T x = 1 - im2 - d.re * d.re;

    T num = 1 + d.re;
    T den = 1 - d.re;

    num = im2 + num * num;
    den = im2 + den * den;

    return Dual!T(T(0.25) * (std.math.log(num) - std.math.log(den)),
        T(0.5) * std.math.atan2(2 * d.du, x));
}

///
@("atanh")
@safe pure nothrow @nogc unittest {
    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_atanh = atanh;

    assert(atanh(dual(0.0)) == 0.0);
    assert(isClose(atanh(dual(0.5L)).re, std_math_atanh(0.5L)));
    assert(isClose(atanh(dual(0.5f)).re, std_math_atanh(0.5f)));
}

/**
    Params: y = A real number.
    Returns: The value of cos(y) + i sin(y).

    Note:
    `expi` is included here for convenience and for easy migration of code.
*/
Dual!real expi(real y) @trusted pure nothrow @nogc {
    import core.math : cos, sin;

    return Dual!real(cos(y), sin(y));
}

///
@("expi??")
@safe pure nothrow unittest {
    import core.math : cos, sin;

    assert(expi(0.0L) == 1.0L);
    assert(expi(1.3e5L) == dual(cos(1.3e5L), sin(1.3e5L)));
}

/**
    Params: y = A real number.
    Returns: The value of cosh(y) + i sinh(y)

    Note:
    `coshisinh` is included here for convenience and for easy migration of code.
*/
Dual!real coshisinh(real y) @safe pure nothrow @nogc {
    static import core.math;
    static import std.math;

    if (core.math.fabs(y) <= 0.5)
        return Dual!real(std.math.cosh(y), std.math.sinh(y));
    else {
        auto d = std.math.exp(y);
        auto zi = 0.5 / d;
        d = 0.5 * d;
        return Dual!real(d + zi, d - zi);
    }
}

///
@("coshisinh??")
@safe pure nothrow @nogc unittest {
    import std.math.trigonometry : cosh, sinh;

    assert(coshisinh(3.0L) == dual(cosh(3.0L), sinh(3.0L)));
}
