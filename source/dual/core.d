// Written in the D programming language.

/** This module contains the $(LREF Dual) type, which is used to represent
    dual numbers, along with related mathematical operations and functions.

    $(LREF Dual) will eventually
    $(DDLINK deprecate, Deprecated Features, replace)
    the built-in types `cfloat`, `cdouble`, `creal`, `ifloat`,
    `idouble`, and `ireal`.

    Macros:
        TABLE_SV = <table border="1" cellpadding="4" cellspacing="0">
                <caption>Special Values</caption>
                $0</table>
        PLUSMN = &plusmn;
        NAN = $(RED NAN)
        INFIN = &infin;
        PI = &pi;

    Authors:    Lars Tandle Kyllingstad, Don Clugston
    Copyright:  Copyright (c) 2010, Lars T. Kyllingstad.
    License:    $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0)
    Source:     $(PHOBOSSRC std/dual.d)
*/
module std.dual.core;

import std.traits;

/** Helper function that returns a dual number with the specified
    real and imaginary parts.

    Params:
        R = (template parameter) type of real part of dual number
        I = (template parameter) type of imaginary part of dual number

        re = real part of dual number to be constructed
        du = (optional) imaginary part of dual number, 0 if omitted.

    Returns:
        `Dual` instance with real and imaginary parts set
        to the values provided as input.  If neither `re` nor
        `du` are floating-point numbers, the return type will
        be `Dual!double`.  Otherwise, the return type is
        deduced using $(D std.traits.CommonType!(R, I)).
*/
auto dual(R)(const R re) @safe pure nothrow @nogc if (is(R : double)) {
    static if (isFloatingPoint!R)
        return Dual!R(re, 0);
    else
        return Dual!double(re, 0);
}

/// ditto
auto dual(R, I)(const R re, const I du) @safe pure nothrow @nogc if (is(R : double) && is(I : double)) {
    static if (isFloatingPoint!R || isFloatingPoint!I)
        return Dual!(CommonType!(R, I))(re, du);
    else
        return Dual!double(re, du);
}

///
@("Construction")
@safe pure nothrow unittest {
    auto a = dual(1.0);
    static assert(is(typeof(a) == Dual!double));
    assert(a.re == 1.0);
    assert(a.du == 0.0);

    auto b = dual(2.0L);
    static assert(is(typeof(b) == Dual!real));
    assert(b.re == 2.0L);
    assert(b.du == 0.0L);

    auto c = dual(1.0, 2.0);
    static assert(is(typeof(c) == Dual!double));
    assert(c.re == 1.0);
    assert(c.du == 2.0);

    auto d = dual(3.0, 4.0L);
    static assert(is(typeof(d) == Dual!real));
    assert(d.re == 3.0);
    assert(d.du == 4.0L);

    auto e = dual(1);
    static assert(is(typeof(e) == Dual!double));
    assert(e.re == 1);
    assert(e.du == 0);

    auto f = dual(1L, 2);
    static assert(is(typeof(f) == Dual!double));
    assert(f.re == 1L);
    assert(f.du == 2);

    auto g = dual(3, 4.0L);
    static assert(is(typeof(g) == Dual!real));
    assert(g.re == 3);
    assert(g.du == 4.0L);
}

/** A dual number parametrised by a type `T`, which must be either
    `float`, `double` or `real`.
*/
struct Dual(T) if (isFloatingPoint!T) {
    import std.format.spec : FormatSpec;
    import std.range.primitives : isOutputRange;

    /** The real part of the number. */
    T re;

    /** The dual part of the number. */
    T du;

    /** Converts the dual number to a string representation.

    The second form of this function is usually not called directly;
    instead, it is used via $(REF format, std,string), as shown in the examples
    below.  Supported format characters are 'e', 'f', 'g', 'a', and 's'.

    See the $(MREF std, format) and $(REF format, std,string)
    documentation for more information.
    */
    string toString() const @safe /* TODO: pure nothrow */ {
        import std.exception : assumeUnique;

        char[] buf;
        buf.reserve(100);
        auto fmt = FormatSpec!char("%s");
        toString((const(char)[] s) { buf ~= s; }, fmt);
        static trustedAssumeUnique(T)(T t) @trusted {
            return assumeUnique(t);
        }

        return trustedAssumeUnique(buf);
    }

    static if (is(T == double)) ///
        @("Formatting")
        @safe unittest {
            auto c = dual(1.2, 3.4);

            // Vanilla toString formatting:
            assert(c.toString() == "1.2+3.4e");

            // Formatting with std.string.format specs: the precision and width
            // specifiers apply to both the real and imaginary parts of the
            // dual number.
            import std.format : format;

            assert(format("%.2f", c) == "1.20+3.40e");
            assert(format("%4.1f", c) == " 1.2+ 3.4e");
        }

    /// ditto
    void toString(Writer, Char)(scope Writer w, scope const ref FormatSpec!Char formatSpec) const
    if (isOutputRange!(Writer, const(Char)[])) {
        import std.format.write : formatValue;
        import std.math.traits : signbit;
        import std.range.primitives : put;

        formatValue(w, re, formatSpec);
        if (signbit(du) == 0)
            put(w, "+");
        formatValue(w, du, formatSpec);
        put(w, "e"); // TODO: Replace with epsilon unicode char?
    }

@safe pure nothrow @nogc:

    /** Construct a dual number with the specified real and
    imaginary parts. In the case where a single argument is passed
    that is not dual, the imaginary part of the result will be
    zero.
    */
    this(R : T)(Dual!R d) {
        re = d.re;
        du = d.du;
    }

    /// ditto
    this(Rx : T, Ry:
        T)(const Rx x, const Ry y) {
        re = x;
        du = y;
    }

    /// ditto
    this(R : T)(const R r) {
        re = r;
        du = 0;
    }

    // ASSIGNMENT OPERATORS

    // this = dual
    ref Dual opAssign(R : T)(Dual!R d) {
        re = d.re;
        du = d.du;
        return this;
    }

    // this = numeric
    ref Dual opAssign(R : T)(const R r) {
        re = r;
        du = 0;
        return this;
    }

    // COMPARISON OPERATORS

    // this == dual
    bool opEquals(R : T)(Dual!R d) const {
        return re == d.re && du == d.du;
    }

    // this == numeric
    bool opEquals(R : T)(const R r) const {
        return re == r && du == 0;
    }

    // UNARY OPERATORS

    // +dual
    Dual opUnary(string op)() const if (op == "+") {
        return this;
    }

    // -dual
    Dual opUnary(string op)() const if (op == "-") {
        return Dual(-re, -du);
    }

    // BINARY OPERATORS

    // dual op dual
    // Boilerplate that generates i.e.
    // a + b --> { w = a; w += b; return w }
    Dual!(CommonType!(T, R)) opBinary(string op, R)(Dual!R d) const {
        alias C = typeof(return);
        auto w = C(this.re, this.du);
        return w.opOpAssign!(op)(d);
    }

    // dual op numeric
    // Boilerplate that generates i.e.
    // a + b --> { w = a; w += b; return w }
    Dual!(CommonType!(T, R)) opBinary(string op, R)(const R r) const
    if (isNumeric!R) {
        alias C = typeof(return);
        auto w = C(this.re, this.du);
        return w.opOpAssign!(op)(r);
    }

    // numeric + dual,  numeric * dual
    // Commutative, can switch the order to
    // dual + numeric, dual * numeric
    Dual!(CommonType!(T, R)) opBinaryRight(string op, R)(const R r) const
    if ((op == "+" || op == "*") && (isNumeric!R)) {
        return opBinary!(op)(r);
    }

    // numeric - dual
    Dual!(CommonType!(T, R)) opBinaryRight(string op, R)(const R r) const
    if (op == "-" && isNumeric!R) {
        return Dual(r - re, -du);
    }

    // numeric / dual
    Dual!(CommonType!(T, R)) opBinaryRight(string op, R)(const R r) const
    if (op == "/" && isNumeric!R) {
        typeof(return) w = void;
        w.re = r / re;
        w.du = -r * du / (re * re);
        return w;
    }

    // numeric ^^ dual
    Dual!(CommonType!(T, R)) opBinaryRight(string op, R)(const R lhs) const
    if (op == "^^" && isNumeric!R) {
        import std.math.exponential : log;

        Unqual!(CommonType!(T, R)) value = void, deriv = void;

        value = lhs ^^ re;
        deriv = value * log(lhs) * du;

        return typeof(return)(value, deriv);
    }

    // OP-ASSIGN OPERATORS

    // dual += dual,  dual -= dual
    ref Dual opOpAssign(string op, C)(const C d) if ((op == "+" || op == "-") && is(C R == Dual!R)) {
        mixin("re " ~ op ~ "= d.re;");
        mixin("du " ~ op ~ "= d.du;");
        return this;
    }

    // dual *= dual
    ref Dual opOpAssign(string op, C)(const C d) if (op == "*" && is(C R == Dual!R)) {
        this *= d.re;
        du += re * d.du;
        return this;
    }

    // dual /= dual
    ref Dual opOpAssign(string op, C)(const C d) if (op == "/" && is(C R == Dual!R)) {
        du = (d.re * du - re * d.du) / (d.re * d.re);
        re /= d.re;
        return this;
    }

    // dual ^^= dual
    ref Dual opOpAssign(string op, C)(const C d) if (op == "^^" && is(C R == Dual!R)) {
        import std.math.exponential : log;

        immutable temp = re ^^ d.re;
        du = temp * (d.re / re * du + log(re) * d.du);
        re = temp;
        return this;
    }

    // dual += numeric,  dual -= numeric
    ref Dual opOpAssign(string op, U: T)(const U a) if (op == "+" || op == "-") {
        mixin("re " ~ op ~ "= a;");
        return this;
    }

    // dual *= numeric,  dual /= numeric
    ref Dual opOpAssign(string op, U: T)(const U a) if (op == "*" || op == "/") {
        mixin("re " ~ op ~ "= a;");
        mixin("du " ~ op ~ "= a;");
        return this;
    }

    // dual ^^= real
    ref Dual opOpAssign(string op, R)(const R k) if (op == "^^" && isFloatingPoint!R) {
        immutable temp = re ^^ (k - 1);
        du *= k * temp;
        re *= temp;
        return this;
    }

    // dual ^^= int
    ref Dual opOpAssign(string op, U)(const U i) if (op == "^^" && isIntegral!U) {
        switch (i) {
        case 0:
            re = 1.0;
            du = 0.0;
            break;
        case 1:
            // identity; do nothing
            break;
        case 2:
            this *= this;
            break;
        case 3:
            auto d = this;
            this *= d;
            this *= d;
            break;
        default:
            this^^=cast(real) i;
        }
        return this;
    }
}

@("Overloads - Unary Maths Ops")
@safe pure nothrow unittest {
    import std.dual;
    static import core.math;
    import std.math;

    // Check unary operations.
    auto c1 = Dual!double(0.5, 2.0);

    assert(c1 == +c1);

    assert((-c1).re == -(c1.re));
    assert((-c1).du == -(c1.du));
    assert(c1 == -(-c1));
}

@("Overloads - Binary Maths Ops (Dual, Dual)")
@safe pure nothrow unittest {
    import std.math;

    enum EPS = double.epsilon;
    auto c1 = dual(1.0, 1.0);
    auto c2 = Dual!double(0.5, 2.0);

    // Check dual-dual operations.
    auto cpc = c1 + c2;
    assert(cpc.re == c1.re + c2.re);
    assert(cpc.du == c1.du + c2.du);

    auto cmc = c1 - c2;
    assert(cmc.re == c1.re - c2.re);
    assert(cmc.du == c1.du - c2.du);

    auto ctc = c1 * c2;
    assert(isClose(ctc.re, c1.re * c2.re, EPS));
    // assert(isClose(arg(ctc), arg(c1)+arg(c2), EPS));

    auto cdc = c1 / c2;
    assert(isClose(cdc.re, c1.re / c2.re, EPS));
    // assert(isClose(arg(cdc), arg(c1)-arg(c2), EPS));

    auto cec = c1 ^^ c2;
    assert(isClose(cec.re, 1.0, 1e-12));
    assert(isClose(cec.du, 0.5, 1e-12));
}

@("Overloads - Binary Maths Ops (Dual, Real)")
@safe pure nothrow unittest {
    import std.math;

    enum EPS = double.epsilon;
    auto c1 = dual(1.0, 1.0);
    auto c2 = Dual!double(0.5, 2.0);
    double a = 123.456;

    auto cpr = c1 + a;
    assert(cpr.re == c1.re + a);
    assert(cpr.du == c1.du);

    auto cmr = c1 - a;
    assert(cmr.re == c1.re - a);
    assert(cmr.du == c1.du);

    auto ctr = c1 * a;
    assert(ctr.re == c1.re * a);
    assert(ctr.du == c1.du * a);

    auto cdr = c1 / a;
    assert(isClose(cdr.re, c1.re / a, EPS));
    // assert(isClose(arg(cdr), arg(c1), EPS));

    auto cer = c1 ^^ 3.0;
    assert(isClose(cer.re, (c1.re) ^^ 3, EPS));
    // assert(isClose(arg(cer), arg(c1)*3, EPS));

    auto rpc = a + c1;
    assert(rpc == cpr);

    auto rmc = a - c1;
    assert(rmc.re == a - c1.re);
    assert(rmc.du == -c1.du);

    auto rtc = a * c1;
    assert(rtc == ctr);

    auto rdc = a / c1;
    assert(isClose(rdc.re, a / c1.re, EPS));
    // assert(isClose(arg(rdc), -arg(c1), EPS));

    rdc = a / c2;
    assert(isClose(rdc.re, a / c2.re, EPS));
    // assert(isClose(arg(rdc), -arg(c2), EPS));

    auto rec1a = 1.0 ^^ c1;
    assert(rec1a.re == 1.0);
    assert(rec1a.du == 0.0);

    auto rec2a = 1.0 ^^ c2;
    assert(rec2a.re == 1.0);
    assert(rec2a.du == 0.0);

    auto rec3a = 0.79 ^^ dual(6.8, 5.7);
    auto rec3b = dual(0.79, 0.0) ^^ dual(6.8, 5.7);
    assert(isClose(rec3a.re, rec3b.re, 1e-14));
    assert(isClose(rec3a.du, rec3b.du, 1e-14));

    auto rer = a ^^ dual(2.0, 0.0);
    auto rcheck = a ^^ 2.0;
    static assert(is(typeof(rcheck) == double));
    assert(feqrel(rer.re, rcheck) == double.mant_dig);
    assert(isIdentical(rer.re, rcheck));
    assert(rer.du == 0.0);

    auto rer2 = a ^^ dual(-2.0, 0.0);
    rcheck = a ^^ (-2.0);
    assert(feqrel(rer2.re, rcheck) == double.mant_dig);
    assert(isIdentical(rer2.re, rcheck));
    assert(rer2.du == 0.0);

    // Check Dual-int operations.
    foreach (i; 0 .. 6) {
        auto cei = c1 ^^ i;
        assert(isClose(cei.re, (c1.re) ^^ i, 1e-14));
        // Use cos() here to deal with arguments that go outside
        // the (-pi,pi] interval (only an issue for i>3).
        // assert(isClose(core.math.cos(arg(cei)), core.math.cos(arg(c1)*i), 1e-14));
    }

    // Check operations between different dual types.
    auto cf = Dual!float(1.0, 1.0);
    auto cr = Dual!real(1.0, 1.0);
    auto c1pcf = c1 + cf;
    auto c1pcr = c1 + cr;
    static assert(is(typeof(c1pcf) == Dual!double));
    static assert(is(typeof(c1pcr) == Dual!real));
    assert(c1pcf.re == c1pcr.re);
    assert(c1pcf.du == c1pcr.du);

    auto c1c = c1;
    auto c2c = c2;

    c1c /= c1;
    assert(isClose(c1c.re, 1.0, EPS));
    assert(isClose(c1c.du, 0.0, 0.0, EPS));

    c1c = c1;
    c1c /= c2;
    assert(isClose(c1c.re, 2.0, 1e-12));
    assert(isClose(c1c.du, -6.0, 1e-12));

    c2c /= c1;
    assert(isClose(c2c.re, 0.5, EPS));
    assert(isClose(c2c.du, 1.5, EPS));

    c2c = c2;
    c2c /= c2;
    assert(isClose(c2c.re, 1.0, EPS));
    assert(isClose(c2c.du, 0.0, 0.0, EPS));
}

@("Casting")
@safe pure nothrow unittest {
    // Initialization
    Dual!double a = 1;
    assert(a.re == 1 && a.du == 0);
    Dual!double b = 1.0;
    assert(b.re == 1.0 && b.du == 0);
    Dual!double c = Dual!real(1.0, 2);
    assert(c.re == 1.0 && c.du == 2);
}

@("Assignment casting")
@safe pure nothrow unittest {
    // Cast various values to Dual!double, and compare
    Dual!double d;

    d = 1;
    assert(d == 1);
    assert(d.re == 1.0 && d.du == 0.0);

    d = 2.0;
    assert(d == 2.0);
    assert(d.re == 2.0 && d.du == 0.0);

    d = 1.0L;
    assert(d == 1.0L);
    assert(d.re == 1.0 && d.du == 0.0);

    auto w = Dual!real(1.0, 1.0);
    d = w;
    assert(d == w);
    assert(d.re == 1.0 && d.du == 1.0);

    auto c = Dual!float(2.0, 2.0);
    d = c;
    assert(d == c);
    assert(d.re == 2.0 && d.du == 2.0);
}

/*  Makes Dual!(Dual!T) fold to Dual!T.

    The rationale for this is that just like the real line is a
    subspace of the dual plane, the dual plane is a subspace
    of itself.  Example of usage:
    ---
    Dual!T addI(T)(T x)
    {
        return x + Dual!T(0.0, 1.0);
    }
    ---
    The above will work if T is both real and dual.
*/
template Dual(T) if (is(T R == Dual!R)) {
    alias Dual = T;
}

/**
   Params: d = A dual number.
   Returns: The absolute value (or modulus) of `d`.
*/
T fabs(T)(Dual!T d) @safe pure nothrow @nogc {
    import core.math;

    return core.math.fabs(d.re);
}

///
@("abs")
@safe pure nothrow unittest {
    static import core.math;

    assert(fabs(dual(1.0)) == 1.0);
    assert(fabs(dual(0.0, 1.0)) == 0.0);
    assert(fabs(dual(1.0L, -2.0L)) == 1.0L);

    assert(fabs(dual(0.0L, -3.2L)) == 0.0L);
    assert(fabs(dual(0.0L, 71.6L)) == 0.0L);
    assert(fabs(dual(-1.0L, 1.0L)) == 1.0L);
}

/**
 * Extracts the norm of a dual number.
 * Params:
 *      d = A dual number
 * Returns:
 *      The squared magnitude of `d`.
 */
T norm(T)(Dual!T d) @safe pure nothrow @nogc {
    return d.re * d.re + d.du * d.du;
}

///
@("norm")
@safe pure nothrow @nogc unittest {
    import std.math.operations : isClose;
    import std.math.constants : PI;

    assert(norm(dual(3.0, 0.0)) == 9.0);
    assert(norm(dual(0.0, 4.0)) == 16.0);
    assert(norm(dual(3.0, 4.0)) == 25.0);
}

/**
  Params: d = A dual number.
  Returns: The dual conjugate of `d`.
*/
Dual!T conj(T)(Dual!T d) @safe pure nothrow @nogc {
    return Dual!T(d.re, -d.du);
}

///
@("conj")
@safe pure nothrow unittest {
    assert(conj(dual(1.0)) == dual(1.0));
    assert(conj(dual(1.0, 2.0)) == dual(1.0, -2.0));
    assert(conj(dual(0.0, 2.0)) == dual(0.0, -2.0));
}

@("conj (multiple types)")
@safe pure nothrow @nogc unittest {
    import std.meta : AliasSeq;

    static foreach (T; AliasSeq!(float, double, real)) {
        {
            auto c = Dual!T(7, 3L);
            assert(conj(c) == Dual!T(7, -3L));
            auto d = Dual!T(0, -3.2L);
            assert(conj(d) == -d);
        }
    }
}

// Helper function for comparing two Dual numbers.
int deqrel(T)(const Dual!T x, const Dual!T y) @safe pure nothrow @nogc {
    import std.math.operations : feqrel;

    const r = feqrel(x.re, y.re);
    const d = feqrel(x.du, y.du);
    return r < d ? r : d;
}

/**
    Params: d = A dual number.
    Returns: The square root of `d`.
*/
Dual!T sqrt(T)(Dual!T d) @safe pure nothrow @nogc {
    static import core.math;

    typeof(return) c;

    if (d == 0) {
        c = typeof(return)(0, 0);
    } else {
        c.re = core.math.sqrt(d.re);
        c.du = d.du / (2 * c.re);
    }
    return c;
}

@("sqrt")
@safe pure nothrow unittest {
    static import core.math;

    assert(sqrt(dual(0.0)) == 0.0);
    assert(sqrt(dual(1.0L, 0)) == core.math.sqrt(1.0L));
    assert(sqrt(dual(1.0, 1.0)) == dual(1.0, 0.5));
    // assert(sqrt(dual(-8.0, -6.0)) == dual(1.0, -3.0));
}

@("sqrt (v2)")
@safe pure nothrow unittest {
    import std.math.operations : isClose;

    auto c1 = dual(1.0, 1.0);
    auto c2 = Dual!double(0.5, 2.0);

    auto c1s = sqrt(c1);
    assert(isClose(c1s.re, 1.09868411347));
    assert(isClose(c1s.du, 0.455089860562));

    auto c2s = sqrt(c2);
    assert(isClose(c2s.re, 1.13171392428));
    assert(isClose(c2s.du, 0.883615530876));
}

// support %f formatting of dual numbers
// https://issues.dlang.org/show_bug.cgi?id=10881
@("More formatting?")
@safe unittest {
    import std.format : format;

    auto x = dual(1.2, 3.4);
    assert(format("%.2f", x) == "1.20+3.40i");

    auto y = dual(1.2, -3.4);
    assert(format("%.2f", y) == "1.20-3.40i");
}

@("More formatting again??")
@safe unittest {
    // Test wide string formatting
    import std.format.write : formattedWrite;

    wstring wformat(T)(string format, Dual!T c) {
        import std.array : appender;

        auto w = appender!wstring();
        auto n = formattedWrite(w, format, c);
        return w.data;
    }

    auto x = dual(1.2, 3.4);
    assert(wformat("%.2f", x) == "1.20+3.40i"w);
}

@("toString")
@safe unittest {
    // Test ease of use (vanilla toString() should be supported)
    assert(dual(1.2, 3.4).toString() == "1.2+3.4i");
}

@("More sqrt?")
@safe pure nothrow @nogc unittest {
    auto c = dual(3.0L, 4.0L);
    c = sqrt(c);
    assert(c.re == 2.0L);
    assert(c.du == 1.0L);
}
