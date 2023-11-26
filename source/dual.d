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
module std.dual;

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
auto dual(R)(const R re)  @safe pure nothrow @nogc
if (is(R : double))
{
    static if (isFloatingPoint!R)
        return Dual!R(re, 0);
    else
        return Dual!double(re, 0);
}

/// ditto
auto dual(R, I)(const R re, const I du)  @safe pure nothrow @nogc
if (is(R : double) && is(I : double))
{
    static if (isFloatingPoint!R || isFloatingPoint!I)
        return Dual!(CommonType!(R, I))(re, du);
    else
        return Dual!double(re, du);
}

///
@safe pure nothrow unittest
{
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
struct Dual(T)
if (isFloatingPoint!T)
{
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
    string toString() const @safe /* TODO: pure nothrow */
    {
        import std.exception : assumeUnique;
        char[] buf;
        buf.reserve(100);
        auto fmt = FormatSpec!char("%s");
        toString((const(char)[] s) { buf ~= s; }, fmt);
        static trustedAssumeUnique(T)(T t) @trusted { return assumeUnique(t); }
        return trustedAssumeUnique(buf);
    }

    static if (is(T == double))
    ///
    @safe unittest
    {
        auto c = dual(1.2, 3.4);

        // Vanilla toString formatting:
        assert(c.toString() == "1.2+3.4i");

        // Formatting with std.string.format specs: the precision and width
        // specifiers apply to both the real and imaginary parts of the
        // dual number.
        import std.format : format;
        assert(format("%.2f", c)  == "1.20+3.40i");
        assert(format("%4.1f", c) == " 1.2+ 3.4i");
    }

    /// ditto
    void toString(Writer, Char)(scope Writer w, scope const ref FormatSpec!Char formatSpec) const
        if (isOutputRange!(Writer, const(Char)[]))
    {
        import std.format.write : formatValue;
        import std.math.traits : signbit;
        import std.range.primitives : put;
        formatValue(w, re, formatSpec);
        if (signbit(du) == 0)
           put(w, "+");
        formatValue(w, du, formatSpec);
        put(w, "i");
    }

@safe pure nothrow @nogc:

    /** Construct a dual number with the specified real and
    imaginary parts. In the case where a single argument is passed
    that is not dual, the imaginary part of the result will be
    zero.
    */
    this(R : T)(Dual!R d)
    {
        re = d.re;
        du = d.du;
    }

    /// ditto
    this(Rx : T, Ry : T)(const Rx x, const Ry y)
    {
        re = x;
        du = y;
    }

    /// ditto
    this(R : T)(const R r)
    {
        re = r;
        du = 0;
    }

    // ASSIGNMENT OPERATORS

    // this = dual
    ref Dual opAssign(R : T)(Dual!R d)
    {
        re = d.re;
        du = d.du;
        return this;
    }

    // this = numeric
    ref Dual opAssign(R : T)(const R r)
    {
        re = r;
        du = 0;
        return this;
    }

    // COMPARISON OPERATORS

    // this == dual
    bool opEquals(R : T)(Dual!R d) const
    {
        return re == d.re && du == d.du;
    }

    // this == numeric
    bool opEquals(R : T)(const R r) const
    {
        return re == r && du == 0;
    }

    // UNARY OPERATORS

    // +dual
    Dual opUnary(string op)() const
        if (op == "+")
    {
        return this;
    }

    // -dual
    Dual opUnary(string op)() const
        if (op == "-")
    {
        return Dual(-re, -du);
    }

    // BINARY OPERATORS

    // dual op dual
    Dual!(CommonType!(T,R)) opBinary(string op, R)(Dual!R d) const
    {
        alias C = typeof(return);
        auto w = C(this.re, this.du);
        return w.opOpAssign!(op)(d);
    }

    // dual op numeric
    Dual!(CommonType!(T,R)) opBinary(string op, R)(const R r) const
        if (isNumeric!R)
    {
        alias C = typeof(return);
        auto w = C(this.re, this.du);
        return w.opOpAssign!(op)(r);
    }

    // numeric + dual,  numeric * dual
    Dual!(CommonType!(T, R)) opBinaryRight(string op, R)(const R r) const
        if ((op == "+" || op == "*") && (isNumeric!R))
    {
        return opBinary!(op)(r);
    }

    // numeric - dual
    Dual!(CommonType!(T, R)) opBinaryRight(string op, R)(const R r) const
        if (op == "-" && isNumeric!R)
    {
        return Dual(r - re, -du);
    }

    // numeric / dual
    Dual!(CommonType!(T, R)) opBinaryRight(string op, R)(const R r) const
        if (op == "/" && isNumeric!R)
    {
        version (FastMath)
        {
            // Compute norm(this)
            immutable norm = re * re + du * du;
            // Compute r * conj(this)
            immutable prod_re = r * re;
            immutable prod_im = r * -du;
            // Divide the product by the norm
            typeof(return) w = void;
            w.re = prod_re / norm;
            w.du = prod_im / norm;
            return w;
        }
        else
        {
            import core.math : fabs;
            typeof(return) w = void;
            if (fabs(re) < fabs(du))
            {
                immutable ratio = re/du;
                immutable rdivd = r/(re*ratio + du);

                w.re = rdivd*ratio;
                w.du = -rdivd;
            }
            else
            {
                immutable ratio = du/re;
                immutable rdivd = r/(re + du*ratio);

                w.re = rdivd;
                w.du = -rdivd*ratio;
            }

            return w;
        }
    }

    // numeric ^^ dual
    Dual!(CommonType!(T, R)) opBinaryRight(string op, R)(const R lhs) const
        if (op == "^^" && isNumeric!R)
    {
        import core.math : cos, sin;
        import std.math.exponential : exp, log;
        import std.math.constants : PI;
        Unqual!(CommonType!(T, R)) ab = void, ar = void;

        if (lhs >= 0)
        {
            // r = lhs
            // theta = 0
            ab = lhs ^^ this.re;
            ar = log(lhs) * this.du;
        }
        else
        {
            // r = -lhs
            // theta = PI
            ab = (-lhs) ^^ this.re * exp(-PI * this.du);
            ar = PI * this.re + log(-lhs) * this.du;
        }

        return typeof(return)(ab * cos(ar), ab * sin(ar));
    }

    // OP-ASSIGN OPERATORS

    // dual += dual,  dual -= dual
    ref Dual opOpAssign(string op, C)(const C d)
        if ((op == "+" || op == "-") && is(C R == Dual!R))
    {
        mixin ("re "~op~"= d.re;");
        mixin ("du "~op~"= d.du;");
        return this;
    }

    // dual *= dual
    ref Dual opOpAssign(string op, C)(const C d)
        if (op == "*" && is(C R == Dual!R))
    {
        auto temp = re*d.re - du*d.du;
        du = du*d.re + re*d.du;
        re = temp;
        return this;
    }

    // dual /= dual
    ref Dual opOpAssign(string op, C)(const C d)
        if (op == "/" && is(C R == Dual!R))
    {
        version (FastMath)
        {
            // Compute norm(d)
            immutable norm = d.re * d.re + d.du * d.du;
            // Compute this * conj(d)
            immutable prod_re = re * d.re - du * -d.du;
            immutable prod_im = du * d.re + re * -d.du;
            // Divide the product by the norm
            re = prod_re / norm;
            du = prod_im / norm;
            return this;
        }
        else
        {
            import core.math : fabs;
            if (fabs(d.re) < fabs(d.du))
            {
                immutable ratio = d.re/d.du;
                immutable denom = d.re*ratio + d.du;

                immutable temp = (re*ratio + du)/denom;
                du = (du*ratio - re)/denom;
                re = temp;
            }
            else
            {
                immutable ratio = d.du/d.re;
                immutable denom = d.re + d.du*ratio;

                immutable temp = (re + du*ratio)/denom;
                du = (du - re*ratio)/denom;
                re = temp;
            }
            return this;
        }
    }

    // dual ^^= dual
    ref Dual opOpAssign(string op, C)(const C d)
        if (op == "^^" && is(C R == Dual!R))
    {
        import std.math.exponential : log;
        immutable temp = re^^d.re;
        du = temp * (d.re/re*du + log(re)*d.du);
        re = temp;
        return this;
    }

    // dual += numeric,  dual -= numeric
    ref Dual opOpAssign(string op, U : T)(const U a)
        if (op == "+" || op == "-")
    {
        mixin ("re "~op~"= a;");
        return this;
    }

    // dual *= numeric,  dual /= numeric
    ref Dual opOpAssign(string op, U : T)(const U a)
        if (op == "*" || op == "/")
    {
        mixin ("re "~op~"= a;");
        mixin ("du "~op~"= a;");
        return this;
    }

    // dual ^^= real
    ref Dual opOpAssign(string op, R)(const R k)
        if (op == "^^" && isFloatingPoint!R)
    {
        immutable temp = re^^(k-1);
        du *= k * temp;
        re *= temp;
        return this;
    }

    // dual ^^= int
    ref Dual opOpAssign(string op, U)(const U i)
        if (op == "^^" && isIntegral!U)
    {
        switch (i)
        {
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
            this ^^= cast(real) i;
        }
        return this;
    }
}

@safe pure nothrow unittest
{
    import std.dual;
    static import core.math;
    import std.math;

    enum EPS = double.epsilon;
    auto c1 = dual(1.0, 1.0);

    // Check unary operations.
    auto c2 = Dual!double(0.5, 2.0);

    assert(c2 == +c2);

    assert((-c2).re == -(c2.re));
    assert((-c2).du == -(c2.du));
    assert(c2 == -(-c2));

    // Check dual-dual operations.
    auto cpc = c1 + c2;
    assert(cpc.re == c1.re + c2.re);
    assert(cpc.du == c1.du + c2.du);

    auto cmc = c1 - c2;
    assert(cmc.re == c1.re - c2.re);
    assert(cmc.du == c1.du - c2.du);

    auto ctc = c1 * c2;
    // assert(isClose(abs(ctc), abs(c1)*abs(c2), EPS));
    // assert(isClose(arg(ctc), arg(c1)+arg(c2), EPS));

    auto cdc = c1 / c2;
    // assert(isClose(abs(cdc), abs(c1)/abs(c2), EPS));
    // assert(isClose(arg(cdc), arg(c1)-arg(c2), EPS));

    auto cec = c1^^c2;
    assert(isClose(cec.re, 0.1152413197994, 1e-12));
    assert(isClose(cec.du, 0.2187079045274, 1e-12));

    // Check dual-real operations.
    double a = 123.456;

    auto cpr = c1 + a;
    assert(cpr.re == c1.re + a);
    assert(cpr.du == c1.du);

    auto cmr = c1 - a;
    assert(cmr.re == c1.re - a);
    assert(cmr.du == c1.du);

    auto ctr = c1 * a;
    assert(ctr.re == c1.re*a);
    assert(ctr.du == c1.du*a);

    auto cdr = c1 / a;
    // assert(isClose(abs(cdr), abs(c1)/a, EPS));
    // assert(isClose(arg(cdr), arg(c1), EPS));

    auto cer = c1^^3.0;
    // assert(isClose(abs(cer), abs(c1)^^3, EPS));
    // assert(isClose(arg(cer), arg(c1)*3, EPS));

    auto rpc = a + c1;
    assert(rpc == cpr);

    auto rmc = a - c1;
    assert(rmc.re == a-c1.re);
    assert(rmc.du == -c1.du);

    auto rtc = a * c1;
    assert(rtc == ctr);

    auto rdc = a / c1;
    // assert(isClose(abs(rdc), a/abs(c1), EPS));
    // assert(isClose(arg(rdc), -arg(c1), EPS));

    rdc = a / c2;
    // assert(isClose(abs(rdc), a/abs(c2), EPS));
    // assert(isClose(arg(rdc), -arg(c2), EPS));

    auto rec1a = 1.0 ^^ c1;
    assert(rec1a.re == 1.0);
    assert(rec1a.du == 0.0);

    auto rec2a = 1.0 ^^ c2;
    assert(rec2a.re == 1.0);
    assert(rec2a.du == 0.0);

    auto rec1b = (-1.0) ^^ c1;
    // assert(isClose(abs(rec1b), std.math.exp(-PI * c1.du), EPS));
    // auto arg1b = arg(rec1b);
    /* The argument _should_ be PI, but floating-point rounding error
     * means that in fact the imaginary part is very slightly negative.
     */
    // assert(isClose(arg1b, PI, EPS) || isClose(arg1b, -PI, EPS));

    auto rec2b = (-1.0) ^^ c2;
    // assert(isClose(abs(rec2b), std.math.exp(-2 * PI), EPS));
    // assert(isClose(arg(rec2b), PI_2, EPS));

    auto rec3a = 0.79 ^^ dual(6.8, 5.7);
    auto rec3b = dual(0.79, 0.0) ^^ dual(6.8, 5.7);
    assert(isClose(rec3a.re, rec3b.re, 1e-14));
    assert(isClose(rec3a.du, rec3b.du, 1e-14));

    auto rec4a = (-0.79) ^^ dual(6.8, 5.7);
    auto rec4b = dual(-0.79, 0.0) ^^ dual(6.8, 5.7);
    assert(isClose(rec4a.re, rec4b.re, 1e-14));
    assert(isClose(rec4a.du, rec4b.du, 1e-14));

    auto rer = a ^^ dual(2.0, 0.0);
    auto rcheck = a ^^ 2.0;
    static assert(is(typeof(rcheck) == double));
    assert(feqrel(rer.re, rcheck) == double.mant_dig);
    assert(isIdentical(rer.re, rcheck));
    assert(rer.du == 0.0);

    auto rer2 = (-a) ^^ dual(2.0, 0.0);
    rcheck = (-a) ^^ 2.0;
    assert(feqrel(rer2.re, rcheck) == double.mant_dig);
    assert(isIdentical(rer2.re, rcheck));
    assert(isClose(rer2.du, 0.0, 0.0, 1e-10));

    auto rer3 = (-a) ^^ dual(-2.0, 0.0);
    rcheck = (-a) ^^ (-2.0);
    assert(feqrel(rer3.re, rcheck) == double.mant_dig);
    assert(isIdentical(rer3.re, rcheck));
    assert(isClose(rer3.du, 0.0, 0.0, EPS));

    auto rer4 = a ^^ dual(-2.0, 0.0);
    rcheck = a ^^ (-2.0);
    assert(feqrel(rer4.re, rcheck) == double.mant_dig);
    assert(isIdentical(rer4.re, rcheck));
    assert(rer4.du == 0.0);

    // Check Dual-int operations.
    foreach (i; 0 .. 6)
    {
        auto cei = c1^^i;
        assert(isClose(abs(cei), abs(c1)^^i, 1e-14));
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
    assert(isClose(c1c.re, 0.5882352941177, 1e-12));
    assert(isClose(c1c.du, -0.3529411764706, 1e-12));

    c2c /= c1;
    assert(isClose(c2c.re, 1.25, EPS));
    assert(isClose(c2c.du, 0.75, EPS));

    c2c = c2;
    c2c /= c2;
    assert(isClose(c2c.re, 1.0, EPS));
    assert(isClose(c2c.du, 0.0, 0.0, EPS));
}

@safe pure nothrow unittest
{
    // Initialization
    Dual!double a = 1;
    assert(a.re == 1 && a.du == 0);
    Dual!double b = 1.0;
    assert(b.re == 1.0 && b.du == 0);
    Dual!double c = Dual!real(1.0, 2);
    assert(c.re == 1.0 && c.du == 2);
}

@safe pure nothrow unittest
{
    // Assignments and comparisons
    Dual!double d;

    d = 1;
    assert(d == 1);
    assert(d.re == 1.0  &&  d.du == 0.0);

    d = 2.0;
    assert(d == 2.0);
    assert(d.re == 2.0  &&  d.du == 0.0);

    d = 1.0L;
    assert(d == 1.0L);
    assert(d.re == 1.0  &&  d.du == 0.0);

    auto w = Dual!real(1.0, 1.0);
    d = w;
    assert(d == w);
    assert(d.re == 1.0  &&  d.du == 1.0);

    auto c = Dual!float(2.0, 2.0);
    d = c;
    assert(d == c);
    assert(d.re == 2.0  &&  d.du == 2.0);
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
template Dual(T)
if (is(T R == Dual!R))
{
    alias Dual = T;
}

@safe pure nothrow unittest
{
    static assert(is(Dual!(Dual!real) == Dual!real));

    Dual!T addI(T)(T x)
    {
        return x + Dual!T(0.0, 1.0);
    }

    auto z1 = addI(1.0);
    assert(z1.re == 1.0 && z1.du == 1.0);

    enum one = Dual!double(1.0, 0.0);
    auto z2 = addI(one);
    assert(z1 == z2);
}


/**
   Params: d = A dual number.
   Returns: The absolute value (or modulus) of `d`.
*/
T abs(T)(Dual!T d) @safe pure nothrow @nogc
{
    import std.math.algebraic : hypot;
    return hypot(d.re, d.du);
}

///
@safe pure nothrow unittest
{
    static import core.math;
    assert(abs(dual(1.0)) == 1.0);
    assert(abs(dual(0.0, 1.0)) == 1.0);
    assert(abs(dual(1.0L, -2.0L)) == core.math.sqrt(5.0L));
}

@safe pure nothrow @nogc unittest
{
    static import core.math;
    assert(abs(dual(0.0L, -3.2L)) == 3.2L);
    assert(abs(dual(0.0L, 71.6L)) == 71.6L);
    assert(abs(dual(-1.0L, 1.0L)) == core.math.sqrt(2.0L));
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    static foreach (T; AliasSeq!(float, double, real))
    {{
        static import std.math;
        Dual!T a = dual(T(-12), T(3));
        T b = std.math.hypot(a.re, a.du);
        assert(std.math.isClose(abs(a), b));
        assert(std.math.isClose(abs(-a), b));
    }}
}

/++
   Params:
    d = A dual number.
    x = A real number.
   Returns: The squared modulus of `d`.
   For genericity, if called on a real number, returns its square.
+/
T sqAbs(T)(Dual!T d) @safe pure nothrow @nogc
{
    return d.re*d.re + d.du*d.du;
}

///
@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    assert(sqAbs(dual(0.0)) == 0.0);
    assert(sqAbs(dual(1.0)) == 1.0);
    assert(sqAbs(dual(0.0, 1.0)) == 1.0);
    assert(isClose(sqAbs(dual(1.0L, -2.0L)), 5.0L));
    assert(isClose(sqAbs(dual(-3.0L, 1.0L)), 10.0L));
    assert(isClose(sqAbs(dual(1.0f,-1.0f)), 2.0f));
}

/// ditto
T sqAbs(T)(const T x) @safe pure nothrow @nogc
if (isFloatingPoint!T)
{
    return x*x;
}

@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    assert(sqAbs(0.0) == 0.0);
    assert(sqAbs(-1.0) == 1.0);
    assert(isClose(sqAbs(-3.0L), 9.0L));
    assert(isClose(sqAbs(-5.0f), 25.0f));
}


/**
 * Extracts the norm of a dual number.
 * Params:
 *      d = A dual number
 * Returns:
 *      The squared magnitude of `d`.
 */
T norm(T)(Dual!T d) @safe pure nothrow @nogc
{
    return d.re * d.re + d.du * d.du;
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;
    assert(norm(dual(3.0, 4.0)) == 25.0);
    // assert(norm(fromPolar(5.0, 0.0)) == 25.0);
    // assert(isClose(norm(fromPolar(5.0L, PI / 6)), 25.0L));
    // assert(isClose(norm(fromPolar(5.0L, 13 * PI / 6)), 25.0L));
}


/**
  Params: d = A dual number.
  Returns: The dual conjugate of `d`.
*/
Dual!T conj(T)(Dual!T d) @safe pure nothrow @nogc
{
    return Dual!T(d.re, -d.du);
}

///
@safe pure nothrow unittest
{
    assert(conj(dual(1.0)) == dual(1.0));
    assert(conj(dual(1.0, 2.0)) == dual(1.0, -2.0));
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    static foreach (T; AliasSeq!(float, double, real))
    {{
         auto c = Dual!T(7, 3L);
         assert(conj(c) == Dual!T(7, -3L));
         auto d = Dual!T(0, -3.2L);
         assert(conj(d) == -d);
    }}
}

version (unittest)
{
    // Helper function for comparing two Dual numbers.
    int ceqrel(T)(const Dual!T x, const Dual!T y) @safe pure nothrow @nogc
    {
        import std.math.operations : feqrel;
        const r = feqrel(x.re, y.re);
        const i = feqrel(x.du, y.du);
        return r < i ? r : i;
    }
}

/**
    Trigonometric functions on dual numbers.

    Params: d = A dual number.
    Returns: The sine, cosine and tangent of `d`, respectively.
*/
Dual!T sin(T)(Dual!T d)  @safe pure nothrow @nogc
{
    auto cs = expi(d.re);
    auto csh = coshisinh(d.du);
    return typeof(return)(cs.du * csh.re, cs.re * csh.du);
}

///
@safe pure nothrow unittest
{
    static import core.math;
    assert(sin(dual(0.0)) == 0.0);
    assert(sin(dual(2.0, 0)) == core.math.sin(2.0));
}

@safe pure nothrow unittest
{
    static import core.math;
    assert(ceqrel(sin(dual(2.0L, 0)), dual(core.math.sin(2.0L))) >= real.mant_dig - 1);
}

/// ditto
Dual!T cos(T)(Dual!T d)  @safe pure nothrow @nogc
{
    auto cs = expi(d.re);
    auto csh = coshisinh(d.du);
    return typeof(return)(cs.re * csh.re, - cs.du * csh.du);
}

///
@safe pure nothrow unittest
{
    static import core.math;
    static import std.math;
    assert(cos(dual(0.0)) == 1.0);
    assert(cos(dual(1.3, 0.0)) == core.math.cos(1.3));
    assert(cos(dual(0.0, 5.2)) == std.math.cosh(5.2));
}

@safe pure nothrow unittest
{
    static import core.math;
    static import std.math;
    assert(ceqrel(cos(dual(0, 5.2L)), dual(std.math.cosh(5.2L), 0.0L)) >= real.mant_dig - 1);
    assert(ceqrel(cos(dual(1.3L)), dual(core.math.cos(1.3L))) >= real.mant_dig - 1);
}

/// ditto
Dual!T tan(T)(Dual!T d) @safe pure nothrow @nogc
{
    return sin(d) / cos(d);
}

///
@safe pure nothrow @nogc unittest
{
    static import std.math;

    int ceqrel(T)(const Dual!T x, const Dual!T y) @safe pure nothrow @nogc
    {
        import std.math.operations : feqrel;
        const r = feqrel(x.re, y.re);
        const i = feqrel(x.du, y.du);
        return r < i ? r : i;
    }
    assert(ceqrel(tan(dual(1.0, 0.0)), dual(std.math.tan(1.0), 0.0)) >= double.mant_dig - 2);
    assert(ceqrel(tan(dual(0.0, 1.0)), dual(0.0, std.math.tanh(1.0))) >= double.mant_dig - 2);
}

/**
    Inverse trigonometric functions on dual numbers.

    Params: d = A dual number.
    Returns: The arcsine, arccosine and arctangent of `d`, respectively.
*/
Dual!T asin(T)(Dual!T d)  @safe pure nothrow @nogc
{
    auto ash = asinh(Dual!T(-d.du, d.re));
    return Dual!T(ash.du, -ash.re);
}

///
@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;
    assert(asin(dual(0.0)) == 0.0);
    assert(isClose(asin(dual(0.5L)).re, PI / 6));
}

@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;
    version (DigitalMars) {} else // Disabled because of https://issues.dlang.org/show_bug.cgi?id=21376
    assert(isClose(asin(dual(0.5f)).re, float(PI) / 6));
}

/// ditto
Dual!T acos(T)(Dual!T d)  @safe pure nothrow @nogc
{
    static import std.math;
    auto as = asin(d);
    return Dual!T(T(std.math.PI_2) - as.re, as.du);
}

///
@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;
    import std.math.trigonometry : std_math_acos = acos;
    assert(acos(dual(0.0)) == std_math_acos(0.0));
    assert(isClose(acos(dual(0.5L)).re, PI / 3));
}

@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;
    version (DigitalMars) {} else // Disabled because of https://issues.dlang.org/show_bug.cgi?id=21376
    assert(isClose(acos(dual(0.5f)).re, float(PI) / 3));
}

/// ditto
Dual!T atan(T)(Dual!T d) @safe pure nothrow @nogc
{
    static import std.math;
    const T re2 = d.re * d.re;
    const T x = 1 - re2 - d.du * d.du;

    T num = d.du + 1;
    T den = d.du - 1;

    num = re2 + num * num;
    den = re2 + den * den;

    return Dual!T(T(0.5) * std.math.atan2(2 * d.re, x),
                     T(0.25) * std.math.log(num / den));
}

///
@safe pure nothrow @nogc unittest
{
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
Dual!T sinh(T)(Dual!T d)  @safe pure nothrow @nogc
{
    static import core.math, std.math;
    return Dual!T(std.math.sinh(d.re) * core.math.cos(d.du),
                     std.math.cosh(d.re) * core.math.sin(d.du));
}

///
@safe pure nothrow unittest
{
    static import std.math;
    assert(sinh(dual(0.0)) == 0.0);
    assert(sinh(dual(1.0L)) == std.math.sinh(1.0L));
    assert(sinh(dual(1.0f)) == std.math.sinh(1.0f));
}

/// ditto
Dual!T cosh(T)(Dual!T d)  @safe pure nothrow @nogc
{
    static import core.math, std.math;
    return Dual!T(std.math.cosh(d.re) * core.math.cos(d.du),
                     std.math.sinh(d.re) * core.math.sin(d.du));
}

///
@safe pure nothrow unittest
{
    static import std.math;
    assert(cosh(dual(0.0)) == 1.0);
    assert(cosh(dual(1.0L)) == std.math.cosh(1.0L));
    assert(cosh(dual(1.0f)) == std.math.cosh(1.0f));
}

/// ditto
Dual!T tanh(T)(Dual!T d) @safe pure nothrow @nogc
{
    return sinh(d) / cosh(d);
}

///
@safe pure nothrow @nogc unittest
{
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
Dual!T asinh(T)(Dual!T d)  @safe pure nothrow @nogc
{
    auto t = Dual!T((d.re - d.du) * (d.re + d.du) + 1, 2 * d.re * d.du);
    return log(sqrt(t) + d);
}

///
@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_asinh = asinh;
    assert(asinh(dual(0.0)) == 0.0);
    assert(isClose(asinh(dual(1.0L)).re, std_math_asinh(1.0L)));
    assert(isClose(asinh(dual(1.0f)).re, std_math_asinh(1.0f)));
}

/// ditto
Dual!T acosh(T)(Dual!T d)  @safe pure nothrow @nogc
{
    return 2 * log(sqrt(T(0.5) * (d + 1)) + sqrt(T(0.5) * (d - 1)));
}

///
@safe pure nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.trigonometry : std_math_acosh = acosh;
    assert(acosh(dual(1.0)) == 0.0);
    assert(isClose(acosh(dual(3.0L)).re, std_math_acosh(3.0L)));
    assert(isClose(acosh(dual(3.0f)).re, std_math_acosh(3.0f)));
}

/// ditto
Dual!T atanh(T)(Dual!T d) @safe pure nothrow @nogc
{
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
@safe pure nothrow @nogc unittest
{
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
Dual!real expi(real y)  @trusted pure nothrow @nogc
{
    import core.math : cos, sin;
    return Dual!real(cos(y), sin(y));
}

///
@safe pure nothrow unittest
{
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
Dual!real coshisinh(real y) @safe pure nothrow @nogc
{
    static import core.math;
    static import std.math;
    if (core.math.fabs(y) <= 0.5)
        return Dual!real(std.math.cosh(y), std.math.sinh(y));
    else
    {
        auto d = std.math.exp(y);
        auto zi = 0.5 / d;
        d = 0.5 * d;
        return Dual!real(d + zi, d - zi);
    }
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.trigonometry : cosh, sinh;
    assert(coshisinh(3.0L) == dual(cosh(3.0L), sinh(3.0L)));
}

/**
    Params: d = A dual number.
    Returns: The square root of `d`.
*/
Dual!T sqrt(T)(Dual!T d)  @safe pure nothrow @nogc
{
    static import core.math;
    typeof(return) c;
    real x,y,w,r;

    if (d == 0)
    {
        c = typeof(return)(0, 0);
    }
    else
    {
        real z_re = d.re;
        real z_im = d.du;

        x = core.math.fabs(z_re);
        y = core.math.fabs(z_im);
        if (x >= y)
        {
            r = y / x;
            w = core.math.sqrt(x)
                * core.math.sqrt(0.5 * (1 + core.math.sqrt(1 + r * r)));
        }
        else
        {
            r = x / y;
            w = core.math.sqrt(y)
                * core.math.sqrt(0.5 * (r + core.math.sqrt(1 + r * r)));
        }

        if (z_re >= 0)
        {
            c = typeof(return)(w, z_im / (w + w));
        }
        else
        {
            if (z_im < 0)
                w = -w;
            c = typeof(return)(z_im / (w + w), w);
        }
    }
    return c;
}

///
@safe pure nothrow unittest
{
    static import core.math;
    assert(sqrt(dual(0.0)) == 0.0);
    assert(sqrt(dual(1.0L, 0)) == core.math.sqrt(1.0L));
    assert(sqrt(dual(-1.0L, 0)) == dual(0, 1.0L));
    assert(sqrt(dual(-8.0, -6.0)) == dual(1.0, -3.0));
}

@safe pure nothrow unittest
{
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
@safe unittest
{
    import std.format : format;

    auto x = dual(1.2, 3.4);
    assert(format("%.2f", x) == "1.20+3.40i");

    auto y = dual(1.2, -3.4);
    assert(format("%.2f", y) == "1.20-3.40i");
}

@safe unittest
{
    // Test wide string formatting
    import std.format.write : formattedWrite;
    wstring wformat(T)(string format, Dual!T c)
    {
        import std.array : appender;
        auto w = appender!wstring();
        auto n = formattedWrite(w, format, c);
        return w.data;
    }

    auto x = dual(1.2, 3.4);
    assert(wformat("%.2f", x) == "1.20+3.40i"w);
}

@safe unittest
{
    // Test ease of use (vanilla toString() should be supported)
    assert(dual(1.2, 3.4).toString() == "1.2+3.4i");
}

@safe pure nothrow @nogc unittest
{
    auto c = dual(3.0L, 4.0L);
    c = sqrt(c);
    assert(c.re == 2.0L);
    assert(c.du == 1.0L);
}

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
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    import std.math.constants : PI;

    assert(exp(dual(0.0, 0.0)) == dual(1.0, 0.0));

    auto a = dual(2.0, 1.0);
    assert(exp(conj(a)) == conj(exp(a)));

    auto b = exp(dual(0.0L, 1.0L) * PI);
    assert(isClose(b.re, -1.0L, 0.0, 1e-15));
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN, isInfinity;

    auto a = exp(dual(0.0, double.infinity));
    assert(a.re.isNaN && a.du.isNaN);
    auto b = exp(dual(0.0, double.infinity));
    assert(b.re.isNaN && b.du.isNaN);
    auto c = exp(dual(0.0, double.nan));
    assert(c.re.isNaN && c.du.isNaN);

    auto d = exp(dual(+double.infinity, 0.0));
    assert(d == dual(double.infinity, 0.0));
    auto e = exp(dual(-double.infinity, 0.0));
    assert(e == dual(0.0));
    auto f = exp(dual(-double.infinity, 1.0));
    assert(f == dual(0.0));
    auto g = exp(dual(+double.infinity, 1.0));
    assert(g == dual(double.infinity, double.infinity));
    auto h = exp(dual(-double.infinity, +double.infinity));
    assert(h == dual(0.0));
    auto i = exp(dual(+double.infinity, +double.infinity));
    assert(i.re.isInfinity && i.du.isNaN);
    auto j = exp(dual(-double.infinity, double.nan));
    assert(j == dual(0.0));
    auto k = exp(dual(+double.infinity, double.nan));
    assert(k.re.isInfinity && k.du.isNaN);

    auto l = exp(dual(double.nan, 0));
    assert(l.re.isNaN && l.du == 0.0);
    auto m = exp(dual(double.nan, 1));
    assert(m.re.isNaN && m.du.isNaN);
    auto n = exp(dual(double.nan, double.nan));
    assert(n.re.isNaN && n.du.isNaN);
}

@safe pure nothrow @nogc unittest
{
    import std.math.constants : PI;
    import std.math.operations : isClose;

    auto a = exp(dual(0.0, -PI));
    assert(isClose(a.re, -1.0, 0.0, 1e-15));

    auto b = exp(dual(0.0, -2.0 * PI / 3.0));
    assert(isClose(b.re, -0.5L));
    assert(isClose(b.du, -0.866025403784438646763L));

    auto c = exp(dual(0.0, PI / 3.0));
    assert(isClose(c.re, 0.5L));
    assert(isClose(c.du, 0.866025403784438646763L));

    auto d = exp(dual(0.0, 2.0 * PI / 3.0));
    assert(isClose(d.re, -0.5L));
    assert(isClose(d.du, 0.866025403784438646763L));

    auto e = exp(dual(0.0, PI));
    assert(isClose(e.re, -1.0, 0.0, 1e-15));
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
Dual!T log(T)(Dual!T x) @safe pure nothrow @nogc
{
    static import std.math;
    return Dual!T(std.math.log(x.re), x.du / x.re);
}

///
@safe pure nothrow @nogc unittest
{
    import core.math : sqrt;
    import std.math.constants : PI;
    import std.math.operations : isClose;

    auto a = dual(2.0, 1.0);
    assert(log(conj(a)) == conj(log(a)));

    auto b = 2.0 * log10(dual(0.0, 1.0));
    auto c = 4.0 * log10(dual(sqrt(2.0) / 2, sqrt(2.0) / 2));
    assert(isClose(b.re, c.re, 0.0, 1e-15));
    assert(isClose(b.du, c.du, 0.0, 1e-15));

    assert(log(dual(-1.0L, 0.0L)) == dual(0.0L, PI));
    assert(log(dual(-1.0L, -0.0L)) == dual(0.0L, -PI));
}

@safe pure nothrow @nogc unittest
{
    import std.math.traits : isNaN, isInfinity;
    import std.math.constants : PI, PI_2, PI_4;

    auto a = log(dual(-0.0L, 0.0L));
    assert(a == dual(-real.infinity, PI));
    auto b = log(dual(0.0L, 0.0L));
    assert(b == dual(-real.infinity, +0.0L));
    auto c = log(dual(1.0L, real.infinity));
    assert(c == dual(real.infinity, PI_2));
    auto d = log(dual(1.0L, real.nan));
    assert(d.re.isNaN && d.du.isNaN);

    auto e = log(dual(-real.infinity, 1.0L));
    assert(e == dual(real.infinity, PI));
    auto f = log(dual(real.infinity, 1.0L));
    assert(f == dual(real.infinity, 0.0L));
    auto g = log(dual(-real.infinity, real.infinity));
    assert(g == dual(real.infinity, 3.0 * PI_4));
    auto h = log(dual(real.infinity, real.infinity));
    assert(h == dual(real.infinity, PI_4));
    auto i = log(dual(real.infinity, real.nan));
    assert(i.re.isInfinity && i.du.isNaN);

    auto j = log(dual(real.nan, 1.0L));
    assert(j.re.isNaN && j.du.isNaN);
    auto k = log(dual(real.nan, real.infinity));
    assert(k.re.isInfinity && k.du.isNaN);
    auto l = log(dual(real.nan, real.nan));
    assert(l.re.isNaN && l.du.isNaN);
}

@safe pure nothrow @nogc unittest
{
    import std.math.constants : PI;
    import std.math.operations : isClose;

    // auto a = log(fromPolar(1.0, PI / 6.0));
    // assert(isClose(a, dual(0.0L, 0.523598775598298873077L), 0.0, 1e-15));

    // auto b = log(fromPolar(1.0, PI / 3.0));
    // assert(isClose(b, dual(0.0L, 1.04719755119659774615L), 0.0, 1e-15));

    // auto c = log(fromPolar(1.0, PI / 2.0));
    // assert(isClose(c, dual(0.0L, 1.57079632679489661923L), 0.0, 1e-15));

    // auto d = log(fromPolar(1.0, 2.0 * PI / 3.0));
    // assert(isClose(d, dual(0.0L, 2.09439510239319549230L), 0.0, 1e-15));

    // auto e = log(fromPolar(1.0, 5.0 * PI / 6.0));
    // assert(isClose(e, dual(0.0L, 2.61799387799149436538L), 0.0, 1e-15));

    auto f = log(dual(-1.0L, 0.0L));
    assert(isClose(f.re, 0.0L, 0.0, 1e-15));
    assert(isClose(f.du, 0.0L, 0.0, 1e-15));
}

/**
 * Calculate the base-10 logarithm of x.
 * Params:
 *      x = A dual number
 * Returns:
 *      The dual base 10 logarithm of `x`
 */
Dual!T log10(T)(Dual!T x) @safe pure nothrow @nogc
{
    import std.math.constants : LN10;

    return log(x) / Dual!T(LN10);
}

///
@safe pure nothrow @nogc unittest
{
    import core.math : sqrt;
    import std.math.constants : LN10, PI;
    import std.math.operations : isClose;

    auto a = dual(2.0, 1.0);
    assert(log10(a) == log(a) / log(dual(10.0)));

    auto b = log10(dual(0.0, 1.0)) * 2.0;
    auto c = log10(dual(sqrt(2.0) / 2, sqrt(2.0) / 2)) * 4.0;
    assert(isClose(b.re, c.re, 0.0, 1e-15));
    assert(isClose(b.du, c.du, 0.0, 1e-15));
}

@safe pure nothrow @nogc unittest
{
    import std.math.constants : LN10, PI;
    import std.math.operations : isClose;

    // auto a = log10(fromPolar(1.0, PI / 6.0));
    // assert(isClose(a, dual(0.0L, 0.227396058973640224580L), 0.0, 1e-15));

    // auto b = log10(fromPolar(1.0, PI / 3.0));
    // assert(isClose(b, dual(0.0L, 0.454792117947280449161L), 0.0, 1e-15));

    // auto c = log10(fromPolar(1.0, PI / 2.0));
    // assert(isClose(c, dual(0.0L, 0.682188176920920673742L), 0.0, 1e-15));

    // auto d = log10(fromPolar(1.0, 2.0 * PI / 3.0));
    // assert(isClose(d, dual(0.0L, 0.909584235894560898323L), 0.0, 1e-15));

    // auto e = log10(fromPolar(1.0, 5.0 * PI / 6.0));
    // assert(isClose(e, dual(0.0L, 1.13698029486820112290L), 0.0, 1e-15));

    auto f = log10(dual(-1.0L, 0.0L));
    assert(isClose(f.re, 0.0L, 0.0, 1e-15));
    assert(isClose(f.du, 1.36437635384184134748L, 0.0, 1e-15));

    assert(ceqrel(log10(dual(-100.0L, 0.0L)), dual(2.0L, PI / LN10)) >= real.mant_dig - 1);
    assert(ceqrel(log10(dual(-100.0L, -0.0L)), dual(2.0L, -PI / LN10)) >= real.mant_dig - 1);
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
Dual!T pow(T, Int)(Dual!T x, const Int n) @safe pure nothrow @nogc
if (isIntegral!Int)
{
    alias UInt = Unsigned!(Unqual!Int);

    UInt m = (n < 0) ? -cast(UInt) n : n;
    Dual!T y = (m % 2) ? x : Dual!T(1);

    while (m >>= 1)
    {
        x *= x;
        if (m % 2)
            y *= x;
    }

    return (n < 0) ? Dual!T(1) / y : y;
}

///
@safe pure nothrow @nogc unittest
{
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
Dual!T pow(T)(Dual!T x, const T n) @trusted pure nothrow @nogc
{
    static import std.math;

    if (x == 0.0)
        return Dual!T(0.0);

    if (x.du == 0 && x.re > 0.0)
        return Dual!T(std.math.pow(x.re, n));

    Dual!T t = log(x);
    return fromPolar!(T, T)(std.math.exp(n * t.re), n * t.du);
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    assert(pow(dual(0.0), 2.0) == dual(0.0));
    assert(pow(dual(5.0), 2.0) == dual(25.0));

    auto a = pow(dual(-1.0, 0.0), 0.5);
    assert(isClose(a.re, 0.0, 0.0, 1e-16));
    assert(isClose(a.du, +1.0, 0.0, 1e-16));

    auto b = pow(dual(-1.0, -0.0), 0.5);
    assert(isClose(b.re, 0.0, 0.0, 1e-16));
    assert(isClose(b.du, -1.0, 0.0, 1e-16));
}

/// ditto
Dual!T pow(T)(Dual!T x, Dual!T y) @trusted pure nothrow @nogc
{
    return (x == 0) ? Dual!T(0) : exp(y * log(x));
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    import std.math.exponential : exp;
    import std.math.constants : PI;
    auto a = dual(0.0);
    auto b = dual(2.0);
    assert(pow(a, b) == dual(0.0));

    auto c = dual(0.0L, 1.0L);
    assert(isClose(pow(c, c).re, exp((-PI) / 2)));
}

/// ditto
Dual!T pow(T)(const T x, Dual!T n) @trusted pure nothrow @nogc
{
    static import std.math;

    return (x > 0.0)
        ? fromPolar!(T, T)(std.math.pow(x, n.re), n.du * std.math.log(x))
        : pow(Dual!T(x), n);
}

///
@safe pure nothrow @nogc unittest
{
    import std.math.operations : isClose;
    assert(pow(2.0, dual(0.0)) == dual(1.0));
    assert(pow(2.0, dual(5.0)) == dual(32.0));

    auto a = pow(-2.0, dual(-1.0));
    assert(isClose(a.re, -0.5, 0.0, 1e-16));

    auto b = pow(-0.5, dual(-1.0));
    assert(isClose(b.re, -2.0, 0.0, 1e-15));
}

@safe pure nothrow @nogc unittest
{
    import std.math.constants : PI;
    import std.math.operations : isClose;

    auto a = pow(dual(3.0, 4.0), 2);
    assert(isClose(a.re, -7.0));
    assert(isClose(a.du, 24.0));

    auto b = pow(dual(3.0, 4.0), PI);
    assert(ceqrel(b, dual(-152.91512205297134, 35.547499631917738)) >= double.mant_dig - 3);

    auto c = pow(dual(3.0, 4.0), dual(-2.0, 1.0));
    assert(ceqrel(c, dual(0.015351734187477306, -0.0038407695456661503)) >= double.mant_dig - 3);

    auto d = pow(PI, dual(2.0, -1.0));
    assert(ceqrel(d, dual(4.0790296880118296, -8.9872469554541869)) >= double.mant_dig - 1);

    auto e = dual(2.0);
    assert(ceqrel(pow(e, 3), exp(3 * log(e))) >= double.mant_dig - 1);
}

@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    import std.math : RealFormat, floatTraits;
    static foreach (T; AliasSeq!(float, double, real))
    {{
         static if (floatTraits!T.realFormat == RealFormat.ibmExtended)
         {
             /* For IBM real, epsilon is too small (since 1.0 plus any double is
                representable) to be able to expect results within epsilon * 100.  */
         }
         else
         {
             T eps = T.epsilon * 100;

             T a = -1.0;
             T b = 0.5;
             Dual!T ref1 = pow(dual(a), dual(b));
             Dual!T res1 = pow(a, dual(b));
             Dual!T res2 = pow(dual(a), b);
             assert(abs(ref1 - res1) < eps);
             assert(abs(ref1 - res2) < eps);
             assert(abs(res1 - res2) < eps);

             T c = -3.2;
             T d = 1.4;
             Dual!T ref2 = pow(dual(a), dual(b));
             Dual!T res3 = pow(a, dual(b));
             Dual!T res4 = pow(dual(a), b);
             assert(abs(ref2 - res3) < eps);
             assert(abs(ref2 - res4) < eps);
             assert(abs(res3 - res4) < eps);
         }
    }}
}
