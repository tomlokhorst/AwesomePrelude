var mul = /* mul */ function (v1) { return function (v2) { return v1 * v2; }; }
var fix = /* fix */ function (v1) { return fix = arguments.callee, v1(function (i) { return fix(v1)(i) }); }
var list = /* list */ function (v3) { return function (v4) { return function (v5) { return v5.nil ? v3 : v4(v5.head)(v5.tail); }; }; }
var add = /* add */ function (v5) { return function (v6) { return v5 + v6; }; }
var fix = /* fix */ function (v1) { return fix = arguments.callee, v1(function (i) { return fix(v1)(i) }); }
var list = /* list */ function (v3) { return function (v4) { return function (v5) { return v5.nil ? v3 : v4(v5.head)(v5.tail); }; }; }
var fix = /* fix */ function (v3) { return fix = arguments.callee, v3(function (i) { return fix(v3)(i) }); }
var bool = /* bool */ function (v5) { return function (v6) { return function (v7) { return v7 ? v5(/*force*/) : v6(/*force*/); }; }; }
var cons = /* cons */ function (v6) { return function (v7) { return { head : v6, tail : v7 }; }; }
var sub = /* sub */ function (v6) { return function (v7) { return v6 - v7; }; }
var eq = /* eq */ function (v5) { return function (v6) { return v5 == v6; }; }
var cons = /* cons */ function (v5) { return function (v6) { return { head : v5, tail : v6 }; }; }
var fix = /* fix */ function (v1) { return fix = arguments.callee, v1(function (i) { return fix(v1)(i) }); }
var bool = /* bool */ function (v3) { return function (v4) { return function (v5) { return v5 ? v3(/*force*/) : v4(/*force*/); }; }; }
var cons = /* cons */ function (v4) { return function (v5) { return { head : v4, tail : v5 }; }; }
var sub = /* sub */ function (v4) { return function (v5) { return v4 - v5; }; }
var eq = /* eq */ function (v3) { return function (v4) { return v3 == v4; }; }
var maybe = /* maybe */ function (v1) { return function (v2) { return function (v3) { return v3.nothing ? v1 : v2(v3.just); }; }; }
var mul = /* mul */ function (v2) { return function (v3) { return v2 * v3; }; }
var just = /* just */ function (v1) { return { just : v1 }; }
var sub = /* sub */ function (v1) { return function (v2) { return v1 - v2; }; }
var c1 = list(0)
var c2 = fix(function (v1) { return function (v2) { return c1(function (v3) { return function (v4) { return add(v3)(v1(v4)); }; })(v2); }; })
var c3 = function (v6) { return function (v7) { return v6; }; }({ nil : 1 })
var c4 = bool(function (v5) { return c3(v5); })
var c5 = cons(8)
var c6 = fix(function (v3) { return function (v4) { return c4(function (v5) { return function (v6) { return function (v7) { return v6; }; }(c5(v3(sub(v4)(1))))(v5); })(eq(v4)(0)); }; })
var c7 = c6(3)
var c8 = list(c7)
var c9 = fix(function (v1) { return function (v2) { return c8(function (v3) { return function (v4) { return cons(v3)(v1(v4)); }; })(v2); }; })
var c10 = function (v4) { return function (v5) { return v4; }; }({ nil : 1 })
var c11 = bool(function (v3) { return c10(v3); })
var c12 = cons(8)
var c13 = fix(function (v1) { return function (v2) { return c11(function (v3) { return function (v4) { return function (v5) { return v4; }; }(c12(v1(sub(v2)(1))))(v3); })(eq(v2)(0)); }; })
var c14 = c13(3)
var c15 = c9(c14)
var c16 = c2(c15)
var c17 = mul(c16)
var c18 = maybe(4)
var c19 = c18(function (v1) { return mul(v1)(8); })
var c20 = sub(3)
var c21 = c20(2)
var c22 = just(c21)
var c23 = c19(c22)
var c24 = c17(c23)
var __main = c24