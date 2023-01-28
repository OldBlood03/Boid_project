package s2.expressions

/** Playground for testing with expressions. */


object ExpressionTest:
  import Expressions.*

  // Exercise 1 in chapter 17.3.

  // Implement the expressions a, b, c and d.

  // Variables used in this exercise:
  def x = Variable("x")
  def y = Variable("y")
  def z = Variable("z")

  def Pow(x:Exp, p:Int):Exp =
    if p <= 1 then Mul(x,Const(1))
    else Mul(x,Pow(x,p-1))

  // a) 2x

  def func1 = Mul(Const(2), x)

  // b) (2x)^3   eli  2x * 2x * 2x
  // Use the previous result here.
  def func2 = Pow(func1,3)

  // c) 3 x y + x (x + 7 z)

  def func3 = Add(Mul(Const(3),Mul(x,y)),Mul(x,Add(x,Mul(Const(7),z))))

  // d) x^2 + 2 x y + y^2

  def func4 = Add(Add(Mul(Const(2),Mul(x,y)),Pow(x,2)),Pow(y,2))

end ExpressionTest

/** Implementations for expression handling.
  */
object Expressions:

  // Exercise 2 in chapter 17.3.

  def prettyprint(e: Exp): String =
    e match
      case x: Const            => x.c.toString()
      case v: Variable         => v.name
      case Add(e: Exp, f: Exp) => "( " + prettyprint(e) + " + " + prettyprint(f) + " )"
      case Mul (Variable(x),Variable(y)) if x==y => s"${x}^2"
      case Mul (Mul(Variable(x),Variable(y)),Variable(a)) if x==y && x==a => s"${x}^3"
      case Mul (Variable(a),Mul(Variable(x),Variable(y))) if x==y && x==a => s"${x}^3"
      case Mul(e: Exp, f: Exp) => "( " + prettyprint(e) + " * " + prettyprint(f) + " )"

  // Exercise 3 in chapter 17.3.

  def containsVar (e:Exp,name:String) = {
    prettyprint(e).contains(name)
  }

  def bind(e: Exp, v: Variable, a: Double): Exp =
    e match
      case c: Const                  => c
      case Variable (b) if b == v.name => Const(a)
      case e @ Variable(a) => e
      case add @ Add(e: Exp, f: Exp) =>
        if containsVar(add, v.name) then
          Add(bind(e,v,a),bind(f,v,a))
        else add
      case mul @ Mul(e: Exp, f: Exp) =>
        if containsVar(mul, v.name) then
          Mul(bind(e,v,a),bind(f,v,a))
        else mul

  // Exercise 4 in chapter 17.3.

  def derivate(e: Exp, d: Variable): Exp =
    e match
      case Const(_)            => Const(0)
      case Variable (a) if a==d.name => Const(1)
      case Variable(_) => Const(0)
      case Add(e: Exp, f: Exp) => Add(derivate(e,d),derivate(f,d))
      case Mul(e: Exp, f: Exp) => Add(Mul(derivate(e,d),f),Mul(derivate(f,d),e))

end Expressions
