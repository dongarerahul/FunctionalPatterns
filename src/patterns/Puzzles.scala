package patterns

object Puzzles {
  def eval[X, Y]: (X => Y) => X => Y = xy => x => xy(x)

  /**
   * In this context it means: the argument of the function you're trying to define
   * so " _(xh) " is a function of type ((X => H) => H) => ( X => H) => H

  It takes an (X => (X => H)), and applies it to an X => H to get an H !
  **/
  def mu[X, H]: ((((X => H) => H) => H) => H) => ((X => H) => H) = xhhhh => xh => xhhhh(a => a(xh))
  //let mu =                                                       \ xhhhh ->\ xh-> xhhhh (\ a -> a xh)

  def strength[X, Y, H]: (X => Y) => ((X => H) => H) => (Y => H) => H =
    xy => xhh => yh => xhh(x => yh(xy(x)))



  def em[X, H, A]:  (((X => H) => H) => X )    =>     (((A => X) => H) => H) => (A => X)     =
    xhhx => axhh => a => xhhx(xh => axhh(ax => xh(ax(a))))

  def as[H, A, B]: (((A => B) => H) => H) => ((A => H) => H) => (B => H) => H =
    abhh => ahh => bh => abhh(ab => ahh(a => bh(ab(a))))


  /*
  Well, you have to work backwards.
    We're given an XHHX and have to produce an AXHHAX.
    Equivalently, we're given XHHX, AXHH and have to produce AX.
    Equivalently, we're given XHHX, AXHH, A and have to produce X.

    Now, the only way to produce X from what we have is to find an XHH and feed it into our XHHX = XHH => X.
    So we have to find an XHH = XH => H. So given an XH together with all our other stuff, we have to produce an H.

    So the problem reduces to: given XHHX, AXHH, A, XH, find an H.
    We now clearly have to find an AXH = (A => X) => H to feed into the AXHH.
    And that reduces to: given an A => X, produce an H.
  */

  def rm[X, S]:  (S => S => X) => S => X = ssx => s => ssx(s)(s)

  /** *
    * a = len
    * xh = "World "
    * xhhhh = "Hello"
  */

  def main(args: Array[String]) {
    val f = eval[String, Int]
    def len(s: String) = s.length

    println(f(len)("Hello"))

    val g = rm[String, Int]
    def fun(x: String)(y: String) : Int = x.length + y.length

    val result = rm(fun)("Bye")

    //mu("Hello")("World")()
    println(result)
  }
}
