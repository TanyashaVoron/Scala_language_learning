package exercises01

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(other.x+x,other.y+y)

  def -(other: Vector): Vector = new Vector(x-other.x,y-other.y)

  def *(scalar: Double): Vector = new Vector(x*scalar,y*scalar)

  def unary_- : Vector = *(-1)

  def euclideanLength: Double = Math.sqrt(x*x+y*y)

  def normalized: Vector = if (x == 0 && y == 0) new Vector(0,0)
  else new Vector(x/euclideanLength,y/euclideanLength)

  override def equals(other: Any): Boolean = other.equals(x,y)

  // Vector(x, y)
  override def toString: String = new String("Vector("+x+", "+y+")")
}

object Vector {
  def fromAngle(angle: Double, length: Double): Vector = new Vector(length*Math.cos(angle), length*Math.sin(angle))

  def sum(list: List[Vector]): Vector = list.foldRight(new Vector(0,0))((a,b)=>a+b)

  def unapply(arg: Vector): Option[(Double, Double)] = Option(arg.x,arg.y)
}
