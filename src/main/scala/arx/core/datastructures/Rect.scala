package arx.core.datastructures

import scala.math._
import java.io.{ObjectOutput, ObjectInput, Externalizable}

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/30/11
 * Time: 2:45 PM
 * Created by nonvirtualthunk
 */


class Rect[@specialized(Int) T](a: T,b: T,c: T,d: T) {
	var x: T = a
	var y: T = b
	var width: T = c
	var height: T = d

	def minX = x
	def minY = y

	def w = width
	def h = height

	override def toString : String = {
		"Rect( x: " + x + ", y: " + y + ", w: " + w + ", h: " + h + ")"
	}

    override def equals ( other : Any ) = {
        other match {
            case rect : Rect[_] => rect.x == x && rect.y == y && rect.width == width && rect.height == height
            case _ => false
        }
    }
}

@SerialVersionUID(1L)
class Recti(a:Int,b:Int,c:Int,d:Int) extends Rect[Int](a,b,c,d) with Externalizable {
	def this() { this(0,0,0,0) }
	def maxX = x + w
	def maxY = y + h

	def writeExternal(out: ObjectOutput) {
		out.writeInt(x)
		out.writeInt(y)
		out.writeInt(w)
		out.writeInt(h)
	}
	def readExternal(in: ObjectInput) {
		x = in.readInt
		y = in.readInt
		width = in.readInt
		height = in.readInt
	}
}

object Rect{
	def fromMinMax ( xMin : Int , yMin : Int , xMax : Int , yMax : Int ) = new Recti( xMin, yMin , xMax - xMin , yMax - yMin )
	def apply ( x : Int , y : Int , w : Int , h : Int ) = {
		new Recti(x,y,w,h)
	}

	def apply[T] (r: Rect[T]): Rect[T] = {
		new Rect[T](r.x,r.y,r.w,r.h);
	}

	def intersect (r1: Rect[Float], r2: Rect[Float]): Rect[Float] = {
		val lx = max(r1.x,r2.x)
		val ly = max(r1.y,r2.y)
		val hx = min(r1.x + r1.w,r2.x + r2.w)
		val hy = min(r1.y + r1.h,r2.y + r2.h)
		new Rect[Float](lx,ly,hx-lx,hy-ly)
	}
}