package arx.core

import datastructures.IntRange
import rich._
import units._
import annotation.tailrec
import vec.Vec4f
import arx.core.functions.MemoizingFunction

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 3:01 PM
 * Created by nonvirtualthunk
 */

object ArxImplicits {
	val ea = this.getClass.desiredAssertionStatus()

	def posit(condition: => Boolean, message: => String) {
		if (ea) {
			if (!condition) {
				throw new AssertionError(message)
			}
		}
	}

	object Mod {
		def apply[T] ( modded: Moddable[T], func: T => T ): Moddable[T] = { new ModdedInOutFunction[T](modded,func) }
		def apply[T,U] ( func : (U) => T , u : Moddable[U] ) : Moddable[T] = { new ModdedSimpleClosure[T,U](func,u) }
	}


	implicit def toUOMFloat (f : Float) : UnitOfMeasureFloat = new UnitOfMeasureFloat(f)
	implicit def toUOMFloat (d : Double) : UnitOfMeasureFloat = new UnitOfMeasureFloat(d.toFloat)
	implicit def toUOMFloat (i : Int) : UnitOfMeasureFloat = new UnitOfMeasureFloat(i)
	implicit def toUOMFloat (i : Short) : UnitOfMeasureFloat = new UnitOfMeasureFloat(i.toFloat)

	implicit def float2RicherFloat ( f : Float ) : RicherFloat = new RicherFloat(f)
	implicit def tuple2EitherFloat ( t : (Float,Float) ) : EitherFloat = new EitherFloat(t._1,t._2)
	implicit def int2RicherInt ( i : Int ) : RicherInt = new RicherInt(i)

	implicit def Moddable2T[T] ( moddable: Moddable[T] ): T = moddable.resolve()
	implicit def T2Moddable[T] ( t: T ) : Moddable[T] = Moddable(t)
	implicit def TFunc2Moddable[T] ( func: () => T ): Moddable[T] = Moddable(func)

	implicit def ratioToDensity ( r : RatioUnitOfMeasure[UnitOfMass,UnitOfVolume] )  = new UnitOfDensity(r.overValue,r.underValue)
	implicit def toUnitOfAcceleration( r : RatioUnitOfMeasure[UnitOfDistance,UnitOfTimeSquared] ) = new UnitOfAcceleration(r.overValue,r.underValue)
	implicit def rtoVelocity ( rum : RatioUnitOfMeasure[UnitOfDistance,UnitOfTime] ) = new UnitOfSpeed(rum.overValue,rum.underValue)

	implicit def tup2IntRange ( tup : (Int,Int) ) : IntRange = new IntRange(tup._1,tup._2)

	implicit def toArxString(str: String): ArxString = new ArxString(str)
	implicit def toString (str: ArxString) : String = str.intern

	implicit def toArxList[T] ( l : List[T] ) : ArxList[T] = { new ArxList(l) }
	implicit def toArxSet[T] ( l : Set[T] ) : ArxSet[T] = { new ArxSet(l) }

	def Color ( r : Int , g : Int , b : Int , a : Int ) = Vec4f( r.toFloat/255.0f , g.toFloat/255.0f , b.toFloat/255.0f, a.toFloat/255.0f )

	def cosf(theta:Float) = scala.math.cos(theta).toFloat
	def sinf(theta:Float) = scala.math.sin(theta).toFloat
	def tanf(theta:Float) = scala.math.tan(theta).toFloat
	def cosf(theta:Double) = scala.math.cos(theta).toFloat
	def sinf(theta:Double) = scala.math.sin(theta).toFloat
	def tanf(theta:Double) = scala.math.tan(theta).toFloat
	def atanf(theta:Float) = math.atan(theta).toFloat
	def atan2f(y:Float,x:Float) = math.atan2(y,x).toFloat
	def sqrtf(x:Float) = scala.math.sqrt(x).toFloat
	def lengthf(x:Float,y:Float) = sqrtf(x*x+y*y)
	def lengthi(x:Int,y:Int) = sqrtf(x*x+y*y)
	def absf( x : Float )  = scala.math.abs(x).toFloat
	def absi( x : Int ) : Int = scala.math.abs(x)
	def powf( x : Float , e : Float ) : Float = math.pow(x,e).toFloat
	def floorf ( x : Float ) = scala.math.floor(x).toFloat
	def ceilf ( x : Float ) = scala.math.ceil(x).toFloat

	@tailrec
	final def forEachPair[T,U] (l : List[T]) ( func : (T,T) => U ) {
		l match {
			case Nil =>
			case single :: Nil =>
			case one :: two :: Nil => func(one,two)
			case lots =>
				func(lots(0),lots(1))
				forEachPair(lots.tail)(func)
		}
	}

    def memoize[T,U] ( f : (T) => U ) = new MemoizingFunction.MemoizingFunction1(f)
    def memoize[S,T,U] ( f : (S,T) => U ) = new MemoizingFunction.MemoizingFunction2(f)
    def memoize[R,S,T,U] ( f : (R,S,T) => U ) = new MemoizingFunction.MemoizingFunction3(f)
    def memoize[Q,R,S,T,U] ( f : (Q,R,S,T) => U ) = new MemoizingFunction.MemoizingFunction4(f)
    def memoize[P,Q,R,S,T,U] ( f : (P,Q,R,S,T) => U ) = new MemoizingFunction.MemoizingFunction5(f)
    def memoize[O,P,Q,R,S,T,U] ( f : (O,P,Q,R,S,T) => U ) = new MemoizingFunction.MemoizingFunction6(f)

	val meter3 = 1.meter3
	val m3 = meter3
	val cm3 = new UnitOfVolume(Centimeter,1.0f)
	val kg = 1.kg
	val meter = 1.meter
	val centimeter = 1.centimeter
	val meter2 = 1.meters2
	val cubicMeter = meter3
	val squareMeter = meter2
	val kg_m3 = kg per meter3
	val meters = 1.meter
	val second = 1.second
	val second2 = 1.second2
	val year = 1.year
	val oneMeterCube = 1.meter x 1.meter x 1.meter
	val zeroMeterCube = 0.meter x 0.meter x 0.meter
	val zeroSeconds = 0.seconds
	val zeroMetersPerSecond = 0.m_s
	val zeroMetersPerSecondSquared = 0.m_s2
	val negativeMetersPerSecondSquared = (-1).m_s2
	val negaDimension = (-1).meter x (-1).meter x (-1).meter
	val foreverTime = 10000000.years

	implicit val zeroMeters : UnitOfDistance = 0.meters
	implicit val zeroMeters2 : UnitOfArea = 0.meters2
	implicit val zeroMeters3 : UnitOfVolume = 0.meters3
	implicit val zeroKg : UnitOfMass = 0.kg
}