package arx.core.units

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 3:09 PM
 * Created by nonvirtualthunk
 */

import java.io.{ObjectOutput, ObjectInput}
import java.text.DecimalFormat
import arx.core.ArxImplicits._
import arx.core.vec.{Vec2f, Vec3f}
import arx.core.ArxImplicits._

abstract class UnitOfMeasure[T <: UnitOfMeasure[T]] ( var unit : MeasurementUnit , var value : Float ) extends Serializable {
//		def this() { this(null,0.0f) }
	def + ( u : T ) = if ( u.unit eq unit ) { create(unit,value + u.value) } else { create(baseUnitOfMeasure,this.toBaseUnitOfMeasure + u.toBaseUnitOfMeasure) }
	def - ( u : T ) = if ( u.unit eq unit ) { create(unit,value - u.value) } else { create(baseUnitOfMeasure,this.toBaseUnitOfMeasure - u.toBaseUnitOfMeasure) }
	def * ( scalar : Float ) = create(unit,value * scalar)
	def / ( scalar : Float ) = create(unit,value / scalar)
	def /[U <: UnitOfMeasure[U]] ( o : U ) : RatioUnitOfMeasure[T,U] = new RatioUnitOfMeasure[T,U](create(unit,value),o)
	def per[U <: UnitOfMeasure[U]] ( o : U ) : RatioUnitOfMeasure[T,U] = new RatioUnitOfMeasure[T,U](create(unit,value),o)
	def div[O <: UnitOfMeasure[O]] ( r : RatioUnitOfMeasure[T,O] ) = {
		val overBase = r.overValue.toBaseUnitOfMeasure
		r.underValue.create(r.underValue.baseUnitOfMeasure,r.underValue.toBaseUnitOfMeasure * (toBaseUnitOfMeasure / (if(overBase==0.0f){0.000001f}else{overBase})))
	}
	def < ( u : T ) = if ( u.unit eq this.unit ) { this.value < u.value } else { this.toBaseUnitOfMeasure < u.toBaseUnitOfMeasure }
	def == ( u : T ) = if ( ! (u eq null) ) { comparate(u,(a,b) => scala.math.abs(a - b) < 0.0001f) } else { false }
	def != ( u : T ) = !(this == u)
	def <= ( u : T ) = if ( u.unit eq this.unit ) { this.value <= u.value } else { this.toBaseUnitOfMeasure <= u.toBaseUnitOfMeasure }
	def > ( u : T ) = if ( u.unit eq this.unit ) { this.value > u.value } else { this.toBaseUnitOfMeasure > u.toBaseUnitOfMeasure }
	def >= ( u : T ) = if ( u.unit eq this.unit ) { this.value >= u.value } else { this.toBaseUnitOfMeasure >= u.toBaseUnitOfMeasure }

	override def equals ( other : Any ) : Boolean = {
		other match {
			case u : UnitOfMeasure[T] => u.order == this.order && u.baseUnitOfMeasure == this.baseUnitOfMeasure && scala.math.abs(u.toBaseUnitOfMeasure - this.toBaseUnitOfMeasure) < 0.0001f
			case _ => false
		}
	}
	override def toString = value + unit.suffix + (if ( order > 1 ) { "^" + order } else { "" })
	def displayString = toLongString
	def toLongString = {
		val orderString = if (order <= 1) { "" } else if ( order == 2 ) { " squared" } else if ( order == 3 ) { " cubed" } else { " fourthed?" }
		if ( value =~= value.round ) {
			value.toInt + " " + unit.name + "s" + orderString
		} else {
			UnitOfMeasure.decimalFormatter.format(value) + " " + unit.name + "s" + orderString
		}
	}
	def shortDisplayString = {
		val orderString = if (order <= 1) { "" } else { order.toString }
		if ( value =~= value.round ) {
			if ( value.round == 0 ) {
				unit.lowerOrderOfMagnitude match {
					case Some(lower) => {
						val inLower = this.toBaseUnitOfMeasure / math.pow(lower.conversion,order).toFloat
						if ( inLower =~= 0.0f ) {
							value.toInt + " " + unit.suffix + orderString
						} else {
							UnitOfMeasure.decimalFormatter.format(inLower) + " " + lower.suffix + orderString
						}
					}
					case None => value.toInt + " " + unit.suffix + orderString
				}
			} else { value.toInt + " " + unit.suffix + orderString }
		} else {
			val valueString = UnitOfMeasure.decimalFormatter.format(value)
			val baseOutput = valueString + " " + unit.suffix + orderString
			if ( valueString == "0" ) {
				unit.lowerOrderOfMagnitude match {
					case Some(lower) => {
						val inLower = this.toBaseUnitOfMeasure / math.pow(lower.conversion,order).toFloat
						if ( inLower =~= 0.0f ) {
							baseOutput
						} else {
							UnitOfMeasure.decimalFormatter.format(inLower) + " " + lower.suffix + orderString
						}
					}
					case None => baseOutput
				}
			} else {
				baseOutput
			}
		}
	}
	def toFloat = value

	def comparate ( u:T,f : (Float,Float) => Boolean ) : Boolean = { f(this.toBaseUnitOfMeasure,u.toBaseUnitOfMeasure) }
	def operate ( u : T , f : (Float,Float) => Float ) : T = {
		if ( u.unit == unit ) { create(unit,f(value,u.value)) }
		else { create(baseUnitOfMeasure,f(toBaseUnitOfMeasure,u.toBaseUnitOfMeasure)) }
	}
	def baseUnitOfMeasure : MeasurementUnit
	def toBaseUnitOfMeasure : Float
	def create ( unit : MeasurementUnit , value : Float ) : T
	def order : Int

	def writeExternal(p1: ObjectOutput) {
		p1.writeFloat(value)
		p1.writeObject(unit)
	}
	def readExternal(p1: ObjectInput) {
		value = p1.readFloat()
		unit = p1.readObject().asInstanceOf[MeasurementUnit]
	}
}
object UnitOfMeasure {
	val decimalFormatter = new DecimalFormat("#.##")
}


class UnitOfSpeed(d : UnitOfDistance,t : UnitOfTime) extends RatioUnitOfMeasure[UnitOfDistance,UnitOfTime](d,t) {
	def this() { this(0.0.meters,1.0.seconds) }
	def inMetersPerSecond = toBaseUnitOfMeasure

	def / ( t2 : UnitOfTime ) : UnitOfAcceleration = { new UnitOfAcceleration(d,t * t2) }

	def getOrElse ( s : UnitOfSpeed ) : UnitOfSpeed = this
	def nonEmpty = true

	override def create(o: UnitOfDistance, u: UnitOfTime) : this.type = new UnitOfSpeed(o,u).asInstanceOf[this.type]

	def resolve() = this
	def baseValue() = this
}
object NoSpeed extends UnitOfSpeed(0.0f.meters,1.0.seconds) {
	override def getOrElse ( s : UnitOfSpeed ) = { s }
	override def nonEmpty = false
	override def toString = "NoSpeed"
}

class UnitOfDensity(m : UnitOfMass,v : UnitOfVolume) extends RatioUnitOfMeasure[UnitOfMass,UnitOfVolume](m,v) {
	def this() { this(0.0.kg,1.0.meter3) }
	def inKgPerMeter3 = toBaseUnitOfMeasure

	override def create(m : UnitOfMass,u : UnitOfVolume) : this.type = new UnitOfDensity(m,u).asInstanceOf[this.type]

	def resolve() = this
	def baseValue() = this
}

class UnitOfDistance(unit : MeasurementUnit,value: Float) extends UnitOfMeasure[UnitOfDistance](unit,value) {
	def this() { this(Meter,0.0f) }
	def inMeters : Float = value * unit.conversion
	def inCentimeters : Float = (value * unit.conversion) / Centimeter.conversion
	def * ( u : UnitOfDistance ) : UnitOfArea = {
		if ( unit == u.unit ) { new UnitOfArea(unit,value * u.value) }
		else { new UnitOfArea(Meter,inMeters * u.inMeters) }
	}
	def x ( u : UnitOfDistance ) : Dimensions2 = new Dimensions2(this,u)
	def copyWithValue( v : Float) = new UnitOfDistance(unit,v)

	def baseUnitOfMeasure = Meter
	def toBaseUnitOfMeasure = inMeters
	def create(u: MeasurementUnit, v: Float) = new UnitOfDistance(u,v)
	def order : Int = 1

	def resolve() = this
	def baseValue() = this
}
class UnitOfArea(unit : MeasurementUnit,value: Float) extends UnitOfMeasure[UnitOfArea](unit,value) {
	def this() { this(Meter,0.0f) }
	def inSquareMeters : Float = value * unit.conversion * unit.conversion
	def * ( u : UnitOfDistance ) : UnitOfVolume = {
		if ( unit == u.unit ) { new UnitOfVolume(unit,value * u.value) }
		else { new UnitOfVolume(Meter,inSquareMeters * u.inMeters) }
	}
	def copyWithValue( v : Float) = new UnitOfArea(unit,v)

	def baseUnitOfMeasure = Meter
	def toBaseUnitOfMeasure = inSquareMeters
	def create(u: MeasurementUnit, v: Float) = new UnitOfArea(u,v)
	def order : Int = 2

	def resolve() = this
	def baseValue() = this
}
class UnitOfVolume(unit : MeasurementUnit,value: Float) extends UnitOfMeasure[UnitOfVolume](unit,value) {
	def this() { this(Meter,0.0f) }
	def inCubicMeters : Float = value * unit.conversion * unit.conversion * unit.conversion
	def copyWithValue( v : Float) = new UnitOfVolume(unit,v)

	def baseUnitOfMeasure = Meter
	def toBaseUnitOfMeasure = inCubicMeters
	def create(u: MeasurementUnit, v: Float) = new UnitOfVolume(u,v)
	def order : Int = 3

	def resolve() = this
	def baseValue() = this
}
class UnitOfMass(unit : MeasurementUnit,value: Float) extends UnitOfMeasure[UnitOfMass](unit,value) {
	def this () { this(Kilogram,0.0f) }
	def inKilograms : Float = value * unit.conversion
	def inKg = inKilograms

	def baseUnitOfMeasure = Kilogram
	def toBaseUnitOfMeasure = inKg
	def create(u: MeasurementUnit, v: Float) = new UnitOfMass(u,v)
	def order : Int = 1

	def resolve() = this
	def baseValue() = this
}
class UnitOfTime(unit : MeasurementUnit,value: Float) extends UnitOfMeasure[UnitOfTime](unit,value) {
	def this () { this(Second,0.0f) }
	def * (u : UnitOfTime) = new UnitOfTimeSquared(baseUnitOfMeasure,u.toBaseUnitOfMeasure * this.toBaseUnitOfMeasure)
	def inSeconds : Float = value * unit.conversion
	def inNanoseconds : Float = inSeconds * 1000000000.0f
	def inMilliseconds : Float = inSeconds * 1000.0f
	def inMinutes : Float = inSeconds / Minute.conversion


	def baseUnitOfMeasure = Second
	def toBaseUnitOfMeasure = inSeconds
	def in(newUnit:MeasurementUnit) = this.inSeconds / newUnit.conversion
	def create(u: MeasurementUnit, v: Float) = new UnitOfTime(u,v)
	def order : Int = 1

	def resolve() = this
	def baseValue() = this
}
class UnitOfTimeSquared(unit : MeasurementUnit,value: Float) extends UnitOfMeasure[UnitOfTimeSquared](unit,value) {
	def inSecondsSquared = value * unit.conversion * unit.conversion
	def baseUnitOfMeasure = Second
	def toBaseUnitOfMeasure = inSecondsSquared
	def create(u: MeasurementUnit, v: Float) = new UnitOfTimeSquared(u,v)
	def order = 2

	def resolve() = this
	def baseValue() = this
}
class UnitOfAcceleration ( dist : UnitOfDistance , time : UnitOfTimeSquared ) extends RatioUnitOfMeasure[UnitOfDistance,UnitOfTimeSquared](dist,time) {
	def * ( t : UnitOfTime ) = new UnitOfSpeed(dist,new UnitOfTime(Second,underValue.inSecondsSquared / (if(t.inSeconds==0.0f){0.00000001f}else{t.inSeconds})))
	def inMetersPerSecondSquared = toBaseUnitOfMeasure

	override def create(o: UnitOfDistance, u: UnitOfTimeSquared) = new UnitOfAcceleration(o,u).asInstanceOf[this.type]

	def resolve() = this
	def baseValue() = this
}

class Dimensions2(val x:UnitOfDistance,val y:UnitOfDistance) extends Serializable {
	def asVec = Vec2f(x.inMeters.toFloat,y.inMeters.toFloat)
	def x ( uod : UnitOfDistance ) : Dimensions = new Dimensions(x,y,uod)

	def area : UnitOfArea = x * y

	def + (t:UnitOfDistance) : Dimensions2 = new Dimensions2(x + t,y + t)
	def * (f:Float) : Dimensions2 = new Dimensions2(x * f,y * f)
	override def equals(p1: Any) = p1 match {
		case d : Dimensions2 => d.x == x && d.y == y
		case _ => false
	}

	def resolve() = this
	def baseValue() = this
}
@SerialVersionUID(1L)
class Dimensions(val x:UnitOfDistance,val y:UnitOfDistance,val z:UnitOfDistance) extends Serializable {
	def clampMin(m: UnitOfDistance) = new Dimensions( if ( x < m ) { m } else { x } , if ( y < m ) { m } else { y } , if ( z < m ) { m } else { z } )

	def asVec = Vec3f(x.inMeters.toFloat,y.inMeters.toFloat,z.inMeters.toFloat)
	def volume = new UnitOfVolume(Meter,x.inMeters*y.inMeters*z.inMeters)
	def surfaceArea = new UnitOfArea(Meter,x.inMeters*y.inMeters*2.0f + y.inMeters*z.inMeters*2.0f + x.inMeters*z.inMeters*2.0f)
	def inMeters = Vec3f(x.inMeters.toFloat,y.inMeters.toFloat,z.inMeters.toFloat)

	def - (t:UnitOfDistance) : Dimensions = new Dimensions(x - t,y - t,z - t)
	def + (t:UnitOfDistance) : Dimensions = new Dimensions(x + t,y + t,z + t)
	def * (t:Float) : Dimensions = new Dimensions(x * t,y * t,z * t)
	def * (t:Vec3f) : Dimensions = new Dimensions(x * t.x,y * t.y,z * t.z)
	def min : UnitOfDistance = scala.math.min( scala.math.min(x.inMeters,y.inMeters) , z.inMeters ).meters
	def max : UnitOfDistance = scala.math.max( scala.math.max(x.inMeters,y.inMeters) , z.inMeters ).meters
	def minArea : UnitOfArea = if ( x <= z && y <= z ) { x * y } else if ( x <= y && z <= y ) { x * z } else if ( y <= x && z <= x ) { y * z } else { throw new IllegalStateException }
	def maxArea : UnitOfArea = if ( x >= z && y >= z ) { x * y } else if ( x >= y && z >= y ) { x * z } else if ( y >= x && z >= x ) { y * z } else { throw new IllegalStateException }
	def scaleByVolume( f : Float ) = this * powf(f,0.33333f)
	override def toString = x + " x " + y + " x " + z
	override def equals(p1: Any) = p1 match {
		case d : Dimensions => d.x == x && d.y == y && d.z == z
		case _ => false
	}

	def resolve() = this
	def baseValue() = this

	def radius = {
		val xm = x.inMeters * 0.5f
		val ym = y.inMeters * 0.5f
		val zm = z.inMeters * 0.5f
		val euclid = xm*xm + ym*ym + zm*zm
		if ( euclid != 0 ) {
			sqrtf(euclid).meters
		} else {
			zeroMeters
		}
	}
}
object Dimensions {
	val Zero = new Dimensions(zeroMeters,zeroMeters,zeroMeters)
}
class UnitlessDimensions2(x:Float,y:Float) extends Serializable{
	def x ( f : Float ) : UnitlessDimensions3 = new UnitlessDimensions3(x,y,f)
	def x ( uod : UnitOfDistance ) : Dimensions = {
		new Dimensions( uod.copyWithValue(x) , uod.copyWithValue(y) , uod )
	}
	def meters = new Dimensions2(x.meters,y.meters)
	def centimeters = new Dimensions2(x.centimeters,y.centimeters)
}
class UnitlessDimensions3(x:Float,y:Float,z:Float) extends Serializable {
	def meters : Dimensions = new Dimensions(x.meters,y.meters,z.meters)
	def meter = meters
	def centimeters : Dimensions = new Dimensions(x.centimeters,y.centimeters,z.centimeters)
	def in ( uom : UnitOfDistance ) : Dimensions = new Dimensions ( uom.copyWithValue(x) , uom.copyWithValue(y) , uom.copyWithValue(z) )
}

case class Velocity ( var x : UnitOfSpeed , var y : UnitOfSpeed , var z : UnitOfSpeed ) {
	def inMetersPerSecond = Vec3f(x.inMetersPerSecond,y.inMetersPerSecond,z.inMetersPerSecond)
	def inMetersPerSecond(ret : Vec3f) = { ret.x = x.inMetersPerSecond; ret.y = y.inMetersPerSecond; ret.z = z.inMetersPerSecond; ret }
	def toBaseUnitOfMeasure = Vec3f(x.toBaseUnitOfMeasure,y.toBaseUnitOfMeasure,z.toBaseUnitOfMeasure)
	def - ( t : Velocity ) : Velocity = Velocity(x - t.x,y - t.y,z - t.z)
	def + ( t : Velocity ) : Velocity = Velocity(x + t.x,y + t.y,z + t.z)
	def * ( t : UnitOfTime ) : Dimensions = new Dimensions(x * t,y * t,z * t)
	def / ( t : UnitOfTime ) : Acceleration = Acceleration(x / t,y / t,z / t)

	def toVec3f = Vec3f(x.toBaseUnitOfMeasure,y.toBaseUnitOfMeasure,z.toBaseUnitOfMeasure)
	def getOrElse ( v : Velocity ) : Velocity = Velocity(x.getOrElse(v.x),y.getOrElse(v.y),z.getOrElse(v.z))

	def apply ( i : Int ) = if ( i == 0 ) { x } else if ( i == 1 ) { y } else if ( i == 2 ) { z } else { throw new IllegalArgumentException("Invalid axis index in velocity object") }
	def update ( i : Int , s :UnitOfSpeed ) {
		if ( i == 0 ) { x = s }
		else if ( i == 1 ) { y = s }
		else if ( i == 2 ) { z = s }
		else { throw new IllegalArgumentException("Invalid axis index in velocity object") }
	}

	def == ( v : Velocity ) = v.x == x && v.y == y && v.z == z
	def != ( v : Velocity ) = v.x != x || v.y != y || v.z != z

	def isNaN = x.overValue.value != x.overValue.value || y.overValue.value != y.overValue.value || z.overValue.value != z.overValue.value ||
					x.underValue.value != x.underValue.value || y.underValue.value != y.underValue.value || z.underValue.value != z.underValue.value

	def resolve() = this
	def baseValue() = this
}

//	case class Velocity2 ( var x : UnitOfSpeed , var y : UnitOfSpeed ) {
//
//	}

object NoVelocity extends Velocity(NoSpeed,NoSpeed,NoSpeed) {
	override def getOrElse ( v : Velocity ) = v
}

case class Acceleration ( var x : UnitOfAcceleration , var y : UnitOfAcceleration , var z : UnitOfAcceleration ) {
	def inMetersPerSecondSquared = Vec3f(x.inMetersPerSecondSquared,y.inMetersPerSecondSquared,z.inMetersPerSecondSquared)
	def inMetersPerSecondSquared (ret : Vec3f) = { ret.x = x.inMetersPerSecondSquared; ret.y = y.inMetersPerSecondSquared; ret.z = z.inMetersPerSecondSquared; ret }
	def - ( t : Acceleration ) : Acceleration = Acceleration(x - t.x,y - t.y,z - t.z)
	def + ( t : Acceleration ) : Acceleration = Acceleration(x + t.x,y + t.y,z + t.z)
	def * ( t : UnitOfTime ) : Velocity = new Velocity(x * t,y * t,z * t)
	def * ( t : UnitOfTimeSquared ) : Dimensions = new Dimensions(x * t,y * t,z * t)
	def * ( f : Float ) : Acceleration = new Acceleration(x * f,y * f,z * f)

	def apply ( i : Int ) = if ( i == 0 ) { x } else if ( i == 1 ) { y } else if ( i == 2 ) { z } else { throw new IllegalArgumentException("Invalid axis index in velocity object") }
	def update ( i : Int , s : UnitOfAcceleration ) {
		if ( i == 0 ) { x = s }
		else if ( i == 1 ) { y = s }
		else if ( i == 2 ) { z = s }
		else { throw new IllegalArgumentException("Invalid axis index in velocity object") }
	}

	def == ( v : Acceleration ) = v.x == x && v.y == y && v.z == z
	def != ( v : Acceleration ) = v.x != x || v.y != y || v.z != z

	def isNaN = x.overValue.value != x.overValue.value || y.overValue.value != y.overValue.value || z.overValue.value != z.overValue.value ||
					x.underValue.value != x.underValue.value || y.underValue.value != y.underValue.value || z.underValue.value != z.underValue.value
}
//class UnitOfDensity(mass:UnitOfMass,volume:UnitOfVolume) extends RatioUnitOfMeasure[UnitOfMass,UnitOfVolume](mass,volume) {}


class UnitOfTemperature(degrees:Float) extends UnitOfMeasure[UnitOfTemperature](Kelvin,degrees) {
	def this () { this(0.0f) }
	def inCentigrade = this.toBaseUnitOfMeasure - 273.0f
	def inKelvin = this.toBaseUnitOfMeasure
	def baseUnitOfMeasure = Kelvin
	def toBaseUnitOfMeasure = degrees * unit.conversion
	def create(unit: MeasurementUnit, v: Float) = new UnitOfTemperature(v)
	def order = 1
}