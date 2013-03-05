package arx.core.units

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 3:09 PM
 * Created by nonvirtualthunk
 */

abstract class MeasurementUnit extends Serializable{
	val name : String
	val suffix : String
	var conversion : Float

	def lowerOrderOfMagnitude : Option[MeasurementUnit] = None
	def higherOrderOfMagnitude : Option[MeasurementUnit] = None

	override def toString = "MeasurementUnit(" + name + " suffix=\"" + suffix + "\" conversion = " + conversion + ")"

	def allHigherOrdersOfMagnitude : List[MeasurementUnit] = higherOrderOfMagnitude match { case Some ( m ) => m :: m.allHigherOrdersOfMagnitude ; case None => Nil }
	def allLowerOrdersOfMagnitude : List[MeasurementUnit] = lowerOrderOfMagnitude match { case Some(m) => m :: m.allLowerOrdersOfMagnitude ; case None => Nil }
	def allOrdersOfMagnitude = allLowerOrdersOfMagnitude.reverse ::: this :: allHigherOrdersOfMagnitude
}
object Meter extends MeasurementUnit {
	val name = "meter"
	val suffix = "m"
	var conversion = (1.0).toFloat

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Decimeter)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Kilometer)
}
object Decimeter extends MeasurementUnit {
	val name = "decimeter"
	val suffix = "dm"
	var conversion = (0.1).toFloat

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Centimeter)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Meter)
}
object Centimeter extends MeasurementUnit {
	val name = "centimeter"
	val suffix = "cm"
	var conversion = (0.01).toFloat

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Millimeter)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Decimeter)
}
object Millimeter extends MeasurementUnit {
	val name = "millimeter"
	val suffix = "mm"
	var conversion = (0.001).toFloat

	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Centimeter)
}
object Kilometer extends MeasurementUnit {
	val name = "kilometer"
	val suffix = "km"
	var conversion = (1000.0).toFloat

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Meter)
}
object Kilogram extends MeasurementUnit {
	val name = "killogram"
	val suffix = "kg"
	var conversion = (1.0).toFloat

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Gram)
}
object Gram extends MeasurementUnit {
	val name = "gram"
	val suffix = "g"
	var conversion = (0.001).toFloat

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Milligram)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Kilogram)
}
object Milligram extends MeasurementUnit {
	val name = "milligram"
	val suffix = "mg"
	var conversion = (Gram.conversion * 0.001).toFloat

	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Gram)
}
object Kelvin extends MeasurementUnit {
	val name = "kelvin"
	val suffix = "k"
	var conversion = (1.0f).toFloat
}
object Second extends MeasurementUnit {
	val name = "second"
	val suffix = "s"
	var conversion = (1.0f).toFloat

	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Minute)
}
object Minute extends MeasurementUnit {
	val name = "minute"
	val suffix = "min"
	var conversion = (60.0f).toFloat

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Second)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Hour)
}
object Hour extends MeasurementUnit {
	val name = "hour"
	val suffix = "hour"
	var conversion = (Minute.conversion * 60.0).toFloat

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Minute)
	override def higherOrderOfMagnitude : Option[MeasurementUnit] = Some(Day)
}
object Day extends MeasurementUnit {
	val name = "day"
	val suffix = "day"
	var conversion = (Hour.conversion * 24.0).toFloat

	override def lowerOrderOfMagnitude : Option[MeasurementUnit] = Some(Hour)
}
object Year extends MeasurementUnit {
	val name = "year"
	val suffix = "year"
	var conversion = (Day.conversion * 365.0).toFloat
}