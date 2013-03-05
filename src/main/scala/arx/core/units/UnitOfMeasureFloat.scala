package arx.core.units

import arx.core.ArxImplicits

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 3:11 PM
 * Created by nonvirtualthunk
 */


class UnitOfMeasureFloat(f : Float) {
	def meters = new UnitOfDistance(Meter,f)
	def meter = this.meters
	def centimeters = new UnitOfDistance(Centimeter,f)
	def centimeter = this.centimeters
	def cm = this.centimeters
	def millimeters = new UnitOfDistance(Millimeter,f)
	def squareMeters = new UnitOfArea(Meter,f)
	def squareMeter = this.squareMeters
	def squareCentimeters = new UnitOfArea(Centimeter,f)
	def meters2 = this.squareMeters
	def centimeters2 = squareCentimeters
	def cubicMeters = new UnitOfVolume(Meter,f)
	def cubicCentimeters = new UnitOfVolume(Centimeter,f)
	def meters3 = this.cubicMeters
	def meter3 = this.cubicMeters
	def m3 = this.meter3
	def centimeters3 = cubicCentimeters
	def feet = new UnitOfDistance(Meter,f * 0.3048f)
	def inches = new UnitOfDistance(Centimeter,f * 2.54f)
	def km = new UnitOfDistance(Kilometer,f)
	def kilometers = this.km
	def kilometer = this.km

	def seconds = new UnitOfTime(Second,f)
	def second = this.seconds
	def minute = new UnitOfTime(Minute,f)
	def minutes = this.minute
	def hour = new UnitOfTime(Hour,f)
	def hours = this.hour
	def day = new UnitOfTime(Day,f)
	def days = this.day
	def year = new UnitOfTime(Year,f)
	def years = this.year

	def second2 = new UnitOfTimeSquared(Second,f)

	def kg = new UnitOfMass(Kilogram,f)
	def kilograms = this.kg
	def kilogram = this.kg
	def pounds = new UnitOfMass(Kilogram,f * 0.45359f) //auto-convert to metric
	def grams = new UnitOfMass(Gram,f)
	def gram = this.grams
	def mg = new UnitOfMass(Milligram,f)
	def milligram = this.mg
	def milligrams = this.mg

	def kelvin = new UnitOfTemperature(f)
	def degreesKelvin = this.kelvin
	def degreesCelsius = new UnitOfTemperature(f + 273.15f)
	def degreesCentigrade = this.degreesCelsius
	def degreesFahrenheit = new UnitOfTemperature((f + 459.67f)*0.5555555f)
	def centigrade = this.degreesCentigrade

	def m_s = new UnitOfSpeed( new UnitOfDistance(Meter,f) , ArxImplicits.second )
	def m_s2 = new UnitOfAcceleration( new UnitOfDistance(Meter,f) , ArxImplicits.second2 )
	def km_h = new UnitOfSpeed( new UnitOfDistance(Meter,1) , new UnitOfTime(Hour,1) )

	def kg_m3 = new RatioUnitOfMeasure[UnitOfMass,UnitOfVolume]( new UnitOfMass(Kilogram,f) , ArxImplicits.m3 )
	def g_cm3 = new UnitOfDensity(new UnitOfMass(Gram,f) , ArxImplicits.cm3)

	def x ( o : Float ) : UnitlessDimensions2 = new UnitlessDimensions2(f,o)
}