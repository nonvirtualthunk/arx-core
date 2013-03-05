package arx.core.units

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 3:12 PM
 * Created by nonvirtualthunk
 */


class RatioUnitOfMeasure[Over <: UnitOfMeasure[Over],Under <: UnitOfMeasure[Under]](overValueArg : Over,underValueArg: Under) extends Serializable {
	val underValue = if ( underValueArg.value == 0.0f ) {
		println("Creating invalid under-value in ratio unit of measure");
		underValueArg.create(underValueArg.unit,1.0f)
	} else {
		underValueArg
	}
	val overValue = if ( underValueArg.value == 0.0f ) { overValueArg.create(overValueArg.unit,0.0f) } else { overValueArg }

	def * ( u : Under ) : Over = {
		val underBase = underValue.toBaseUnitOfMeasure
		val r = if ( underBase == 0.0f ) { 0.0f } else { u.toBaseUnitOfMeasure / underBase }
		overValue * r
	}

	def > ( r : RatioUnitOfMeasure[Over,Under] ) = this.toBaseUnitOfMeasure > r.toBaseUnitOfMeasure
	def < ( r : RatioUnitOfMeasure[Over,Under] ) = this.toBaseUnitOfMeasure < r.toBaseUnitOfMeasure
	def * ( f : Float ) : this.type = create(overValue * f,underValue)
	def - ( r : RatioUnitOfMeasure[Over,Under] ) : this.type = {
		val thisO = this.overValue.toBaseUnitOfMeasure
		val thisU = this.underValue.toBaseUnitOfMeasure

		val thatO = r.overValue.toBaseUnitOfMeasure
		val thatU = r.underValue.toBaseUnitOfMeasure

		create( overValue.create(overValue.baseUnitOfMeasure,(thisO/thisU) - (thatO/thatU)) , underValue.create(underValue.baseUnitOfMeasure,1.0f) )
//			create(this.overValue - r.overValue,this.underValue - r.underValue)
	}
	def + ( r : RatioUnitOfMeasure[Over,Under] ) : this.type = {
		val thisO = this.overValue.toBaseUnitOfMeasure
		val thisU = this.underValue.toBaseUnitOfMeasure

		val thatO = r.overValue.toBaseUnitOfMeasure
		val thatU = r.underValue.toBaseUnitOfMeasure

		create( overValue.create(overValue.baseUnitOfMeasure,(thisO/thisU) + (thatO/thatU)) , underValue.create(underValue.baseUnitOfMeasure,1.0f) )
//			create(this.overValue + r.overValue,this.underValue + r.underValue)
	}
	def toBaseUnitOfMeasure = overValue.toBaseUnitOfMeasure / (if(underValue.toBaseUnitOfMeasure==0.0f){0.0000001f}else{underValue.toBaseUnitOfMeasure})
	override def equals ( other : Any ) : Boolean = {
		other match {
			case r : RatioUnitOfMeasure[Over,Under] =>
				val underBase = r.underValue.toBaseUnitOfMeasure
				val thisUnderBase = this.underValue.toBaseUnitOfMeasure
				r.overValue.baseUnitOfMeasure == this.overValue.baseUnitOfMeasure && r.overValue.order == this.overValue.order &&
				r.underValue.baseUnitOfMeasure == this.underValue.baseUnitOfMeasure && r.underValue.order == this.underValue.order &&
				scala.math.abs(	(if ( thisUnderBase == 0.0f ) { 0.0f } else { (this.overValue.toBaseUnitOfMeasure / thisUnderBase) }) -
								(if ( underBase == 0.0f ) { 0.0f } else { (r.overValue.toBaseUnitOfMeasure / (underBase)) })	) < 0.0001f
			case _ => false
		}
	}
	def create(o: Over, u: Under) : this.type = new RatioUnitOfMeasure(o,u).asInstanceOf[this.type]
	override def toString = {
		val normalized = if ( underValue == 0.0f ) { 0.0f } else { overValue.toFloat / underValue.toFloat}
		normalized + " " + overValue.unit.suffix + "/" + underValue.unit.suffix
	}

//		def writeExternal(p1: ObjectOutput) {p1.writeObject(overValue);p1.writeObject(underValue)}
//
//		def readExternal(p1: ObjectInput) {overValue = p1.readObject.asInstanceOf[Over];underValue = p1.readObject.asInstanceOf[Under]}
}