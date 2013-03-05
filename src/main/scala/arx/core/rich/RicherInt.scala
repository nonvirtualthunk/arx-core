package arx.core.rich

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 3:16 PM
 * Created by nonvirtualthunk
 */

class RicherInt ( val f : Int ) {
	def +- ( a : Int ) : EitherInt = { new EitherInt(f + a,f - a) }
	def -+ ( a : Int ) : EitherInt = { new EitherInt(f - a,f + a) }

	def clamp ( minimum : Int , maximum : Int ) : Int = { scala.math.min(maximum,scala.math.max(f,minimum)) }
	def isBitSet ( bitFlag : Int ) = (f & bitFlag) == bitFlag
	def anyBitsSet ( bitMask : Int ) = (f & bitMask) != 0
}