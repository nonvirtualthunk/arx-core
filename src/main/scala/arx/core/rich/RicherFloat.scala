package arx.core.rich

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 3:14 PM
 * Created by nonvirtualthunk
 */


class RicherFloat ( val f : Float ) {
	def =~= ( a : Float ) : Boolean = aeq(a,0.00001f)
	def aeq ( a : Float , eps : Float ) : Boolean = { scala.math.abs(a - f) < eps }
	def +- ( a : Float ) : EitherFloat = { new EitherFloat(f + a,f - a) }
	def -+ ( a : Float ) : EitherFloat = { new EitherFloat(f - a,f + a) }

	def clamp ( minimum : Float , maximum : Float ) : Float = { scala.math.min(maximum,scala.math.max(f,minimum)) }
	def clampFloor ( minimum : Float ) : Float = scala.math.max(f,minimum)
	def clampCeil ( maximum : Float ) : Float = scala.math.min(f,maximum)
}