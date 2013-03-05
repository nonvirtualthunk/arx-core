package arx.core.rich

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 3:15 PM
 * Created by nonvirtualthunk
 */

class EitherFloat ( val a : Float , val b : Float ) {
	def aeq ( p : EitherFloat , eps : Float ) : Boolean = { math.abs(p.a - a) < eps || math.abs(p.b - b) < eps }
	def =~= ( p : EitherFloat ) : Boolean = this.aeq(p,0.00001f)
	def aeq ( f : Float , eps : Float ) : Boolean = { math.abs(a - f) < eps || math.abs(b - f) < eps }
	def =~= ( f : Float ) : Boolean = aeq(f,0.00001f)
	def + ( p : Float ) : EitherFloat = new EitherFloat(a + p,b + p)
	def - ( p : Float ) : EitherFloat = new EitherFloat(a - p,b - p)
	def * ( p : Float ) : EitherFloat = new EitherFloat(a * p,b * p)
}