package arx.core.rich

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 3:17 PM
 * Created by nonvirtualthunk
 */

class EitherInt ( val a : Int , val b : Int ) {
	def + ( p : Int ) : EitherInt = new EitherInt(a + p,b + p)
	def - ( p : Int ) : EitherInt = new EitherInt(a - p,b - p)
	def * ( p : Int ) : EitherInt = new EitherInt(a * p,b * p)
}