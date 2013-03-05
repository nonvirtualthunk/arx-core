package arx.core.datastructures

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 4:40 PM
 * Created by nonvirtualthunk
 */

@SerialVersionUID(1L)
class IntRange ( val lower : Int , val upper : Int ) extends Serializable {
	def contains ( f : Int ) = lower <= f && upper >= f
	def length = upper - lower
}