package arx.core

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/4/12
 * Time: 11:01 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import java.util.concurrent.atomic.AtomicBoolean

@SerialVersionUID(1L)
trait Moddable[+T] extends Serializable {
	def resolve () : T
	def dynamic : Boolean = true
	def baseValue() : T

	override def equals(p1: Any) = p1 match {
		case null => false
		case m : Moddable[T] => m.resolve() == this.resolve()
		case o => this.resolve() == o
	}

	def toFunction = new FunctionFromModdable(this)
}

@Deprecated
trait SelfModdable[+T] extends Moddable[T] {
	override def equals(p1: Any) = p1 match {
		case null => false
		case s : SelfModdable[T] => super.equals(p1)
		case m : Moddable[T] => this.equals(m.resolve())
		case o => this.resolve() == o
	}
}

class FunctionFromModdable[+T](moddable : Moddable[T]) extends Function0[T] {
	def apply() = moddable.resolve()
}

class ModdedValue[T](val value: T) extends Moddable[T]{
	override def dynamic : Boolean = false
	override def resolve () : T = value
	override def baseValue() = value
	override def toString: String = { resolve().toString }
	override def equals(p1: Any) = p1 match {
		case null => false
		case m : Moddable[T] => m.resolve() == this.resolve()
		case o => this.resolve() == o
	}
}

abstract class WrappingModdable[T](var modded: Moddable[T]) extends Moddable[T] {
	override def toString: String = { resolve().toString }
}

class ModdedInOutFunction[T](m: Moddable[T],val func: T => T) extends WrappingModdable[T](m){
	var active : Boolean = true
	@transient var resolving : Boolean = false

	override def resolve () : T = {
		synchronized {
			if ( ! resolving ) {
				resolving = true
				val ret = if( active ) {
					val input = modded.resolve()
					val output = func(input)
					posit( ! input.isInstanceOf[AnyRef] || ( input != output ) , "modifier function had the same input as output, possible violation of immutability" )
					output
				} else {
					modded.resolve()
				}
				resolving = false
				ret
			} else {
				modded.resolve()
			}
		}
	}

	def baseValue() = m.baseValue()
}

class ModdedSimpleClosure[T,U](f : (U) => T,u : Moddable[U]) extends Moddable[T] {
	def resolve() = f(u.resolve())
	def baseValue() = f(u.resolve())
}

trait Modifier[T] extends WrappingModdable[T]
class FunctionModifier[T]( modded : Moddable[T] , f : T => T ) extends ModdedInOutFunction[T](modded,f) with Modifier[T] {

}

class ModdedOutFunction[T](val func: () => T) extends Moddable[T]{
//	@transient var resolving  = false

	override def resolve () : T = {
//		if ( ! resolving ) {
//			resolving = true
			val ret = func()
//			resolving = false
			ret
//		} else {
//			throw new IllegalStateException("Infinite loop detected in moddedOutFunction()")
//		}
	}

	def baseValue() = func()
}

//class ModdedByNameFunction[T](func: => T) extends Moddable[T]{
//	override def resolve () : T = {
//		func
//	}
//}

class DamageModifier(modded : Moddable[Float] , val damage : Float ) extends WrappingModdable[Float](modded) with Modifier[Float] {
	def resolve() : Float = {
		modded.resolve() - damage
	}
	def baseValue() = modded.baseValue()

	def copyWithNewDamage(d : Float) = new DamageModifier(modded,d)
}
object DamageModifier {
	def apply(modded : Moddable[Float] , damage : Float ) : DamageModifier = {
		modded match {
			case dm : DamageModifier => dm.copyWithNewDamage(dm.damage + damage)
			case _ => new DamageModifier(modded,damage)
		}
	}
}

class ForwardingModdable[T]( moddable : () => Moddable[T] ) extends Moddable[T] {
	def resolve() = moddable().resolve()
	def baseValue() = moddable().resolve()
}

//object Moddable{
//	def removeAllReferencesTo[T](to: T, from: Moddable[List[T]]) : Moddable[List[T]] = {
//		from match {
//			case wm : WrappingModdable[List[T]] =>
//				wm.modded = removeAllReferencesTo(to,wm.modded)
//				wm
//			case vm : ModdedValue[List[T]] => Moddable( vm.value filterNot ( _ == to ) )
//			case o : Moddable[List[T]] => o
//		}
//	}
//
//	def apply[T] ( value: T ): Moddable[T] = { new ModdedValue[T](value) }
//	def apply[T] ( func: () => T ): Moddable[T] = { new ModdedOutFunction[T](func) }
//
//	implicit def Moddable2T[T] ( moddable: Moddable[T] ): T = {
//		moddable.resolve()
//	}
//	implicit def T2Moddable[T] ( t: T ) : Moddable[T] = {
//		Moddable(t)
//	}
//
//	def removeDamageModifiers ( m : Moddable[Float] ) : Moddable[Float] = {
//		m match {
//			case dm : DamageModifier => removeDamageModifiers(dm.modded)
//			case wm : WrappingModdable[Float] =>
//				wm.modded = removeDamageModifiers(wm.modded)
//				wm
//			case om : Moddable[Float] => om
//		}
//	}
//
//	/** Should replace the closest to top hard value with itself plus 'f' */
//	def addToValue ( m : Moddable[Float], f : Float ) : Moddable[Float] = {
//		m match {
//			case wm : WrappingModdable[Float] =>
//				wm.modded = addToValue(wm,f)
//				wm
//			case mv : ModdedValue[Float] =>
//				Moddable(mv.value + f)
//			case o : Moddable[Float] => o
//		}
//	}
//}