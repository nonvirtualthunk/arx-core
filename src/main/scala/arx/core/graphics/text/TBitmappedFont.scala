package arx.core.graphics.text

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/4/12
 * Time: 12:57 PM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import arx.core.vec.{ReadVec2f, Vec2f}

trait TBitmappedFont {
	def characterTexCoords(c: Char): Array[ReadVec2f]
	def characterWidth(c : Char): Float
	def characterHeight(c : Char): Float

	def bind ( i : Int)

	def maxCharacterDimensions : ReadVec2f
}