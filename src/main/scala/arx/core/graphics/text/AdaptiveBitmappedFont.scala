package arx.core.graphics.text

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/15/13
 * Time: 8:49 AM
 * Created by nonvirtualthunk
 */

import java.io.InputStream
import arx.core.vec.{Vec2f, ReadVec2f}
import collection.mutable
import java.awt._
import org.lwjgl.opengl.GL11
import arx.core.graphics.data.TextureBlock
import arx.core.serialization.{ArxInputStream, ArxOutputStream, TVersionedExternalizable}
import arx.core.datastructures.IntRange
import arx.core.ArxImplicits
import ArxImplicits._

class AdaptiveBitmappedFont(fontStream:InputStream) extends TBitmappedFont with TVersionedExternalizable {
	def this() { this(null) }
	var font = if ( fontStream != null ) { Font.createFont(Font.TRUETYPE_FONT,fontStream).deriveFont(Font.PLAIN,60.0f) } else { null }
	var textureBlock = if ( fontStream != null ) { new TextureBlock(2048,2048) } else { null }

	val asciiRange : IntRange = 32 -> 126

	var asciiTexCoords = Array.ofDim[ReadVec2f](asciiRange.upper,4)
	var unicodeTexCoords = new mutable.HashMap[Char,Array[ReadVec2f]]

	var asciiCharacterWidths = Array.ofDim[Float](asciiRange.upper)
	var asciiCharacterHeights = Array.ofDim[Float](asciiRange.upper)
	var unicodeCharacterDimensions = new mutable.HashMap[Char,ReadVec2f]

	var _maxCharacterDimensions = Vec2f(0.0f,0.0f)

	var needsSolidification = false

	var fontHelper = if ( fontStream != null ) { new FontHelper(font) } else { null }

	if ( fontStream != null ) { init () }
	def init () {
		textureBlock.minFilter = GL11.GL_LINEAR_MIPMAP_LINEAR
		textureBlock.magFilter = GL11.GL_LINEAR
		println("Creating adaptive bitmapped font")

		var ch = 0
		while ( ch < asciiRange.upper ) {
			if ( ch >= asciiRange.lower ) {
				val img = fontHelper.drawChar(ch.toChar)

				val charWidth = img.width.toFloat / fontHelper.pixelSize
				val charHeight = img.height.toFloat / fontHelper.pixelSize

				val tc = textureBlock.getOrElseUpdate(img)
				asciiTexCoords(ch) = tc
				asciiCharacterWidths(ch) = charWidth
				asciiCharacterHeights(ch) = charHeight
				_maxCharacterDimensions.x = math.max(_maxCharacterDimensions.x,charWidth)
				_maxCharacterDimensions.y = math.max(_maxCharacterDimensions.y,charHeight)
			} else {
				asciiTexCoords(ch) = Array.fill(4)(Vec2f.Zero)
				asciiCharacterWidths(ch) = 0.001f
				asciiCharacterHeights(ch) = 0.001f
			}
			ch += 1
		}
	}


	def currentVersion = 1
	def writeExternalVersioned(out: ArxOutputStream) {
		out.write(font)
		out.write(textureBlock)
		out.write(asciiTexCoords)
		out.write(unicodeTexCoords)
		out.write(asciiCharacterWidths)
		out.write(asciiCharacterHeights)
		out.write(_maxCharacterDimensions)
	}
	def readExternalVersioned(version: Int, in: ArxInputStream) {
		font = in.read
		textureBlock = in.read
		asciiTexCoords = in.read
		unicodeTexCoords = in.read
		asciiCharacterWidths = in.read
		asciiCharacterHeights = in.read
		_maxCharacterDimensions = in.read
		fontHelper = new FontHelper(font)
	}

	def characterTexCoords(c: Char) : Array[ReadVec2f] = {
		if ( asciiRange.contains(c) ) {
			asciiTexCoords(c)
		} else {
			ensureUnicodeCharAdded(c)
			unicodeTexCoords(c)
		}
	}
	def characterWidth(c: Char) = {
		if ( asciiRange.contains(c) ) {
			asciiCharacterWidths(c)
		} else {
			ensureUnicodeCharAdded(c)
			unicodeCharacterDimensions(c).x
		}
	}
	def characterHeight(c: Char) = {
		if ( asciiRange.contains(c) ) {
			asciiCharacterHeights(c)
		} else {
			ensureUnicodeCharAdded(c)
			unicodeCharacterDimensions(c).y
		}
	}

	def bind(i: Int) {
		textureBlock.bind(i)
	}
	def maxCharacterDimensions = _maxCharacterDimensions

	def ensureUnicodeCharAdded( c : Char ) {
		if ( ! unicodeTexCoords.contains(c) ) {
			val img = fontHelper.drawChar(c)

			val charWidth = img.width.toFloat / fontHelper.pixelSize
			val charHeight = img.height.toFloat / fontHelper.pixelSize
			val tc = textureBlock.getOrElseUpdate(img)

			_maxCharacterDimensions.x = math.max(_maxCharacterDimensions.x,charWidth)
			_maxCharacterDimensions.y = math.max(_maxCharacterDimensions.y,charHeight)

			unicodeCharacterDimensions(c) = Vec2f(charWidth,charHeight)
			unicodeTexCoords(c) = tc
		}
	}
}