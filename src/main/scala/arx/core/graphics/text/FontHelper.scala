package arx.core.graphics.text

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 2/15/13
 * Time: 9:04 AM
 * Created by nonvirtualthunk
 */

import arx.Prelude._
import arx.application.Noto
import java.awt.{RenderingHints, AlphaComposite, Color, Font}
import java.awt.image.BufferedImage
import arx.graphics.{Rect, SubImageView, Image}

class FontHelper(font:Font) {
	def pixelSize = imgSize
	val bufferedImage = new BufferedImage((font.getSize*1.5).toInt,(font.getSize*1.5).toInt, BufferedImage.TYPE_INT_ARGB)
	val g = bufferedImage.createGraphics

	val backgroundColor: Color = new Color(1.0f,1.0f,1.0f,1.0f)
	val foregroundColor: Color = new Color(1.0f,1.0f,1.0f,1.0f)

	g.setFont(font)
	g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
	g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
	g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)

	val fm = g.getFontMetrics
	val ascent = fm.getMaxAscent
	val descent = fm.getMaxDescent

	val imgSize : Int = math.max(fm.charWidth('W')+3,fm.getMaxAscent+fm.getMaxDescent+fm.getLeading/2+3) //fm. //font.getSize * 1.5).toInt
	var retImage : Image = Image.withDimensions(imgSize,imgSize)

	def clearCanvas () {
		// Clear image with background color (make transparent if color has alpha value)
		g.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR,0.0f))
		g.setColor(backgroundColor)
		g.fillRect(0,0,imgSize,imgSize)
	}

	/**
	 * Draws a character to an Image and returns it. The image returned
	 * will be invalidated as soon as drawChar is next called
	 * @param ch the character to render
	 * @return an image representation of the character
	 */
	def drawChar ( ch : Char ) = {
		clearCanvas()
		// prepare to draw characters in foreground color
		g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1.0f))
		g.setColor(foregroundColor)

		g.drawString( String.valueOf(ch) , 0 , ascent )

		val cWidth = if ( ch == 'f' ) { fm.charWidth(ch) + 2 } else { fm.charWidth(ch) }
		val cHeight = fm.getHeight - 2
		val cX = 0
		val cY = 0

		val retSubImage = new SubImageView(retImage,Rect(0,0,cWidth,cHeight))
		val rast = bufferedImage.getData
		var x = 0 ; while ( x < cWidth ) {
			var y = 0 ; while ( y < cHeight ) {
				var q = 0; while ( q < 4 ) {
					val rastX = x + cX
					val rastY = math.max(y + cY,0)
					retSubImage(x,cHeight - y - 1,q) = rast.getSample(rastX,rastY,q).toByte
				q += 1}
			y += 1}
		x += 1}
		retSubImage
	}
}